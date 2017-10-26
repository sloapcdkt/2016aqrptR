
##############AQS file loader
##
## This function reads data exported from AQS into R. Currently, the only accepted input 
## formats are workfiles created from the AMP501 form ("Extract Raw Data Report") or AMP350MX
## form ("Max Values Report"). The file to be imported must be in the working directory.
##
## USAGE
## load.aqs(file, format="AMP501")
##
## ARGUMENTS
## file   - The name of the file to be imported. Must be in quotes.
## format - A text string designating the format of the AQS file. The default is "AMP501". "AMP350MX" is
##          also accepted. Any other string yeilds an error.
## tz - Optional timezone; defualts to UTC which leaves times unchanged. For Pacific Standard Time
##      use 'tz = "Etc/GMT+8"'    
##
## DETAILS
## load.aqs reads in an AQS file and converst it to a dataframe. It detects the number of unique AQS
## site code/pollutant/POC/sample duration combinations, and checks whether multiple unit codes and/or
## method codes exist for any of these combinations. If it detects multiple units and/or methods, a 
## a warning and summary of the findings is displayed, but a dataframe is still produced. A summary
## of the data in AQS export is also displayed. Finally, the dataframe is produced. Each unique 
## site code/pollutant/POC/sample duration combination gets its own column. Each date gets its own row.
## (Dates are stored as POSIXct objects.) Methods, units, MDLs, Null Codes, information codes and all
## other information in the AQS is discarded. Missing data will be coded as NA.
##
## VALUE
## A dataframe.



load.aqs<-function(file, format="AMP501", tz="UTC" ){
  if(format!="AMP501" & format!="AMP350MX") {
    stop("Specified file format not recognized. Must by AMP501 or AMP350MX")
  }
  if(format=="AMP501"){
    temp<-read.table(file, sep="|", )
    temp$monitor<-paste(temp[,3], temp[,4],temp[,5],temp[,6], temp[,7], temp[,8], sep="-")
    temp<-temp[,c(29, 9:13)]
    
  } else {
    ind<-readLines(file)
    tot<-length(ind)
    ind<-grep("^[^2]", ind)[-(1:4)]
    temp<-read.table(file, header=F, sep="|", skip=ind[1], nrows=ind[2]-ind[1]-1)
    for (i in 2:length(ind)){
      if(ind[i]==tot) break
      temp2<-read.table(file, header=F, sep="|", skip=ind[i], nrows=ind[i+1]-ind[i]-1)
      temp<-rbind(temp,temp2)
    }
    
    temp$monitor<-paste(temp[,2], temp[,3],temp[,4],temp[,5], temp[,6], temp[,7], sep="-")
    temp<-temp[,c(66,9,8,17,18)]
  }
  
  no.monitors<-unique(temp$monitor)
  
  check<-data.frame(Monitor=no.monitors, 
                    Units=rep(NA,length(no.monitors)),
                    Methods=rep(NA,length(no.monitors)))
  
  for(i in 1:length(no.monitors)){
    check$Units[i]<-length(unique(temp[which(temp$monitor==no.monitors[i]),2]))
    check$Methods[i]<-length(unique(temp[which(temp$monitor==no.monitors[i]),3]))
  }
  
  if(mean(check$Units)!=1 | mean(check$Methods)!=1) {
    message("Warning: Multiple units and/or methods detected for the same monitor.\nTable below reports the number of different codes per monitor:")
    print(check)
  } 
  for(i in 1:length(no.monitors)){
    check$Units[i]<-unique(temp[which(temp$monitor==no.monitors[i]),2])[1]
    check$Methods[i]<-unique(temp[which(temp$monitor==no.monitors[i]),3])[1]
  }
  message("Summary of monitor information: \nIf there are multiple codes per monitor, only the first is displayed.")
  print(check)
  
  if(format=="AMP501"){
    temp$date<-paste(temp$V11, temp$V12)
    temp$date<-as.POSIXct(strptime(temp$date, "%Y%m%d %H:%M"), tz=tz)
    temp<-temp[,c(7,1,6)]
  } else {
    temp$date<-as.POSIXct(strptime(temp$V17, "%Y%m%d"), tz=tz)
    temp<-temp[,c(6,1,5)]
    names(temp)[3]<-"V13"
  }
  
  
  require(reshape2)
  temp<-dcast(temp, date~monitor, value.var="V13")
  
  return(temp)
}