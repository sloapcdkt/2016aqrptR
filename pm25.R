###########################################################################
### Calculate total and by site 24-hr PM2.5 exceedences (state and fed) ###
### Generate monthly summary plots                                      ###
###########################################################################

#################################
## LOAD 24hr PM25 data from AQS##
#################################

# In AQS, run AMP350MX selecting 24 hour PM2.5 (88101) for the whole county
# This ensures 40 CFR rules for data completeness and averaging and 
# rounding are applied. Resulting file is called "24hourPM25.txt".

source('AQSloader.R')
pm25<-load.aqs("24hourPM25.txt", format="AMP350MX") 
names(pm25)<-c("date", "mesa", "slo", "cdf", "atas")


################################
## Count up PM2.5 exceedences ##
################################

#fed 24-hr standard

apply(pm25[,-1], 2, function(x) sum(x>35.5, na.rm=T))
apply(pm25[,-1], 2, function(x) sum(x>35.0, na.rm=T))

#fed 24-hr, countywide
sum(apply(pm25[,-1], 1, function(x) sum(x>35.5, na.rm=T)>0))


###################################################################
### figure 7    New Style                                       ###
###################################################################

f2 <- function(site, main){
  
  ## padding between months
  p <- 0.25
  
  ## set-up empty plot window
  plot(as.numeric(format(pm25$date, "%m")), pm25[,site], type="n",
       xlim=c(0, 12+p), ylim=c(0, 40),
       ylab="", xlab="",
       xaxt="n", yaxt="n", bty="n")
  
  ## site label
  text(12.70, -5, main, font=2, pos=2, xpd = TRUE, cex = 1.2)
  
  ## black lines for <35, red for exceedences (>35)
  segments(as.numeric(format(pm25$date, "%m"))-p,
           pm25[,site],
           as.numeric(format(pm25$date, "%m"))+p,
           col = ifelse(pm25[,site] < 35, ifelse(pm25[,site] < 0, 0, 1), 2))
  
  ## median line
  lines(c(1:12 - 2*p, 12+2*p),
        tapply(pm25[,site], format(pm25$date, "%m"), function(a) median(a, na.rm=T))[c(1:12,12)],
        type = "s",
        lwd = 2, lend = 1)
  
  ## month labels on x-axis
  axis(1, at=1:12, labels=unique(format(pm25$date, "%b")), 
       las=1, tick=FALSE, line = -1.5, cex.axis = 0.7)
  
  ## y-axis labels: min, max and state standard, if max > 50
  at <- range(pm25[,site], na.rm=TRUE)
  if(max(at) > 35) at <- c(at, 35)
  if(min(at) < 0) at <- c(at, 0)
  #axis(2, at = at, las = 1, line = -1, cex.axis = 0.8)
  
  ## y-axis labels: 0, max and state standard, if max > 50
  at <- round(max(pm25[,site], na.rm=TRUE))
  ticks <- seq(0, 40, by = 10)
  # if(max(at) > 50) at <- c(at, 50)
  at <- c(at, ticks[which(ticks < max(at))])
  axis(2, at = at, las = 1, line = -1, cex.axis = 0.8)
  
  ## day-of-month labels for exceedences
  text(as.numeric(format(pm25$date, "%m"))+1.9*p,
       ifelse(pm25[,site] < 35, NA, pm10[,site]+0.3),
       as.numeric(format(pm25$date, "%d")),
       col=2,
       cex=0.7)
}


svg("fig5.svg", width = 8, height = 8, 
    pointsize = 10)

par(mfrow = c(2, 2),
    mar = c(4, 2, 0, 0))

f2("atas", "Atascadero")
f2("slo", "San Luis Obispo")
f2("cdf", "CDF")
f2("mesa", "Mesa2")

dev.off()

###################################################################
### Clean up                                                    ###
###################################################################

rm(pm25, f2, load.aqs)
