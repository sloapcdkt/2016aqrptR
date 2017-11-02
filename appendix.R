##############
## Appendix ##
##############

#######################################
## define consistent color palatte   ##
#######################################
colors <- RColorBrewer::brewer.pal(8, "Set1")[-6]
colors <- c(colors, colors[c(2,6,7)])
sites <- c( "San Luis Obispo", "Morro Bay", "NRP",
            "Paso Robles", "Atascadero",  "Red Hills", "Carrizo Plains",
            "CDF", "Mesa2", "Oso Flaco")
colors <- data.frame(sites=sites, colors=colors, stringsAsFactors = FALSE)
rownames(colors) <- c("slo", "morro", "nrp", "paso", "atas", "red", "carrizo", "cdf", "mesa", "oso")
rm(sites)


##############################
## Figure A1                ##
##############################

## pull exceedences counts from Annual AQ Reports. 
pm.trend<-data.frame(year=2010:2016, 
                     cdf=c(74, 65, 70, 93, 79, 62, 71), 
                     mesa2=c(41, 33, 36, 55, 39, 30, 43))

## plot it
svg("figa1.svg", width = 8, height = 6, 
    pointsize = 10)

## set up plot area
plot(pm.trend$year, pm.trend$cdf, type="n",
     bty = "n", xaxt = "n", yaxt = "n",
     ylim=c(15, 100), xlim = c(2010, 2017),
     ylab="",
     xlab="")

## add axes
axis(1, at = 2010:2016, labels = 2010:2016)
axis(2, at = seq(20, 100, 20), las = 1)

## CDF trend line
points(pm.trend$year, pm.trend$cdf,
       type="l",
       pch=16,
       lwd=2,
       col=colors["cdf", "colors"])

## Mesa2 trend line
points(pm.trend$year, pm.trend$mesa2,
       type="l",
       pch=16,
       lwd=2,
       col=colors["mesa", "colors"])

## legend 
text(x = 2016, y = pm.trend[7, 2:3],
     pos = 4,
     labels = colors[c("cdf", "mesa"), "sites"],
     font = 2,
     col = colors[c("cdf", "mesa"), "colors"])


dev.off()
## clean up
rm(pm.trend)


################################
## Figures A2-A4              ##
################################

# load data. Historic hourly data from CDF was previous extracted from AQS and formatted
# for a different analysis, so just use that cleaned up file:
old<-read.csv("arch-cdf.csv", stringsAsFactors = FALSE) 

# format date, month, year variables
old$date<-as.POSIXct(old$date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
old$month<-format(old$date, "%m")
old$year<-format(old$date, "%Y")

# load libaries 
library(openair) # for wind rose function
library(dplyr)   # for data sorting

# plot function
plot.rose<-function(monthid){
  
  old %>%
    filter(month==monthid) %>%
    select(ws=wsv, wd=wdv, date) -> sub
  
  windRose(sub, main="", type="year", 
           paddle=FALSE, key.position="right", annotate = FALSE,
           key = list(footer="mph"), breaks=c(0, 3,6, 9, 12), grid.line=10)
}

###
# Figures A2-A4
###

# April
svg("figa2.svg", width = 8, height = 4, 
    pointsize = 10)
plot.rose("04")   
dev.off()

# May
svg("figa3.svg", width = 8, height = 4, 
    pointsize = 10)
plot.rose("05")   
dev.off()

# June
svg("figa4.svg", width = 8, height = 4, 
    pointsize = 10)
plot.rose("06")   
dev.off()

# Clean up
rm(old, plot.rose)


##########################################
## Filter Days Analysis (w/ Mel Zeldin) ##
##########################################

##################
## load needed libraries and functions:
library(openair)
library(dplyr)
source('AQSloader.R')

##################
## Load CDF Data from AQS export 
##################
cdf<-load.aqs("AMP501_1594974-0.txt")
names(cdf)<-c("date", "ws", "wd", "pm10")
cdf <- cdf[cdf$date < as.POSIXct("2017-01-01", tz = "UTC"), ]
###################
## bring in S1 wind data:
###################

# get file names
files <- list.files(pattern = "^vdv")

# loop thru files, load them, then merge together
for (i in 1:length(files)){  
  assign(paste0("s1.", i), read.csv(files[i], comment.char="#", skip=6, as.is = TRUE))
}
s1 <- do.call(rbind, mget(paste0("s1.", 1:length(files))))
rm(list=paste0("s1.", 1:length(files)), files, i)

# format S1
s1 <- s1[, c(1, 31, 24)]
names(s1)<-c("date", "wd", "ws")
s1$date<-as.POSIXct(as.character(s1$date), tz="UTC", format="%Y-%m-%d %H:%M:%S")

##################
## merge hourly cdf and s1 data
##################

names(cdf)[-1]<-sapply(names(cdf)[-1],FUN=paste, "cdf", sep=".")
names(s1)[-1]<-sapply(names(s1)[-1],FUN=paste, "s1", sep=".")

data<-merge(cdf, s1, by="date", all = TRUE)
rm(cdf,s1, load.aqs)


###################
## Apply filter criteria
###################

## need these variables later
data$day<-format(data$date, "%Y-%m-%d")
data$hour<-format(data$date, "%H")


## filter to only the hours 1000-1500
data %>%
  filter (hour >= 10) %>%
  filter (hour <= 15) -> noprecip   

## apply criterion: Site S1 must have all hourly wind speeds > or = 5 m/s

noprecip %>% 
  group_by(day) %>%
  summarize(min.ws = min(ws.s1, na.rm=T)) -> min.ws

id<-min.ws$day[min.ws$min.ws >= 5]
a<-noprecip$day %in% id
noprecip<-noprecip[a,]

## apply criterion:  Site S1 must NOT have any hourly wind direction >310 degrees

noprecip %>% 
  group_by(day) %>%
  summarize(max.wd = max(wd.s1, na.rm=T)) -> max.wd

id<-max.wd$day[max.wd$max.wd <= 310]
a<-noprecip$day %in% id
noprecip<-noprecip[a,]

## apply criterion:  The CDF site must NOT have any hourly wind direction <285 degrees

noprecip %>% 
  group_by(day) %>%
  summarize(min.wd = min(wd.cdf, na.rm=T)) -> min.wd
id<-min.wd$day[min.wd$min.wd >= 285]  
a<-noprecip$day %in% id
noprecip<-noprecip[a,]

## apply criterion:  Site S1 must have at least 3 of the 6 hours > 10 m/s
noprecip %>% 
  group_by(day) %>%
  summarize(hours = sum(ws.s1>10, na.rm=T)) -> hours

id<-hours$day[hours$hours >= 3]
a<-noprecip$day %in% id
noprecip<-noprecip[a,]

## apply criterion:  Site S1 vector average wind direction must be between 
## 285 and 300 degrees for the 6-hr period.

noprecip$cos<-cos(2*pi*noprecip$wd.s1/360)*noprecip$ws.s1
noprecip$sin<-sin(2*pi*noprecip$wd.s1/360)*noprecip$ws.s1

noprecip %>%
  group_by(day) %>%
  summarize(cos = sum(cos, na.rm=T)/6) ->cos
noprecip %>%
  group_by(day) %>%
  summarize(sin = sum(sin, na.rm=T)/6) ->sin
vec<-merge(cos, sin, by="day")
vec$ws<-sqrt(vec$sin^2+vec$cos^2)
vec$wd<-atan2(vec$sin, vec$cos)*180/pi+360

id<-vec$day[vec$wd >= 285 & vec$wd <= 300]
a<-noprecip$day %in% id
noprecip<-noprecip[a,]

## require all 6 pm10 measurements to be valid
noprecip %>%
  group_by(day) %>%
  summarize(pm = sum(!is.na(pm10.cdf))) -> pm

id<-pm$day[pm$pm != 6]
a<-which(noprecip$day %in% id)
noprecip<-noprecip[-a,]

## require all 6 ws.S1 measurements to be valid
noprecip %>%
  group_by(day) %>%
  summarize(ws = sum(!is.na(ws.s1))) -> ws

id<-ws$day[ws$ws != 6]
a<- which(noprecip$day %in% id)
if(length(a) > 0) noprecip<-noprecip[-a,]


## cleanup
rm(cos, hours, max.wd, min.wd, min.ws, pm, sin, vec, ws, a, id)

##############################
## summarize data
# how many days do we have?
length(unique(noprecip$day)) 

noprecip$ratio <- noprecip$pm10.cdf/noprecip$ws.s1

# calc summary
noprecip %>% 
  group_by(year=format(noprecip$date, "%Y")) %>%
  summarize(days=length(pm10.cdf)/6,
            mean.pm=round(mean(pm10.cdf),0),
            sd.pm=round(sd(pm10.cdf),0),
            max.pm=round(max(pm10.cdf),0),
            mean.wscdf=round(mean(ws.cdf),1),
            sd.wscdf=round(sd(ws.cdf),1),
            max.wscdf=round(max(ws.cdf),1),
            mean.wsS1=round(mean(ws.s1),1),
            sd.wsS1=round(sd(ws.s1),1),
            max.wsS1=round(max(ws.s1),1),
            mean.ratio = round(mean(ratio), 1))-> overview.noprecip

overview.noprecip # print data for table in Appendix

################
# some exploratory graphs (not in report):
################
boxplot(noprecip$pm10.cdf~format(noprecip$date, "%Y"),
        main="CDF hourly PM10 during Filter Days")

par(mfrow=c(3,2))
for (i in 2011:2016){
  hist(noprecip$pm10.cdf[format(noprecip$date, "%Y")==i],
       main="", xlab=i)
}
par(mfrow=c(1,1))

plot(noprecip$ws.s1, noprecip$pm10.cdf,
     col = format(noprecip$date, "%Y"), pch = 16)

legend("topleft", unique(format(noprecip$date, "%Y")),
       col = unique(format(noprecip$date, "%Y")),
       pch = 16)


############################
## Figure A5: plot normalized values...
############################

svg("figa5.svg", width = 6, height = 4, 
    pointsize = 10)

plot(overview.noprecip$mean.wsS1, overview.noprecip$mean.pm,
     ylab="Mean CDF PM10, ug/m3",
     xlab="Mean S1 wind speed, m/s",
     xaxt = "n", yaxt = "n", bty = "n",
     xlim=c(10,12),
     ylim=c(250, 370))
axis(1, at = 10:12)
axis(2, at = seq(260, 360, by = 20), las = 2)
text(overview.noprecip$mean.wsS1, overview.noprecip$mean.pm, labels=overview.noprecip$year, pos=1)

dev.off()

#############################
## ANOVA

## prep data
noprecip$year<-format(noprecip$date, "%Y")
noprecip$ratio<-with(noprecip, pm10.cdf/ws.s1)

## doublecheck mean of ratios
mean(noprecip$ratio[noprecip$year < 2015])

## are the ratios gaussian?
hist(noprecip$ratio, freq=F)   #close enough; actually tighter than gaussian
lines(density(noprecip$ratio))
lines(seq(0, 60, by = 0.1), dnorm((seq(0, 60, by = 0.1)),mean(noprecip$ratio), sd(noprecip$ratio) ), col="blue")

## are the baseline years significantly different from one another?
m1 <-lm(ratio~year, data=noprecip, subset=year <= 2014)
summary(m1)## 
anova(m1)  ## NO, p-value=0.09458
par(mfrow=c(2,2))
plot(m1)   ## looks OK

## is 2015 different from the pooled baseline years?
m2<-lm(ratio~(year==2015), noprecip, subset = year != 2016)
summary(m2)
anova(m2) ## YES, p-value=0.04945
plot(m2)  ## looks OK

# equal variance?
var.test(noprecip$ratio[noprecip$year < 2015], noprecip$ratio[noprecip$year == 2015])

## is 2016 different from the pooled baseline years?
m3<-lm(ratio~(year==2016), noprecip, subset = year != 2015)
summary(m3)
anova(m3) ## YES, p-value=0.0005
plot(m3)  ## looks OK

var.test(noprecip$ratio[noprecip$year < 2015], noprecip$ratio[noprecip$year == 2016])

# 2016, exlcuding post-mitigation filter days
summary(lm(ratio~(year==2016), data=noprecip, 
           subset= date < as.POSIXct("2016-07-30", tz = "UTC") & year != 2015))

# 2016 only, mitigation vs non-mitigation days
summary(lm(ratio~(date < as.POSIXct("2016-07-30", tz = "UTC")), data=noprecip, 
           subset= year == 2016))

# Jan - August vs sept - dec, baseline years
summary(lm(ratio~(as.numeric(format(date, "%m")) < 8), data=noprecip, 
           subset= year < 2015))


# 2016 vs baseline, peri-mitigation months only
summary(lm(ratio~(year == 2016), data=noprecip, 
           subset= (as.numeric(format(date, "%m")) < 9) & year != 2015))

summary(lm(ratio~(year == 2016), data=noprecip, 
           subset= (as.numeric(format(date, "%m")) > 8) & year != 2015))


## 2015 vs baseline
## t-test, assuming equal variances gives identical results as m2
t.test(noprecip$ratio[noprecip$year < 2015],noprecip$ratio[noprecip$year==2015], var.equal =T)
## assumption of equal variances is valid
var.test(noprecip$ratio[noprecip$year < 2015],noprecip$ratio[noprecip$year==2015])
## try non-parametric, even though ratio is ~Normal.
kruskal.test(ratio~(year==2015), data=noprecip[noprecip$year != 2016, ])      ## 2015 vs not 2015: p-value = 0.03037
with(noprecip[noprecip$year < 2015,], kruskal.test(ratio, as.factor(year))) #2010 thru 2014 vs each other


## 2016 vs baseline
## t-test, assuming equal variances gives identical results as m3
t.test(noprecip$ratio[noprecip$year < 2015],noprecip$ratio[noprecip$year==2016], var.equal =T)
## assumption of equal variances is valid (just barely)
var.test(noprecip$ratio[noprecip$year < 2015],noprecip$ratio[noprecip$year==2016])
## try non-parametric, even though ratio is ~Normal.
kruskal.test(ratio~(year==2016), data=noprecip[noprecip$year != 2015, ])      ## 2016 vs baseline p-value = 0.00013


######################
## clean up
rm(i, m1, m2, m3, noprecip, overview.noprecip)
par(mfrow=c(1,1))

####################################################
## Decision Tree                                  ##
####################################################


###################
## Prepare data for decision tree analysis
###################

data$hour <- as.numeric(data$hour)

## need a "wide" data.frame, with all hours for each day on the same row

d<-data[data$hour==0, -c(7,8)]
names(d)<-paste(names(d), 0, sep=".")

for(i in 1:23){
  dd<-data[data$hour==i, -c(1,7,8)]
  names(dd)<-paste(names(dd), i, sep=".")
  d<-cbind(d,dd)
}
rm(dd, i)

## calculate 24-hr PM averages
mean2<-function(x) {   ## create AQS-like averages: Need at least 16 valid hours & truncate result
  ifelse(sum(is.na(x))<7, trunc(mean(x, na.rm=TRUE)), NA)
}


d$pm10.ave<-apply(d[,grep("pm10", names(d))], 1, mean2) 
d<-d[,-c(grep("pm", names(d)[1:121]))]                     ## remove hourly pm data
d$exceed<-as.factor(ifelse(d$pm10.ave > 50, "yes", "no"))  ## identify PM10 exceedences

##format dates
names(d)[1]<-"date"
d$year<-format(d$date, "%Y")

############################
## Grow and Prune Decision Tree

## train on all data, excluding 2015 & 2016

## trees() ignores cases with NA's, so..
##     first train on full dataset to select variables
##     then retrain with unused variables excluded, in order to increase number of training cases
##     then prune, running CV a few times to find best, since there's a random component
##     then retrain using retained variables, then re prune
##     then bootstrap, then look at 2015.

## load library
library(tree)

## train on 2010-2014 using all wd/ws variables
## diff between results in 2015 AQR and this year appears to be due to S1 data exported from T&B 

tree.all <- tree(exceed ~ . ,data = d[d$year < 2015,-c(1,98,100)], split="gini")
summary(tree.all) # misclass: 0.04957 = 64 / 1291 
plot(tree.all)
text(tree.all)

## retrain, using only variables that were selected above.
vars<-unique(as.character(tree.all$frame[,1]))
vars<-vars[-which(vars=="<leaf>")]
dd<-d[,c("exceed", "year", vars)]
dd<-dd[complete.cases(dd),]

tree.all2<-tree(exceed ~ . , data= dd[dd$year < 2015,] , split="gini")
summary(tree.all2) # misclass: 0.05146 = 67 / 1302 

summary(dd$exceed[dd$year < 2015]) # NULL error rate = 286/(286+1016) =  0.2196621

## re-retrain, using only variables that were selected above.
vars<-unique(as.character(tree.all2$frame[,1]))
vars<-vars[-which(vars=="<leaf>")]
dd<-d[,c("year", "exceed", vars)]
dd<-dd[complete.cases(dd),]

tree.all3<-tree(exceed ~ . , data= dd[dd$year < 2015,] , split="gini")
summary(tree.all3) # misclass: 0.05146 = 67 / 1302 


# prune tree; Run these next two lines a few times, to get a feel for best size
cv.tree.all3<-cv.tree(tree.all3, FUN = prune.misclass)
plot(cv.tree.all3)  ## 3 or 4 usually seems best
prune.all2<-prune.misclass(tree.all2, best = 3)

summary(prune.all2) # 3 node: 0.094 = 123/1302
plot(prune.all2)
text(prune.all2)

# re-train again, using only variables in pruned tree 
vars<-unique(as.character(prune.all2$frame[,1]))
vars<-vars[-which(vars=="<leaf>")]
dd<-d[,c("year", "exceed", vars)]
dd<-dd[complete.cases(dd),]

tree.all4<-tree(exceed ~ . , data= dd[dd$year < 2015,] , split="gini")
summary(tree.all4) # misclass :0.07522 = 102 / 1356
plot(tree.all4)
text(tree.all4)
summary(dd$exceed[dd$year < 2015]) # NULL error: 0.218

# re-prune..
cv.tree.all4<-cv.tree(tree.all4, FUN = prune.misclass)
plot(cv.tree.all4)  ## 3 usually seems best
prune.all4<-prune.misclass(tree.all4, best = 3)

summary(prune.all4) # misclassifcation rate: 0.0937 w/ 3 nodes
plot(prune.all4)
text(prune.all4)
summary(dd$exceed[dd$year < 2015]) # NULL error: 0.218 

# confusion matrix for final tree:
tree.pred = predict(prune.all4, dd, type = "class")
trues<-dd$exceed
table(trues, tree.pred)

# bootstrap misclassification error rate
# there's a random element here, so results in published report might not match exactly

error.rate<-rep(NA, 1000)

for(i in 1:1000){  # this loop will take a little while to execute
  boot.sample<-dd[sample(1:nrow(dd[dd$year < 2015,]),nrow(dd[dd$year < 2015,]), replace=TRUE ), ]
  boot.pred<-predict(prune.all4, boot.sample, type="class")
  boot.trues<-boot.sample$exceed
  conf.mat<-table(boot.pred, boot.trues)
  error.rate[i] <- 1-sum(diag(conf.mat))/sum(conf.mat)
}

summary(error.rate)
hist(error.rate)
abline(v=0.0937)
quantile(error.rate, probs = c(0.025, 0.975)) # 7.8 to 11.0 % ## theres a random e
round(100-100*quantile(error.rate, probs = c(0.025, 0.975)), 1)
abline(v=quantile(error.rate, probs = c(0.025, 0.975)) )

rm(i, boot.sample, boot.pred, boot.trues, conf.mat, error.rate)

################
## Figure A6

## need to include only days which have data for CDF WD at 1300, a valid 24-hr PM10 average, and 
## data for S1 WS at 1500:

vars<-unique(as.character(prune.all4$frame[,1]))
vars<-vars[-which(vars=="<leaf>")]
ddd<-d[,c("year","pm10.ave", "exceed", vars)]
ddd<-ddd[complete.cases(ddd),]

## days meeting rule criteria, per year
preds<-predict(prune.all4,ddd, type="class")
preds<-tapply(preds, ddd$year, FUN=function(a) sum(a=="yes"))  # sum up by year

## sum of exceedence days each year
exceeds<-tapply(ddd$exceed, ddd$year, FUN=function(a) sum(a=="yes", na.rm=TRUE))

## x-axis
years<-c(2011, 2012, 2013, 2014, 2015, 2016)

#### fig a6
svg("figa6.svg", width = 6, height = 4, 
    pointsize = 10)

## plot
plot(years, preds, type="n",
     ylim=c(40, 100), xlim = c(2011, 2017),
     las=1, bty = "n", 
     ylab="Days",
     xlab="", xaxt = "n")

axis(1, at = 2011:2016)

points(years, exceeds,
       type="l",
       pch=16,
       lwd=2,
       col=colors["cdf", "colors"])
points(years, preds,
       type="l",
       pch=16,
       lwd=2,
       col=colors["carrizo", "colors"])

text(x = 2016, y = c(exceeds[6], preds[6]),
     pos = 4,
     labels = c("Observed", "Predicted"),
     font = 2,
     col = colors[c("cdf", "carrizo"), "colors"])

dev.off()

# Trend-line for observed exceedences doesn't exactly line up with trend line for CDF exceedence in
# Figure A1, because days missing the data needed by the decision tree rule are excluded here.

######################
## trend plot (not in report)

plot(ddd$pm10.ave, type="l")
cols<-ifelse(predict(prune.all4,ddd, type="class")=="no", "lightblue", "red")
points(ddd$pm10.ave, pch=16, cex=0.5, col=cols)
abline(h=50, col="grey", lwd=0.5)
abline(h=0, col=1, lwd=0.5)

######################
## clean up
rm(d, dd, ddd, cols, cv.tree.all3, cv.tree.all4, exceeds, preds, prune.all2, prune.all4,
   tree.all, tree.all2, tree.all3, tree.all4, tree.pred, trues, vars, years, mean2, data, colors)
