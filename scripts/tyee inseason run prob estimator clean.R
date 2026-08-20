require(matrixStats)
library(fitdistrplus)
library(reshape2)
library(tidyverse)

options(scipen=10000)

#read in-season data
dfindex<-read.csv("data/common/tyee_daily_indices_sockeye_1956-2025.csv", 
                  header = TRUE, check.names = FALSE)

# read current index data
current <- fread("data/current_year/tyee data 2026.csv") %>%
  mutate(Date = as.IDate(Date))%>%
  select(Date,"Index"=sockeye) %>%
  drop_na(Index)

#read Q data
dftyeeQ<-read.csv("data/common/tyee Q.csv",header=TRUE,sep=",")

years <- intersect(names(dfindex), as.character(dftyeeQ$Year))

dfindex <- dfindex %>%
  rename_with(~paste0("ind.", .x), all_of(years))

for (yr in years) {
  q <- dftyeeQ$q[dftyeeQ$Year == as.numeric(yr)]
  
  dfindex[[paste0("est.", yr)]] <-
    dfindex[[paste0("ind.", yr)]]*q
}

info_cols <- c("Date", "MONTH", "DAY", "month-day")
year_cols <- as.character(sort(as.numeric(years)))
paired_cols <- unlist(lapply(year_cols, function(y) {
  c(paste0("ind.", y), paste0("est.", y))
}))

dfindex <- dfindex %>%
  select(all_of(info_cols), all_of(paired_cols)) %>%
  mutate(Date = format(as.Date(Date), "%d-%b")) %>%
  mutate(ind.2026 = current$Index[
    match(Date, format(current$Date, "%d-%b"))])


#read in run-timing from Tyee cumulative percent
dfRT<-read.csv("data/common/tyee cumulative percent 1970-2018.csv",check.names=FALSE,
               header=TRUE,sep=",")

# read in catch data
gillnet <- fread("data/current_year/commercial catch 2026-gillnet.csv") %>%
  select(Date, catch = `Sockeye (Kept)`)

seine <- fread("data/current_year/commercial catch 2026-seine.csv") %>%
  select(Date, catch = `Sockeye (Kept)`)

demo <- fread("data/current_year/fns demo catches 2026.csv") %>%
  filter(nation %in% c("Lax Kw'alaams", "Metlakatla", "North Coast Skeena")) %>%
  select(Date = date, catch = sockeye)

catch_daily <- bind_rows(gillnet, seine, demo) %>%
  group_by(Date) %>%
  summarise(
    catch = sum(catch, na.rm = TRUE),
    .groups = "drop") %>%
  arrange(Date) %>%
  mutate(cum_catch = cumsum(catch))

# set todays date
current_date <- max(current$Date)
cdate <- format(current_date,"%d-%b")
today<-grep(paste0("^",cdate,"$"),dfindex$Date)
#current day index sum
cum.index=sum(dfindex$ind.2026[1:today],na.rm=TRUE)
cum.index

#set catchability distribution to be used in the estimate
catchabilitymean<-"meanlast10"

#catchability distribution fit to logistic distribution
catchability<-dftyeeQ$catchability

#fit logistic to catchability
cfit<-fitdistr(catchability,"logistic")

if (catchabilitymean == "overall"){
meanc<-cfit$estimate[1]
} else if (catchabilitymean =="meanlast10") {
meanc<-mean(catchability[16:25])
} else if (catchabilitymean =="2020") {
meanc<-1058  
}
meanc

#create logistic distribution for 1/q based on location=mean given above
cdistfull<-rlogis(20000,location=meanc,scale=cfit$estimate[2])
#remove negative numbers
cdistpos<-cdistfull[!cdistfull<0]
#sample from positive distribution
cdist<-sample(cdistpos,10000,TRUE)

hist(catchability,prob=TRUE)
lines(density(cdist))

##Set run timing as Average or Late - late is 1 week late and adjusts the run 
# timing to 7 days earlier.
rt<-"Average"

if (rt == "Average"){
  rt.adj=0
} else if (rt =="Late") {
  rt.adj=-7
} 
rt.adj

#find todays row in cumulative run timing 
today<-grep(paste0("^",cdate,"$"),dfRT$Date)+rt.adj
#create vector for runtiming on today (1985-2018)
daily<-as.numeric(dfRT[today,11:length(dfRT)])
daily <- daily[is.finite(daily) & daily > 0]

#fit daily run timing to gamma (positive only) distribution
dailyfit <- fitdistr(daily, "gamma")

#create gamma distribution
RTdist<-rgamma(10000,shape=dailyfit$estimate[1],rate=dailyfit$estimate[2])
hist(daily,prob=TRUE)
lines(density(RTdist))
#figure for "todays" run timing to look at fit
png(paste0("Todays Run Timing ",cdate," ",rt,".png"),600,450)
hist(daily,breaks=10,main=paste0("Histogram of run-timing ",cdate),xlab="Proportion Run through Tyee")
legend("topright",lty=1,legend="Gamma fit",bty='n')
lines(density(RTdist),col="blue")
dev.off()

#calculate escapement distribution (1/q * index to date)
index_dist<-cum.index*cdist
hist(index_dist)

#calculate escapement distribution with run-timing uncertainty
esc.estimate<-index_dist / RTdist
hist(esc.estimate)

#add in catch from marine commercial
# catch=0
# esc.estimate<-esc.estimate+catch
# hist(esc.estimate,breaks=30)

catch_daily <- catch_daily %>%
  mutate(month_day = format(Date, "%d-%b"))
catch_to_date <- catch_daily$cum_catch[
  match(cdate, catch_daily$month_day)]
if (is.na(catch_to_date)) {
  catch_to_date <- 0
}

# add cumulative catch to TRTC estimate
esc.estimate <- esc.estimate + catch_to_date

hist(esc.estimate, breaks = 30)

#remove outliers created by early season RT distribution values < 0 or very
#small values
esc.estimate<-esc.estimate[esc.estimate <quantile(esc.estimate,.99)]
hist(esc.estimate)

#output percentiles from escapement distribution
probsout<-(quantile(esc.estimate,c(.1,.25,.5,.75,.9)))
probsout
cum.index


#Tyee prob distribution in season histogram
png(file=paste0("Tyee inseason histogram estimate P10P90 to ",cdate," ",rt,".png"), units='in',height=4,width=6,res=300)

eschist<-hist(esc.estimate,breaks=60,plot=FALSE)

hist(esc.estimate,60,col="grey",
     main=paste0("Frequency histogram of Tyee Inseason TRTC\n2026 to ",cdate," : ",rt," Timing"),
     xlab="Number of sockeye",ylab="Frequency",xlim=c(0,max(esc.estimate)))
text(max(esc.estimate)*.8,1500,paste0("Median=",round(median(esc.estimate),digits=0)))

tyeemedian<-round(median(esc.estimate),digits=0)
tyee90th<-round(quantile(esc.estimate,.9))
tyee10th<-round(quantile(esc.estimate,.1))
tyee25th<-round(quantile(esc.estimate,.25))
tyee75th<-round(quantile(esc.estimate,.75))

abline(v=tyeemedian,lwd=3,lty=2,col="blue")
abline(v=tyee90th,lwd=2,lty=2,col="black")
abline(v=tyee10th,lwd=2,lty=2,col="black")

legend("topright",c(paste0("Median=",tyeemedian),paste0("10th=",tyee10th),
                    paste0("25th=",tyee25th),paste0("75th=",tyee75th),
                    paste0("90th=",tyee90th)),bty='n',
       col=c("blue","black",NA,NA,"black"),lwd=c(3,2,NA,NA,2),lty=c(2,2,NA,NA,2))

clip(tyee10th,tyee90th,0,10000)

plot(eschist,col=rgb(0,0,255,max=255,alpha=75),add=TRUE)

dev.off()

####Point P90/P10 plot for 2025

#define input data
index.data<-dfindex$ind.2026
#define start day (has to be after gamm fits work
#also very little confidence really early on
startrunday<-which(dfindex$Date == "01-Jul")
endday <- max(which(!is.na(dfindex$ind.2025)))

looplength<-endday-startrunday

#create storage df/vectors
v<-data.frame(esc=numeric(looplength),p10=numeric(looplength),p90=numeric(looplength))
j=0

for (i in startrunday:endday){
  j=j+1
  cum.index<-sum(index.data[1:i], na.rm = T)
  index_dist<-cum.index*cdist
  daily<-as.numeric(dfRT[i,18:length(dfRT)])
  
  #fit daily run timing to gamma (positive only) distribution
  dailyfit<-fitdistr(daily,"gamma")
  RTdist<-rgamma(5000,shape=dailyfit$estimate[1],rate=dailyfit$estimate[2])
  
  esc_estimate2<-index_dist/RTdist
  #esc_estimate2<-esc_estimate2[esc_estimate2 < quantile(esc_estimate,.99)]
  #dates[j]<-dfRT$Date[i]
  v[j,]<-c(median(esc_estimate2),quantile(esc_estimate2,.1),quantile(esc_estimate2,.9))
} # ERROR HERE

#assign y-axis labels/points
dates<-as.character(dfindex$Date[startrunday:endday])
days<-dfindex$day[startrunday:endday]

#check lengths
length(days);length(v$esc)

#to date figure with 10/90th quantiles
png(file=paste0("Tyee estimate prob to ",cdate,".png"), units='in',res=300,height=6,width=6)
par(mar=c(3,7,4,1))
plot(days,v$p90,
     type='l',lwd=2,col="grey",lty=2,las=1,ylab="",xaxt='n',
     ylim=c(0,max(v$p90*1.2,na.rm=TRUE)))
axis(1,at=days,labels=dates)
polygon(c(days,rev(days)),c(v$p10,rev(v$p90)),col="grey85",border=NA)

lines(days,v$p90,lty=2,lwd=1,col="black")
lines(days,v$p10,lty=2,lwd=1,col="black")
lines(days,v$esc,lwd=3,col="dark blue")

title(ylab="Escapement Estimate",line=5)
title(xlab="Date",line=2)
mtext("Tyee In-Season TRTC Estimate",side=3,line=2,cex=1.2)
mtext("Incorporating uncertainty in Q and Run-Timing",side=3,line=1,cex=.8)
legend("topleft",legend=c("Median Estimate","P10/P90 Estimate"),
       col=c("dark blue","grey"),
       lwd=c(3,2),lty=c(1,2),bty='n')

dev.off()


#histograms all days for run timing 1985-2018
#can be changed to other years
#tryCatch allows loop to continue
pdf("RT histograms gamma only.pdf",width=8,height=11.5)
par(mfrow=c(4,3),mar=c(3,3,2,1))

for (i in 1:102){
  
  daily<-as.numeric(dfRT[i,18:length(dfRT)])
  #daily<-daily[daily<.2]
  #hist(daily,breaks=10)
  #daily
  tryCatch({
    
    #b<-fitdist(daily,"beta")
    g<-fitdist(daily,"gamma")
    #l<-fitdist(daily,"logis")
    
    #RTdistbeta<-rbeta(5000,shape1=b$estimate[1],shape2=b$estimate[2])
    RTdistgamma<-rgamma(5000,shape=g$estimate[1],rate=g$estimate[2])
    #RTdistlogis<-rlogis(5000,location=l$estimate[1],scale=l$estimate[2])
    
    hist(daily,breaks=10,ylim=c(0,12),main=dfRT[i,1])
    #lines(density(RTdistbeta),add=TRUE,col="red")
    lines(density(RTdistgamma),col="blue")
    #lines(density(RTdistlogis),add=TRUE,col="purple")
    lines(density(daily),col="green")
    
  },error=function(e){})
  
}

dev.off()

pdf("RT histograms.pdf",width=8,height=11.5)
par(mfrow=c(4,3),mar=c(3,3,2,1))

for (i in 1:102){
  
  daily<-as.numeric(dfRT[i,18:length(dfRT)])
  hist(daily,breaks=10,main=dfRT[i,1])
  lines(density(daily),add=TRUE)
  
}

dev.off()

#boxplot of run timing through Tyee 1985-2018 by day versus 2016-2018
RT<-dfRT[,c(2,18:length(dfRT))]
RTlong<-melt(RT,id.vars="Day")
head(RTlong)
names(dfRT)
RT2010on<-dfRT[,c(2,49:length(dfRT))]
RTlong2010on<-melt(RT2010on,id.vars="Day")

pdf("Tyee SX RT boxplot pch vs 2016-8.pdf",width=8.5,height=8.5)

boxplot(value~Day,data=RTlong,names=dfRT$Date,axes=TRUE,col="#FF000099",las=1,
        xlab="Date",pch=19,cex=.5,
        main="Sockeye Run-timing Through Tyee 1985-2018", ylab="Proportion Through")

boxplot(value~Day,data=RTlong2010on,names=dfRT$Date,axes=TRUE,col="#0000FF99",las=1,
        xlab="Date",pch=19,cex=.5,
        main="Sockeye Run-timing Through Tyee 1985-2018", ylab="Proportion Through",
        add=TRUE)

grid(NA,NULL,lwd=1,col="grey70",lty=2)

dev.off()

#boxplot of run timing through Tyee 1985-2018 by day versus 2016-2018
RT<-dfRT[,c(2,18:length(dfRT))]
RTlong<-melt(RT,id.vars="Day")

pdf("Tyee SX RT boxplot pch.pdf",width=8.5,height=8.5)

boxplot(value~Day,data=RTlong,names=dfRT$Date,axes=TRUE,col="grey90",las=1,
        xlab="Date",pch=19,cex=.5,
        main="Sockeye Run-timing Through Tyee 1985-2018", ylab="Proportion Through")

grid(NA,NULL,lwd=1,col="grey70",lty=2)

dev.off()



