
library(data.table)
library(tidyverse)
library(ggpubr)

#daily parameters
current.index<-42.08
multiplier=245
todate<-as.Date("2025-08-24")
todate.label<-"August 24"

#load historical data
daily<-fread("data/common/tyee_daily_indices_steelhead_1956-2024.csv", header = T)

#add cumulative daily index and fish and move to long format
data<-daily%>%
  mutate_if(is.character, as.numeric) %>%
  pivot_longer(`1956`:`2024`,names_to="Year",values_to="Index") %>%
  mutate(Year=as.numeric(Year)) %>%
  group_by(Year)%>%
  mutate(Index=replace_na(Index,0),
         CumIndex=cumsum(Index),
         Fish=Index*multiplier,
         CumFish=CumIndex*multiplier,
         FinalIndex=sum(Index,na.rm=TRUE),
         FinalFish=sum(Fish,na.rm=TRUE),
         p=CumIndex/FinalIndex,
         isittoday=ifelse(Date==todate,"today","nottoday"))

#proportion to date for run-timing proportion figure
todays.data<-data%>%
  filter(Date==todate)
med.today<-round(median(todays.data$p,na.rm=TRUE),2)
p25<-round(quantile(todays.data$p,.25,na.rm=TRUE),2)
p75<-round(quantile(todays.data$p,.75,na.rm=TRUE),2)

#estimate number of fish at todates proportions
med.fish<-round(multiplier*current.index/med.today,0)
p25.fish<-round(multiplier*current.index/p25,0)
p75.fish<-round(multiplier*current.index/p75,0)

#plot run timing boxplot with proportions and final estimates
ggplot(data,aes(x=Date,y=p,group=Date,fill=isittoday))+
  geom_boxplot()+
  scale_fill_manual(values=c("grey90","red"))+
  guides(fill="none")+
  scale_y_continuous(breaks=seq(0,1,.1))+
  geom_segment(xend=min(data$Date),yend=med.today,x=todate,y=med.today,
               arrow = arrow(length = unit(0.2, "inches")),linetype="dashed")+
  annotate(geom="text",label=paste0("Proportion=",med.today," (p25=",p25,", p75=",p75,")"),
           x=as.Date("2025-08-07"),y=.1,hjust=0)+
  annotate(geom="text",label=paste0("Steelhead=",med.fish," (p25=",p75.fish,", p75=",p25.fish,")"),
           x=as.Date("2025-08-07"),y=.05,hjust=0)+
  #geom_vline(xintercept = todate,color="purple",linewidth=1.5,alpha=.5)+
  labs(y="Cumulative Proportion",title="Steelhead: Cumulative Proportion Through Tyee to Date")+
  theme_bw()

#ggsave(paste0("steelhead/steelhead run proportion plot ",todate.label,".png"))

#plot linear model todate
ggplot(todays.data,aes(x=CumIndex,y=FinalIndex))+
  geom_point()+
  geom_smooth(method=lm)+
  stat_cor(method = "pearson", label.x = 90, label.y = 10,
           p.accuracy = 0.001, r.accuracy = 0.01)+
  labs(x=paste0("Cumulative Index to ",todate.label),y="Year End Index",title="Steelhead Index-to-Date versus Final Index")+
  theme_bw()

#ggsave(paste0("steelhead/steelhead index lm to final ",todate.label,".png"))

#regression model
dat <- data.frame(count=todays.data$CumFish, final=todays.data$FinalFish)
m <- lm(final ~ count, data = dat) 
summary(m)

# Create prediction interval data frame with upper and lower lines corresponding to sequence covering minimum and maximum of x values in original dataset
newx <- seq(min(dat$count,na.rm=TRUE), max(dat$count,na.rm=TRUE), by=10)
pred_interval <- predict(m, newdata=data.frame(count=newx), interval="prediction",
                         level = 0.75)
#pred_interval <- predict(m, newdata=data.frame(count=newx), interval="confidence",
#                         level = 0.95)
pred_interval <- as.data.frame(pred_interval)
pred_interval$newx<-newx

current=current.index*multiplier
currentx <- current
pred_current <- predict(m, newdata=data.frame(count=currentx), interval="prediction",
                        level = 0.75)
#pred_current <- predict(m, newdata=data.frame(count=currentx), interval="confidence",
#                        level = 0.95)
pred_current <- as.data.frame(pred_current)

ggplot(data=dat, aes(x = count)) + 
  geom_point(aes(y=final))+
  geom_ribbon(data=pred_interval,aes(x=newx,ymin = lwr, ymax = upr),
              fill = "grey60", alpha = 0.2)+
  geom_segment(x=current,y=0,xend=current,yend=pred_current$fit[1],linetype="dashed",color="blue",linewidth=1.5)+
  geom_segment(x=current,y=pred_current$fit[1],xend=0,yend=pred_current$fit[1],
               arrow = arrow(length = unit(0.2, "inches")),
               linetype="dashed",color="blue",linewidth=1)+
  labs(y="Final Estimated Run Past Tyee",x=paste0("Estimated Escapement past Tyee to ",todate.label))+
  annotate(geom="text",label=paste0("Median = ",round(pred_current$fit,0)),x=30000,y=10000,hjust=1)+
  annotate(geom="text",label=paste0("p25 = ",round(pred_current$lwr,0)),x=30000,y=6000,hjust=1)+
  annotate(geom="text",label=paste0("p75 = ",round(pred_current$upr,0)),x=30000,y=2000,hjust=1)+
  geom_line(data=pred_interval,aes(x=newx,y=fit),color="red")+
  theme_bw()

#ggsave(paste0("steelhead/steelhead escapement lm ",todate.label,".png"))
