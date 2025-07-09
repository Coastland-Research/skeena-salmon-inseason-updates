
# calculate cumulative index.
# calculate percent cumulative index (cumulative/total cumulative)

library(readxl)
dailycum<-read_excel("data/2024_archive/Tyee data 2024.xlsx",sheet="tyee pcum")

daily_25 <- read_csv("data/common/tyee_indices_sockeye_1956-2024.csv") %>%
  select(-c(MONTH, DAY, `month-day`))
  
gg.daily.pcum<-dailycum%>%select("Date":"2023")%>%
  pivot_longer("1970":"2023",names_to="Year",values_to="Sockeye")%>%
  mutate(Year=as.numeric(Year))

data2<-data.frame(date=data$Date,cumtrtc=data$cumtyee)%>%
  mutate(`1M`=cumtrtc/1000000,
         `1.5M`=cumtrtc/1500000,
         `2M`=cumtrtc/2000000,
         `2.5M`=cumtrtc/2500000)%>%
  pivot_longer(`1M`:`2.5M`,names_to="estimate",values_to="p")%>%
  select(-cumtrtc)

ggplot(gg.daily.pcum,aes(x=Date,y=Sockeye,group=Date))+
  geom_boxplot(fill="white",color="grey70",alpha=.5)+
  geom_line(data=data2,aes(x=as.POSIXct(date),y=p,color=estimate,group=estimate),linewidth=1,linewidth=1)+
  geom_segment(aes(x=as.POSIXct("2024-06-01"),y=.5,xend=as.POSIXct("2024-07-24"),yend=.5),color="blue",linewidth=1,linetype="dashed")+
  geom_segment(aes(x=as.POSIXct("2024-07-24"),y=.5,xend=as.POSIXct("2024-07-24"),yend=0),color="blue",arrow = arrow(length=unit(.25, 'cm')),linewidth=1,linetype="dashed")+
  #ylim(0,150000)+
  theme_bw()+
  ylim(0,1)+
  labs(color="Estimate",y="Cumulative Proportion of TRTC")+
  theme(legend.position = "bottom")

