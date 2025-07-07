# Cumulative proportion of run


i<-match(tyee.day,data$Date)

early<-data$early[i]
average<-data$average[i]
late<-data$late[i]

names(data)

pcum<-data%>%select(Date,cumtrtc,"Average (70-23)"=`pcum70-23`)%>%
  mutate(`1.5M`=cumtrtc/1500000,
         `1.75M`=cumtrtc/1750000,
         `2M`=cumtrtc/2000000,
         `2.5M`=cumtrtc/2500000,
         `3M`=cumtrtc/3000000,
         "Early (70-23)"=lead(`Average (70-23)`,7),
         "Late (70-23)"=lag(`Average (70-23)`,7))%>%
  select(-cumtrtc)%>%
  pivot_longer(2:9,names_to="Type",values_to="cp")%>%
  mutate(Type=factor(Type,levels=c("Early (85-21)","Average (70-23)", "Late (85-21)"
                                   ,"1.5M","1.75M","2M","2.5M","3M")))

cols<-RColorBrewer::brewer.pal(5, "Set1")

g1<-ggplot(pcum,aes(x=Date,y=cp,color=Type,linetype=Type))+
  geom_line(size=1)+
  scale_colour_manual(values=c("grey25","grey50","grey70",cols))+
  scale_linetype_manual(values=c("dashed","dashed","dashed","solid","solid","solid","solid","solid"))+
  theme_bw()+
  labs(y="Cumulative Proportion")+
  theme(legend.title=element_blank(),legend.background = element_blank())

g2<-ggplot(pcum,aes(x=Date,y=cp,color=Type,linetype=Type))+
  geom_line(size=1)+
  scale_colour_manual(values=c("grey25","grey50","grey70",cols))+
  scale_linetype_manual(values=c("dashed","dashed","dashed","solid","solid","solid","solid","solid"))+
  theme_bw()+
  labs(y="Cumulative Proportion")+
  theme(legend.title=element_blank(),legend.position=c(.17,.65),legend.background = element_blank())+
  ylim(0,1)+
  xlim(as.Date("2023-06-10"),as.Date("2023-08-05"))

ggarrange(g1,g2,align="v",ncol=1)

```

dailycum<-read_excel("data/2024_archive/Tyee data 2024.xlsx",sheet="tyee pcum")

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

