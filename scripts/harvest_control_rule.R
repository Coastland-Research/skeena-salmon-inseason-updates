### Area C Gillnet and Area A Seine

# The following section shows the harvest control rule for commercial sockeye 
# fisheries in the Skeena, the estimated TAC for various run-timings, 
# and the available information on catches in the Area 4 commercial marine gillnet 
# and seine fisheries targeting sockeye at the mouth of the Skeena River 
# (generally sub-areas 4-12 and 4-15). Information should be considered preliminary 
# until reviewed by DFO staff in the post-season.
data<-read_excel("data/2024_archive/Tyee data 2024.xlsx",sheet="Tyee") %>%
  mutate(Date = as.Date(Date))

hcr<-data.frame(TRTC=c(0,1050000,2000000,4000000,5000000),
                HR=c(0,0,.2,.4,.4))

data<-data%>%rowwise()%>%mutate("TAC-average"=case_when(average<1050000~0,
                                                        average>=1050000&average<2000000~((average-1050000)/(2000000-1050000)*(.2-.0)+0)*average,
                                                        average>=2000000&average<4000000~((average-2000000)/(4000000-2000000)*(.4-.2)+.2)*average,
                                                        average>=4000000~.4*average),
                                "TAC-late"=case_when(late<1050000~0,
                                                     late>=1050000&late<2000000~((late-1050000)/(2000000-1050000)*(.2-.0)+0)*late,
                                                     late>=2000000&late<4000000~((late-2000000)/(4000000-2000000)*(.4-.2)+.2)*late,
                                                     late>=4000000~.4*late),
                                "TAC-early"=case_when(early<1050000~0,
                                                      early>=1050000&early<2000000~((early-1050000)/(2000000-1050000)*(.2-.0)+0)*early,
                                                      early>=2000000&early<4000000~((early-2000000)/(4000000-2000000)*(.4-.2)+.2)*early,
                                                      early>=4000000~.4*early))

p1<-ggplot(hcr,aes(x=TRTC/10^6,y=HR))+
  geom_line(size=1.1)+
  geom_vline(xintercept=data$`TAC-average`/10^6,linetype="dashed",col="#E41A1C")+
  geom_vline(xintercept=data$`TAC-late`/10^6,linetype="dashed",col="#4DAF4A")+
  geom_vline(xintercept=data$`TAC-early`/10^6,linetype="dashed",col="#377EB8")+
  theme_bw()+
  ylim(0,1)+
  labs(y = "Canadian Harvest Rate", x = "Skeena sockeye return to Canada (millions)")


ggcatch<-data%>%select(Date,`Cumulative Catch 2023`=cumcatch,
                       `TAC-average`,`TAC-early`,`TAC-late`)%>%
  pivot_longer(2:5,names_to="Catch Type",values_to="Catch")%>%
  filter(Date>"2024-07-01"&Date<"2024-09-01")


p2<-ggplot(ggcatch,aes(x=Date,y=Catch,color=`Catch Type`,size=`Catch Type`,linetype=`Catch Type`))+
  geom_line(size=1.1)+
  scale_color_manual(values=c("black","#E41A1C","#377EB8","#4DAF4A"))+
  scale_size_manual(values=c(2,1,1,1))+
  scale_linetype_manual(values=c("solid","dashed","dashed","dashed"))+
  theme_bw()+
  labs(y="Number of Sockeye")+
  theme(legend.position="top")

catch<-data%>%select(Date,Gillnet=gncatch,Seine=sncatch)%>%
  pivot_longer(2:3,names_to="Gear",values_to="Catch")%>%
  filter(Date>"2024-07-01"&Date<"2024-09-01")

p3<-ggplot(catch,aes(x=Date,y=Catch,fill=Gear))+
  geom_col()+
  scale_fill_brewer(palette="Set1")+
  theme_bw()+
  theme(legend.position="top")+
  expand_limits(x=c(as.Date("2024-07-01"),as.Date("2024-09-01")))

cpue<-data%>%select(Date,gncatch,sncatch,gneffort,sneffort)%>%
  mutate(Gillnet=gncatch/gneffort,Seine=sncatch/sneffort)%>%
  select(Date,Gillnet,Seine)%>%
  pivot_longer(2:3,names_to="Gear",values_to="CPUE")%>%
  filter(Date>"2024-07-01"&Date<"2024-09-01")

p4<-ggplot(cpue,aes(x=Date,y=CPUE,color=Gear))+
  geom_line()+geom_point()+
  scale_color_brewer(palette="Set1")+
  theme_bw()+
  theme(legend.position = "top")+
  labs(y="Sockeye CPUE")+
  expand_limits(y=0)+
  expand_limits(x=c(as.Date("2024-07-01"),as.Date("2024-09-01")))


