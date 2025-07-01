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

