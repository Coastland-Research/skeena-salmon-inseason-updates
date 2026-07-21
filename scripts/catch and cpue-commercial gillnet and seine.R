library(janitor)
#creates plots of gillnet and seine catch by species with kept and released
#creates a plot of 
seinecatch<-fread("data/current_year/commercial catch 2026-seine.csv")
gncatch <- fread("data/current_year/commercial catch 2026-gillnet.csv")

allcatch <- bind_rows(seinecatch, gncatch)%>%
  pivot_longer(`Sockeye (Kept)`:`Steelhead (Released)`,names_to="Species.Disposition",values_to="Catch")%>%
  filter(Date>as.Date("2026-07-05")&Date<as.Date("2026-08-10"))%>%
  mutate(Catch=na_if(Catch,0))%>%
  mutate(Species=case_when(Species.Disposition=="Sockeye (Kept)"~"Sockeye",
                           Species.Disposition=="Sockeye (Released)"~"Sockeye",
                           Species.Disposition=="Coho (Kept)"~"Coho",
                           Species.Disposition=="Coho (Released)"~"Coho",
                           Species.Disposition=="Pink (Kept)"~"Pink",
                           Species.Disposition=="Pink (Released)"~"Pink",
                           Species.Disposition=="Chinook (Kept)"~"Chinook",
                           Species.Disposition=="Chinook (Released)"~"Chinook",
                           Species.Disposition=="Chum (Kept)"~"Chum",
                           Species.Disposition=="Chum (Released)"~"Chum",
                           Species.Disposition=="Steelhead (Kept)"~"Steelhead",
                           Species.Disposition=="Steelhead (Released)"~"Steelhead"))%>%
  mutate(Disposition=case_when(Species.Disposition=="Sockeye (Kept)"~"Kept",
                               Species.Disposition=="Sockeye (Released)"~"Released",
                               Species.Disposition=="Coho (Kept)"~"Kept",
                               Species.Disposition=="Coho (Released)"~"Released",
                               Species.Disposition=="Pink (Kept)"~"Kept",
                               Species.Disposition=="Pink (Released)"~"Released",
                               Species.Disposition=="Chinook (Kept)"~"Kept",
                               Species.Disposition=="Chinook (Released)"~"Released",
                               Species.Disposition=="Chum (Kept)"~"Kept",
                               Species.Disposition=="Chum (Released)"~"Released",
                               Species.Disposition=="Steelhead (Kept)"~"Kept",
                               Species.Disposition=="Steelhead (Released)"~"Released"))

#summarise encounters by day by species and add CPUE
cpue.allcatch<-allcatch%>%
  group_by(Date,Gear,Effort,Species)%>%
  summarise(Encounters=sum(Catch,na.rm=TRUE))%>%
  mutate(CPUE=Encounters/Effort)

#catch plot
gillnet.seine.catch.plot<-
  ggplot(allcatch,aes(x=Date,y=Catch,fill=Gear))+
  geom_col()+
  facet_grid(Species~Disposition,scales="free_y")+
  scale_fill_brewer(palette="Set1")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = 0.5, hjust=0.5))

#cpue plot
gillnet.seine.cpue.plot<-
  ggplot(cpue.allcatch,aes(x=Date,y=CPUE,color=Gear))+
  geom_point()+
  facet_grid(Species~Gear,scales="free_y")+
  scale_color_brewer(palette="Set1")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = 0.5, hjust=0.5))

allcatch.totals<-allcatch%>%group_by(Gear,Species,Disposition)%>%summarise(Total=sum(Catch,na.rm=TRUE))

kept.table<-allcatch.totals%>%pivot_wider(names_from=c("Gear","Disposition"),values_from="Total")%>%
  select(Gillnet_Kept,Seine_Kept)%>%
  rename(Gillnet=Gillnet_Kept,Seine=Seine_Kept)%>%
  adorn_totals("col",name="Total Kept")

released.table<-allcatch.totals%>%pivot_wider(names_from=c("Gear","Disposition"),values_from="Total")%>%
  select(Gillnet_Released,Seine_Released)%>%
  rename(Gillnet=Gillnet_Released,Seine=Seine_Released)%>%
  adorn_totals("col",name="Total Releases")

kept.table.out<-kable(kept.table,centering=FALSE,caption=paste0("Total kept catch of coho, chum, chinook, sockeye and pink salmon,
                                    and steelhead in the commercial marine Area C and A gillnet and seine fisheries to ",
                                    tyee.date,"."))

released.table.out<-kable(released.table,centering=FALSE,width="50%",caption=paste0("Total released catch of coho, chum, chinook, sockeye and pink salmon,
                                    and steelhead in the commercial marine Area C and A gillnet and seine fisheries to ",
                                                tyee.date,"."))
