#Explanation for figure
#This comparison aims to provide a coarse idea on the catchability at Tyee. #The red line in the figure below shows the difference between the escapement #past Tyee and the total run being counted at the Babine Fence, based on some #assumptions. These assumptions include 3 week migration timing, 10% #non-Babine stock composition and a rough estimate of in-river FSC and demo #harvest to date between Tyee and Babine. In 2023 some sockeye were missed #due to the delay from wildfire evacuations, however DFO and LBN staff #indicate that no sockeye were visible below the fence when it was evacuated, #and therefore the number of sockeye missed was probably fairly low. #Additional analysis will be required to provide an estimate of the number of #missed sockeye.

make.tyee.vs.babine.plot<-function(pernonbabine,p.catch.babine,ylow,yhigh,start.date,end.date) {

ty<-fread("data/current_year/Tyee data 2025.csv")%>%
  mutate(Date = as.Date(Date))%>%
  select(Date, Observed.Tyee=cum_check)

fe<-fread("data/current_year/babine fence 2025.csv")%>%
  mutate(Date = as.Date(Date),
         cumrun=cumsum(`Large Sockeye`))%>%
  select(Date,Actual.Fence=cumrun)

catch<-fread("data/current_year/inrivercatch.csv")%>%
  mutate(Date = as.Date(Date),
         catch = DemoCatch+FSCCatch)%>%
  select(Date,catch)

#inrivercatch<-read_excel("data/FNs catch 2024.xlsx",sheet="inriver catch") # update this to 2024
#inrivercatch$Date<-as.Date(inrivercatch$Date)
#ca<-inrivercatch%>%select(Date,inriver.harvest) # all 0 or NA...


#comp<-merge(ty,fe,all=TRUE)%>%merge(ca,all=TRUE)%>%
 #  mutate(Expected.Babine=lag((1-pernonbabine)*Observed.Tyee-inriver.harvest,21),
#         `Difference @ 3 Weeks`=Actual.Fence-Expected.Babine)%>%
#  select(-inriver.harvest)%>%
#  pivot_longer(2:5,names_to="Count",values_to="Fish")

comp<-merge(ty,fe,by="Date",all=TRUE)%>%merge(catch,by="Date",all=TRUE)%>%
  mutate(tyeeatbabine=lag((1-pernonbabine)*Observed.Tyee,21),
         catchatbabine=lag((p.catch.babine)*catch,10),
         Expected.Babine=tyeeatbabine-catchatbabine,
         `Difference @ 3 Weeks`=Actual.Fence-Expected.Babine)%>%
  select(Date,Observed.Tyee,Actual.Fence,Expected.Babine,`Difference @ 3 Weeks`)%>%
  pivot_longer(2:5,names_to="Count",values_to="Fish")


ggplot(comp,aes(x=Date,y=Fish,color=Count,size=Count,linetype=Count)) +
  geom_line() +
  theme_bw()+grids() +
  scale_colour_manual(values=c("black","red","seagreen4","blue")) +
  scale_linetype_manual(values=c("solid","solid","dashed","solid")) +
  scale_size_manual(values=c(2,1,1,1.5)) +
  theme(legend.position="top") +
  labs(y="Number of Large Sockeye") +
  ylim(ylow,yhigh)+
  xlim(start.date,end.date)

}
