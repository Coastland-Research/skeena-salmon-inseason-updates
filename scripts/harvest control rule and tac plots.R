
#create harvest control rule line
#load tyee trtc model data and calculate TAC using HCR points

tac.data<-fread("data/current_year/Tyee model data 2025.csv")%>%
  mutate(Date = as.Date(Date))%>%
  mutate(TAC=case_when(Estimate<1050000~0,
                       Estimate>=1050000&Estimate<2000000~((Estimate-1050000)/(2000000-1050000)*(.2-.0)+0)*Estimate,
                       Estimate>=2000000&Estimate<4000000~((Estimate-2000000)/(4000000-2000000)*(.4-.2)+.2)*Estimate,
                       Estimate>=4000000~.4*Estimate))

hcr<-data.frame(TRTC=c(0,1050000,2000000,4000000,5000000),
                HR=c(0,0,.2,.4,.4))

#function to create hcr plot with current tac vertical lines
make.sx.hcr.plot<-function(hcr,tac.data) {

tac.early<-tac.data%>%filter(Date==tyee.day&Timing=="Early")%>%getElement("Estimate")
tac.average<-tac.data%>%filter(Date==tyee.day&Timing=="Average")%>%getElement("Estimate")
tac.late<-tac.data%>%filter(Date==tyee.day&Timing=="Late")%>%getElement("Estimate")

ggplot(hcr,aes(x=TRTC/10^6,y=HR))+
  geom_line(linewidth=1.1)+
  geom_vline(xintercept=tac.early/10^6,linetype="dashed",col="#E41A1C")+
  geom_vline(xintercept=tac.average/10^6,linetype="dashed",col="#4DAF4A")+
  geom_vline(xintercept=tac.late/10^6,linetype="dashed",col="#377EB8")+
  theme_bw()+
  ylim(0,1)+
  labs(y = "Canadian Harvest Rate", x = "Skeena sockeye return to Canada (millions)")
}

#make.sx.hcr.plot(hcr,tac.data)

#create figure with Ttac.data = #create figure with TAC trend and total cumulative catch for current year

#load catch data and combine for total cumulative catch
catch.gn<-fread("data/current_year/commercial catch 2025-gillnet.csv")
catch.sn<-fread("data/current_year/commercial catch 2025-seine.csv")

make.tacandtotalcatch.plot<-function(catch.gn,catch.sn,tac.data) {

cum.sx.catch<-rbind(catch.gn,catch.sn)%>%
  select(Date,Gear,Catch=`Sockeye (Kept)`)%>%
  group_by(Date)%>%
  summarise(total.catch=sum(Catch,na.rm=TRUE))%>%
  replace(is.na(.), 0)%>%
  mutate(cum.catch=cumsum(total.catch))%>%
  filter(Date<tyee.day)

tac.estimates<-tac.data%>%select(Date,Timing,TAC)

ggplot(tac.estimates,aes(x=Date,y=TAC,color=Timing))+
  geom_line(linewidth=1)+
  geom_line(data=cum.sx.catch,aes(y=cum.catch,color="2025 Cumulative Catch"),linewidth=1.5,alpha=.5)+
  scale_color_manual(values=c("black","#E41A1C","#377EB8","#4DAF4A"))+
  labs(y="Total Allowable Catch\nCatch to Date")+
  ylim(0,2000000)+
  xlim(as.Date("2025-06-10"),as.Date("2025-09-01"))+
  theme_bw()+
  theme(legend.position="bottom")
}

make.tacandtotalcatch.plot(catch.gn,catch.sn,tac.data)

# plot 2: 
# 
# make_p2 <- function(catch.gn, catch.sn) {
#   
#   cum.sx.catch<-rbind(catch.gn,catch.sn)%>%
#     select(Date,Gear,Catch=`Sockeye (Kept)`)%>%
#     group_by(Date)%>%
#     summarise(total.catch=sum(Catch,na.rm=TRUE))%>%
#     replace(is.na(.), 0)%>%
#     mutate(cum.catch=cumsum(total.catch))%>%
#     filter(Date<tyee.day)
#   
# }

make_p3 <- function(catch.gn, catch.sn) {
  
  catch <- rbind(catch.gn, catch.sn) %>%
    select(Date,Gear,Catch=`Sockeye (Kept)`)%>%
    group_by(Date)
  
    ggplot(catch,aes(x=Date,y=Catch,fill=Gear))+
    geom_col()+
    scale_fill_brewer(palette="Set1")+
    theme_bw()+
    theme(legend.position="top")+
    expand_limits(x=c(as.Date("2025-07-01"),as.Date("2025-09-01")))
  
}

make_p3(catch.gn, catch.sn)

make_p4 <- function(catch.gn, catch.sn) {
  cpue <- rbind(catch.gn, catch.sn) %>%
    select(Date,Gear,Effort,Catch=`Sockeye (Kept)`)%>%
    mutate(
      gear_prefix = case_when(
        Gear == "Gillnet" ~ "gn",
        Gear == "Seine" ~ "sn")) %>%
    pivot_wider(
      names_from = gear_prefix,
      values_from = c(Effort, Catch),
      names_glue = "{gear_prefix}{.value}") %>%
    mutate(Gillnet=gnCatch/gnEffort,Seine=snCatch/snEffort)%>%
    select(Date,Gillnet,Seine)%>%
    pivot_longer(2:3,names_to="Gear",values_to="CPUE")%>%
    filter(Date>"2025-07-01"&Date<"2025-09-01")
  
    ggplot(cpue,aes(x=Date,y=CPUE,color=Gear))+
    geom_line()+geom_point()+
    scale_color_brewer(palette="Set1")+
    theme_bw()+
    theme(legend.position = "top")+
    labs(y="Sockeye CPUE")+
    expand_limits(y=0)+
    expand_limits(x=c(as.Date("2025-07-01"),as.Date("2025-09-01")))
    
}

make_p4(catch.gn, catch.sn)
