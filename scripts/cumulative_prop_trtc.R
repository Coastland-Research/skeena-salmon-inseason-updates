
# 2025 update -------------------------------------------------------------

daily_25 <- read.csv("data/common/sk_cumu_prop.csv") %>%
  rename_with(~ sub("^X", "", .x), starts_with("X"))
  
gg.daily.pcum<-daily_25%>%select("Date":"2024")%>%
  pivot_longer("1956":"2024",names_to="Year",values_to="Sockeye")%>%
  mutate(Year=as.numeric(Year), Date = as.Date(Date))

data <- read.csv("data/current_year/tyee data 2025.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(Date = as.Date(gsub("2025", "2024", Date)))

# read in data 
data2<-data.frame(date=data$Date,cumtrtc=data$cum_check)%>%
  mutate(`1M`=cumtrtc/1000000,
         `1.5M`=cumtrtc/1500000,
         `2M`=cumtrtc/2000000,
         `2.5M`=cumtrtc/2500000)%>%
  pivot_longer(`1M`:`2.5M`,names_to="estimate",values_to="p")%>%
  select(-cumtrtc)


make.prop.figure<-function(gg.daily.pcum, data2) {
  
  ggplot(gg.daily.pcum,aes(x=Date,y=Sockeye,group=Date))+
    geom_boxplot(fill="white",color="grey70",alpha=.5)+
    geom_line(data = data2, aes(x=date,y=p,color=estimate,group=estimate),linewidth=1,linewidth=1) +
    geom_segment(aes(x=as.Date("2024-06-01"),y=.5,xend=as.Date("2024-07-24"),yend=.5),color="blue",linewidth=1,linetype="dashed")+
    geom_segment(aes(x=as.Date("2024-07-24"),y=.5,xend=as.Date("2024-07-24"),yend=0),color="blue",arrow = arrow(length=unit(.25, 'cm')),linewidth=1,linetype="dashed")+
    #ylim(0,150000)+
    theme_bw()+
    ylim(0,1)+
    labs(color="Estimate",y="Cumulative Proportion of TRTC")+
    theme(legend.position = "bottom")
  
}

