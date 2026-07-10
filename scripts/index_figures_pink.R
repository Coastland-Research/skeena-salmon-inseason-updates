#create index figures with a filter for species

#daily and cumulative index plots
#functions to make daily and cumulative index plots for pink salmon


make.daily.index.plot.pink<-function(daily.data,daily.quants,xhigh,yhigh) {
  
ggplot(daily.data,aes(x=Date, y=Fish,group=Date))+
    geom_line(data=daily.quants,aes(colour=qgroup,group=Q),linetype="longdash",linewidth=1)+
    geom_line(data = daily.data %>% filter(Year == 2026),
              aes(x = Date, y = Fish, group = 1, color = "2026 Data"),
              linewidth = 1.5,alpha=.7)+
    geom_line(data = daily.data %>% filter(Year <2026), aes(x = Date, y = Fish,group=Year),
              linewidth = .5,alpha=.1)+
    scale_color_manual(values=c("grey75","purple","grey50","black"))+
    labs(y="Daily Index",color="")+
    theme_bw()+
    theme(legend.position="bottom",axis.title.x=element_blank())+
    ylim(0,yhigh)+
    xlim(as.Date("2026-06-03"),xhigh)

}

make.cum.index.plot.pink <-function(cum.data,cum.quants,xhigh,y.cum.high){
  
  ggplot(cum.data,aes(x=Date, y=cum_sum,group=Date))+
    geom_line(data=cum.quants,aes(colour=qgroup,group=Q),linetype="longdash",linewidth=1)+
    geom_line(data = cum.data %>% filter(Year == 2026&Date<=tyee.day),
              aes(x = Date, y = cum_sum, group = 1,color = "2026 Data"),
              linewidth = 1.5,alpha=.7)+
    geom_line(data = cum.data %>% filter(Year <2026), aes(x = Date, y = cum_sum,group=Year),
              linewidth = .5,alpha=.1)+
    scale_color_manual(values=c("grey75","purple","grey50","black"))+
    labs(y="Cumulative Daily Index",color="")+
    theme_bw()+
    theme(legend.position = "bottom")+
    xlim(as.Date("2026-06-03"),xhigh)+
    ylim(0,y.cum.high)
  
}

# 
#daily<-fread("data/common/tyee_daily_indices_pink_even_1956-2024.csv", header = TRUE) %>%
#  mutate(Date = as.IDate(Date))%>%
#  select(-"month-day",-DAY,-MONTH)

#current<-fread("data/current_year/tyee data 2026.csv") %>%
#  select(Date,"2026"=`pink`) 

make.index.figures.pink <-function(daily,current,yhigh,y.cum.high,xhigh) {
  

  daily.index<-left_join(daily,current,by="Date")%>%
    mutate(Date=as.Date(Date))
  
  gg.daily<-daily.index%>%
    mutate_if(is.character, as.numeric) %>%
    pivot_longer(`1956`:`2026`,names_to="Year",values_to="Fish") %>%
    mutate(Year=as.numeric(Year)) %>%
    mutate(Index=replace_na(Fish,0))
  
  gg.daily.quants<-gg.daily %>%
    group_by(Date) %>%
    summarise(per10 = quantile(Fish,.1, na.rm = TRUE), 
              per25 = quantile(Fish,.25, na.rm = TRUE), 
              per50 = quantile(Fish,.50, na.rm = TRUE),
              per75 = quantile(Fish,.75, na.rm = TRUE),
              per90 = quantile(Fish,.9, na.rm = TRUE))%>%
    select(Date,per10,per25,per50,per75,per90)%>%
    pivot_longer("per10":"per90",names_to="Q",values_to="Index")%>%
    group_by(Q)%>%
    mutate(Fish=(Index))%>%
    mutate(qgroup=case_when(Q=="per10"|Q=="per90"~"10/90th",
                            Q=="per25"|Q=="per75"~"25/75th",
                            Q=="per50"~"Median"))
  
  years_out<-c("2008","2009","2010")
  
  gg.daily<-gg.daily%>%filter(!Year%in%years_out)
  
  gg.daily.cum<-daily.index%>%
    pivot_longer(`1956`:`2026`,names_to="Year",values_to="Fish") %>%
    group_by(Year)%>%
    mutate(cum_sum=cumsum(replace_na(Fish,0)))
  
  gg.daily.cum.quants<-gg.daily.cum %>%
    group_by(Date) %>%
    summarise(per10 = quantile(cum_sum,.1, na.rm = TRUE), 
              per25 = quantile(cum_sum,.25, na.rm = TRUE), 
              per50 = quantile(cum_sum,.50, na.rm = TRUE),
              per75 = quantile(cum_sum,.75, na.rm = TRUE),
              per90 = quantile(cum_sum,.9, na.rm = TRUE))%>%
    select(Date,per10,per25,per50,per75,per90)%>%
    pivot_longer("per10":"per90",names_to="Q",values_to="Index")%>%
    group_by(Q)%>%
    mutate(cum_sum=cumsum(Index))%>%
    mutate(qgroup=case_when(Q=="per10"|Q=="per90"~"10/90th",
                            Q=="per25"|Q=="per75"~"25/75th",
                            Q=="per50"~"Median"))
  
  years_out<-c("2008","2009","2010")
  
  gg.daily.cum<-gg.daily.cum%>%filter(!Year%in%years_out)

  daily.index.plot<-make.daily.index.plot.pink(gg.daily,gg.daily.quants,xhigh,yhigh)
  
  
  cum.index.plot<-make.cum.index.plot.pink(gg.daily.cum,gg.daily.cum.quants,xhigh,y.cum.high)
  
  #Daily plot with 2026 and quants
  ggarrange(daily.index.plot,cum.index.plot,align="v",ncol=1,common.legend = TRUE,legend="bottom")
  
}
