make.babine.daily.plot<-function(daily.data,daily.quants,babine.figures.x.high,daily.yhigh) {
  
  ggplot(daily.data,aes(x=Date, y=Fish,group=Date))+
    geom_line(data=daily.quants,aes(colour=qgroup,group=Q),linetype="longdash",linewidth=1)+
    geom_line(data = daily.data %>% filter(Year == 2025),
              aes(x = Date, y = Fish, group = 1, color = "2025 Data"),
              linewidth = 1.5,alpha=.7)+
    geom_line(data = daily.data %>% filter(Year <2025), aes(x = Date, y = Fish,group=Year),
              linewidth = .5,alpha=.1)+
    scale_color_manual(values=c("grey75","purple","grey50","black"))+
    labs(y="Daily Count",color="")+
    theme_bw()+
    theme(legend.position="bottom",axis.title.x=element_blank())+
    ylim(0,daily.yhigh)+
    xlim(as.Date("2025-07-01"),babine.figures.x.high)
}

#babine.figures.x.high=as.Date("2025-08-01")
#cum.yhigh=10000

make.babine.cum.plot<-function(gg.daily.cum,gg.daily.cum.quants,babine.figures.x.high,cum.yhigh){
  
  ggplot(gg.daily.cum,aes(x=Date, y=cum_sum,group=Date))+
    geom_line(data=gg.daily.cum.quants,aes(colour=qgroup,group=Q),linetype="longdash",linewidth=1)+
    
    geom_line(data = gg.daily.cum %>% filter(Year == 2025&Date<=tyee.day),
              aes(x = Date, y = cum_sum, group = 1,color = "2025 Data"),
              linewidth = 1.5,alpha=.7)+
    geom_line(data = gg.daily.cum %>% filter(Year <2025), aes(x = Date, y = cum_sum,group=Year),
              linewidth = .5,alpha=.1)+
    scale_color_manual(values=c("grey75","purple","grey50","black"))+
    labs(y="Cumulative Daily Count",color="")+
    theme_bw()+
    theme(legend.position = "bottom")+
    xlim(as.Date("2025-07-01"),babine.figures.x.high)+
    ylim(0,cum.yhigh)
  
}
