
make.daily.esc.plot<-function(daily.data,daily.quants,ylo,yhigh,start.date,end.date) {
  
  ggplot(daily.data,aes(x=Date, y=Fish,group=Date))+
    geom_line(data=daily.quants,aes(colour=qgroup,group=Q),linetype="longdash",linewidth=1)+
    geom_line(data = daily.data %>% filter(Year == 2025), aes(x = Date, y = Fish, group = 1, color = "2025 Data"), linewidth = 1.5,alpha=.7)+
    geom_line(data = daily.data %>% filter(Year <2025), aes(x = Date, y = Fish,group=Year),
              linewidth = .5,alpha=.1)+
    scale_color_manual(values=c("grey75","purple","grey50","black"))+
    labs(y="Daily Sockeye Escapement",color="")+
    theme_bw()+
    theme(legend.position="bottom",axis.title.x=element_blank())+
    ylim(ylo,yhigh)+
    xlim(as.Date(start.date),as.Date(end.date))
}

make.cum.esc.plot<-function(cum.data,cum.quants,yhigh,start.date,end.date){
  
  ggplot(cum.data,aes(x=Date, y=cum_sum,group=Date))+
    geom_line(data=cum.quants,aes(colour=qgroup,group=Q),linetype="longdash",linewidth=1)+
    geom_line(data = cum.data %>% filter(Year == 2025&Date<=tyee.day),
            aes(x = Date, y = cum_sum, group = 1,color = "2025 Data"), linewidth = 1.5,alpha=.7)+
    geom_line(data = cum.data %>% filter(Year <2025), aes(x = Date, y = cum_sum,group=Year),
              linewidth = .5,alpha=.1)+
    scale_color_manual(values=c("grey75","purple","grey50","black"))+
    labs(y="Cumulative Daily Sockeye Escapement",color="")+
    theme_bw()+
    theme(legend.position = "bottom")+
    ylim(0,yhigh)+
    xlim(as.Date(start.date),as.Date(end.date))
  
}

