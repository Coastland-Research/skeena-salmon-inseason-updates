
make.trtc.sx.plot<-function(data,yhigh,xhigh) {
  
ggplot(data,aes(x=Date,y=Estimate,color=Timing))+
    geom_hline(yintercept=2400000,linetype="dashed")+
    geom_line(linewidth=1.5)+
    theme_bw()+
    ylim(0,yhigh)+
    theme(legend.position="bottom")+
    labs(y="Sockeye TRTC Estimate at Tyee")+
    xlim(as.Date("2025-06-10"),xhigh)
    
}
