
make.trtc.sx.plot<-function(data) {
  
ggplot(data,aes(x=Date,y=Estimate,color=Timing))+
    geom_hline(yintercept=2400000,linetype="dashed")+
    geom_line(linewidth=1.5)+
    theme_bw()+
    theme(legend.position="bottom")+
    labs(y="Sockeye TRTC Estimate at Tyee")
    
}
