
make.babine.rt.plot <- function(data, xhigh, yhigh) {
  ggplot(data, aes(x = Date, y = Estimate, colour = Timing))+
    geom_line(linewidth = 1.5)+
    scale_color_manual(values = c(Average = "#F8766D",
                                  Early   = "#00BA38",
                                  Late    = "#619CFF"))+
    theme_bw()+
    theme(legend.position = "bottom", legend.title = element_blank())+
    labs(x = "Date", y = "Estimated Final Babine Fence Count")+
    coord_cartesian(xlim = c(min(data$Date), xhigh), ylim = c(0, yhigh))
}