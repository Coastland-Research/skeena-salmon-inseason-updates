
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

make.babine.linear.plot <- function(data){
  ggplot(data, aes(cum_count, FinalCount))+
    geom_ribbon(data = pred_df, aes(x = cum_count, ymin = lwr, ymax = upr), inherit.aes = FALSE, alpha = 0.2)+
    geom_line(data = pred_df, aes(y = fit), linewidth = 1.2, colour = "blue")+
    geom_point(size = 2)+
    geom_vline(xintercept = today_count, linetype = "dashed", colour = "red")+
    annotate("text",x = Inf,y = Inf, label = model_label,hjust = 1.1,vjust = 1.2,size = 3.5) + 
    annotate("text", x = today_count, y = today_pred$fit,label = pred_label,hjust = -0.05,vjust = -0.5, colour = "red", size = 3.5) +
    labs(x = "Cumulative fence count to date", y = "Final seasonal fence count") +
    theme_bw()
}

