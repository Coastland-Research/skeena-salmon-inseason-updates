
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
    annotate("text", x = today_count, y = Inf,label = pred_label,hjust = -0.05,vjust = 1.2, colour = "red", size = 3.5) +
    labs(x = "Cumulative fence count to date", y = "Final seasonal fence count") +
    theme_bw()
}


make.babine.linear.date.plot <- function(data) {
  ggplot(data, aes(x = Date)) +
    geom_ribbon(aes(ymin = lwr90, ymax = upr90),fill = "grey85") +
    geom_line(aes(y = prediction),linewidth = 1.2,colour = "black") +
    # Actual cumulative 2026 count
    geom_line(aes(y = daily_cum),linewidth = 1.2,colour = "blue") +
    geom_vline(xintercept = fence.day, linetype = "dashed", colour = "red") +
    labs(x = "Date", y = "Babine fence count") +
    theme_bw()
}








# make.babine.linear.date.plot <- function(data) {
#   ggplot(data, aes(x = Date)) +
#     geom_ribbon(aes(ymin = lwr10, ymax = upr90),fill = "grey85") +
#     geom_ribbon(aes(ymin = lwr25, ymax = upr75),fill = "grey60") +
#     # Predicted final seasonal count
#     geom_line(aes(y = prediction, colour = "Predicted final count"),linewidth = 1.2) +
#     # Actual cumulative 2026 count
#     geom_line(aes(y = daily_cum, colour = "Actual cumulative count"),linewidth = 1.2) +
#     # Today
#     geom_vline(xintercept = fence.day,
#       linetype = "dashed",colour = "red") +
#     geom_point(data = data %>% filter(Date == fence.day),
#       aes(y = prediction),size = 3,colour = "black") + # prediction
#     annotate("text", x = fence.day,y = Inf,
#       label = pred_label,hjust = 1.05,vjust = 1.2,colour = "red",size = 3.5) +
#     scale_colour_manual(values = c(
#         "Predicted final count" = "black",
#         "Actual cumulative count" = "blue")) +
#     labs(x = "Date",y = "Fence count",colour = NULL) +
#     theme_bw() +
#     theme(legend.position = "bottom")
# }