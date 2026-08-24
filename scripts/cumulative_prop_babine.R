# Cumulative average to Babine fence boxplot
source("scripts/babine_sockeye_models.R")

babine_26 <- read.csv("data/common/sk_cumu_prop_babine.csv") %>%
  rename_with(~ sub("^X", "", .x), starts_with("X"))

gg.daily.pcum<-babine_26%>%select("Date":"2025")%>%
  pivot_longer("1956":"2025",names_to="Year",values_to="Sockeye")%>%
  mutate(Year=as.numeric(Year), Date = as.Date(Date))

data <- read.csv("data/current_year/babine fence 2026.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(cum_check = cumsum(`Large.Sockeye`))


median_forecast <- daily_predictions %>%
  filter(Date == fence.day) %>%
  pull(prediction)

data2 <- data %>%
  mutate(
    Date = as.Date(Date),
    cum_prop = cum_check / median_forecast)

make.prop.figure.babine <- function(gg.daily.pcum, data2) {
  ggplot(gg.daily.pcum, aes(x = Date, y = Sockeye, group = Date)) +
    # Historical years
    geom_boxplot(fill = "white", color = "grey70", alpha = 0.5) +
    geom_line(data = data2,aes(x = Date, y = cum_prop, group = 1),
      colour = "blue", linewidth = 1.2, inherit.aes = FALSE)+
    # 2026 observed cumulative proportion
    theme_bw() +
    ylim(0, 1) + 
    xlim(as.Date("2026-06-01"), as.Date("2026-10-01")) +
    labs(x = "Date", y = "Cumulative Proportion of Final Run")}

make.prop.figure.babine(gg.daily.pcum, data2)




# make.prop.figure<-function(gg.daily.pcum, data2) {
#   
#   ggplot(gg.daily.pcum,aes(x=Date,y=Sockeye,group=Date))+
#     geom_boxplot(fill="white",color="grey70",alpha=.5)+
#     geom_line(data = data2, aes(x=date,y=p,color=estimate,group=estimate),linewidth=1) +
#     geom_segment(aes(x=as.Date("2026-06-01"),y=.5,xend=as.Date("2026-07-24"),yend=.5),color="blue",linewidth=1,linetype="dashed")+
#     geom_segment(aes(x=as.Date("2026-07-24"),y=.5,xend=as.Date("2026-07-24"),yend=0),color="blue",arrow = arrow(length=unit(.25, 'cm')),linewidth=1,linetype="dashed")+
#     #ylim(0,150000)+
#     theme_bw()+
#     ylim(0,1)+
#     xlim(as.Date("2026-06-01"),as.Date("2026-10-01"))+
#     labs(color="Estimate",y="Cumulative Proportion of TRTC")+
#     theme(legend.position = "bottom")
#   
# }
# 
# make.prop.figure(gg.daily.pcum, data2)

