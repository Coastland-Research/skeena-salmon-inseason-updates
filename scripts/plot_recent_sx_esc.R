# daily and cumulative escapement for 2021-2025
# highlight 2025 in black


# 1. Read in data ------------------------------------------------------------

library(RColorBrewer)
tyee.sx.daily <- fread("data/current_year/tyee data 2025.csv") %>%
  select(Date,"2025"=esctyee)

recent.sx.esc <- fread("data/common/tyee_daily_sockeye_escapement_1970-2024.csv") 

all.sx.data <- left_join(recent.sx.esc, tyee.sx.daily, by="Date") %>%
  mutate(Date=as.Date(Date))

# daily escapement
sx.daily.recent <- all.sx.data %>%
  pivot_longer("1970":"2025",names_to="Year",values_to="Fish") %>%
  mutate(Year=as.factor(Year)) %>%
  mutate(Index=replace_na(Fish,0)) %>%
  filter(Year %in% c("2021", "2022", "2023", "2024", "2025"))

# cumulative escapement
sx.cumesc <- all.sx.data %>%
  pivot_longer("1970":"2025",names_to="Year",values_to="Fish") %>%
  group_by(Year)%>%
  mutate(Year=as.factor(Year)) %>%
  mutate(Index=replace_na(Fish,0)) %>% 
  filter(Year %in% c("2021", "2022", "2023", "2024", "2025")) %>%
  mutate(cum_sum=cumsum(replace_na(Fish, 0)))


# 2. Create function for plots --------------------------------------------

# 2a. daily esc 2021-2025 plot
make.recent.esc.plot <- function(sx.daily.recent) {
  
# ggplot(sx.daily.recent, aes(x=Date, y=Fish,group=Year, colour = Year)) +
#   geom_line() +
#   scale_color_brewer(palette = "Dark2") +
#   theme_minimal()

  ggplot() +
    geom_line(
      data = filter(sx.daily.recent, Year != 2025),
      aes(x = Date, y = Fish, group = Year, colour = factor(Year)),
      linewidth = 0.5) +
    geom_line(
      data = filter(sx.daily.recent, Year == 2025),
      aes(x = Date, y = Fish, group = Year,color="2025"),
     linewidth = 1.2) +
    #scale_color_brewer(palette = "Dark2") +
    scale_color_manual(values=c(brewer.pal(4,"Dark2"),"black"))+
    theme_bw() +
    theme(axis.title.x=element_blank())+
    labs(colour = "Year") +
    ylab("Daily Sockeye Escapement")
}

# 2b. daily cumulative 2021-2025 plot
make.recent.cum.plot <- function(sx.cumesc) {
  
  ggplot() +
    geom_line(
      data = filter(sx.cumesc, Year != 2025),
      aes(x = Date, y=cum_sum,group=Year, colour = factor(Year)),
      linewidth = 0.5) +
    geom_line(
      data = filter(sx.cumesc, Year == 2025&Date<=tyee.day),
      aes(x = Date, y = cum_sum, group = Year,color="2025"),
      linewidth= 1.2) +
    #scale_color_brewer(palette = "Dark2") +
    scale_color_manual(values=c(brewer.pal(4,"Dark2"),"black"))+
    theme_bw() +
    labs(colour = "Year") +
    ylab("Cumulative Sockeye Escapement")
  
  # ggplot(sx.cumesc, aes(x=Date, y=cum_sum,group=Year, colour = Year))+
  #   geom_line() +
  #   scale_color_brewer(palette = "Dark2") +
  #   theme_minimal() +
  #   ylab("Cumulative Sockeye Escapement")
}


