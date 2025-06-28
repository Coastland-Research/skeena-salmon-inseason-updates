
# 2025 tyee data
data<-read_csv("data/current_year/Tyee data 2025.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(cumtyee = cumsum(replace_na(esctyee,0)))

# all years tyee data (to 2024):
daily <- read_excel("data/2024_archive/Tyee data 2024.xlsx",sheet="tyee daily") %>%
  pivot_longer("1970":"2024",names_to="Year",values_to="Sockeye") %>%
  mutate(Year=as.numeric(Year)) %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(Year)%>%
  mutate(cum_sum=cumsum(replace_na(Sockeye,0)))

# 2025 daily escapement versus 2000-2004
daily_tyee_years <- daily %>%
  filter(Year >= 2000) %>%
  ggplot(aes(x = Date, y = Sockeye, color = "daily sockeye escapement")) + geom_line() + 
  geom_line(data = data%>%filter(Date<=tyee.day), aes(x = Date, y = esctyee, color = "2025 daily sockeye escapement")) + # line for 2025 data
  facet_wrap(~Year, scales = "free_y") +
  theme_minimal()+
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Set1", name = "") +
  ylab("Sockeye Escapement")

# 2025 cumulative escapement versus 2000-2024
cum_tyee_years <- daily %>%
  filter(Year >= 2000) %>%
  ggplot(aes(x = Date, y = cum_sum, color = "cumulative sockeye escapement")) + geom_line() +
  geom_line(data = data%>%filter(Date<=tyee.day), aes(x = Date, y = cumtyee, color = "2025 cumulative sockeye escapement")) +
  facet_wrap(~Year,scales="free_y") +
  theme_minimal()+
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Set1", name = "")+
  ylab("Cumulative Sockeye Escapement")