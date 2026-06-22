
# historical time series for cumulative index by date ith 10yr rolling average

daily<-fread("data/common/tyee_daily_indices_sockeye_1956-2025.csv")

current<-fread("data/current_year/tyee data 2026.csv") %>%
  mutate(Date = as.IDate(Date))%>%
  select(Date,"2026"=sockeye)

daily.index<-left_join(daily,current,by="Date")%>%
  mutate(Date=as.Date(Date))

long_dat <- daily.index %>%
  pivot_longer(cols = matches("^\\d{4}$"),
    names_to = "Year",
    values_to = "Index") %>%
  mutate(Year = as.numeric(Year),Index = as.numeric(Index)) %>%
  arrange(Year, MONTH, DAY) %>%
  group_by(Year) %>%
  mutate(Cumulative_Index = cumsum(replace_na(Index, 0))) %>%
  ungroup()

plot_data <- long_dat %>%
  filter(MONTH == 6, DAY == 20)

plot_data <- plot_data %>%
  arrange(Year) %>%
  mutate(rolling_10yr = rollmean(Cumulative_Index, k = 10,align = "right",fill = NA))

ggplot(plot_data, aes(x = Year)) +
  geom_col(aes(y = Cumulative_Index),
           fill = "steelblue") +
  geom_line(aes(y = rolling_10yr),
            colour = "red",
            linewidth = 1.2) +
  theme_bw() +
  scale_x_continuous(breaks=seq(1950, 2026, by = 5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Year", y = paste("Cumulative Index through", tyee.date))


