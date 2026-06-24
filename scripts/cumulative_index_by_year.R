
# historical time series for cumulative index by date ith 10yr rolling average
make.hist.index.plot <-function(daily,current, figures.x.date) {
  
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
  filter(Date == figures.x.date)
  
  plot_data <- plot_data %>%
  arrange(Year) %>%
  mutate(rolling_10yr = rollmean(lag(Cumulative_Index), k = 10,align = "right",fill = NA))
  
  avg5 <- plot_data %>%
    filter(Year >= max(Year) - 5 & Year < max(Year)) %>%
    summarise(avg = mean(Cumulative_Index)) %>%
    pull(avg)
  
  ggplot(plot_data, aes(x = Year, y = Cumulative_Index, fill = Year == 2026)) +
    geom_col()+
    scale_fill_manual(
      values = c("TRUE" = "purple",
                 "FALSE" = "steelblue"),
      guide = "none")+
    geom_segment(
      aes(x = max(Year) - 5,
        xend = max(Year)-1,
        y = avg5,
        yend = avg5),
      colour = "red",
      linewidth = 0.9)+
  geom_line(aes(y = rolling_10yr),
            colour = "black",
            linewidth = 1.2) +
  theme_bw() +
  scale_x_continuous(breaks=seq(1950, 2026, by = 5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Year", y = paste("Cumulative Index through", tyee.date))
}



