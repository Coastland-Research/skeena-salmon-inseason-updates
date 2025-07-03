# daily and cumulative escapement for 2021-2025
# highlight 2025 in black

# daily esc 2021-2025 plot
make.recent.esc.plot <- function(sx.daily.recent) {
  
# ggplot(sx.daily.recent, aes(x=Date, y=Fish,group=Year, colour = Year)) +
#   geom_line() +
#   scale_color_brewer(palette = "Dark2") +
#   theme_minimal()
  
  ggplot() +
    geom_line(
      data = filter(sx.daily.recent, Year != 2025),
      aes(x = Date, y = Fish, group = Year, colour = factor(Year)),
      size = 0.5) +
    geom_line(
      data = filter(sx.daily.recent, Year == 2025),
      aes(x = Date, y = Fish, group = Year),
      color = "black", size = 1.2) +
    scale_color_brewer(palette = "Dark2") +
    theme_minimal() +
    labs(colour = "Year") +
    ylab("Daily Sockeye Escapement")
}


# daily cumulative 2021-2025 plot
make.recent.cum.plot <- function(sx.cumesc) {
  
  ggplot() +
    geom_line(
      data = filter(sx.cumesc, Year != 2025),
      aes(x = Date, y=cum_sum,group=Year, colour = factor(Year)),
      size = 0.5) +
    geom_line(
      data = filter(sx.cumesc, Year == 2025),
      aes(x = Date, y = cum_sum, group = Year),
      color = "black", size = 1.2) +
    scale_color_brewer(palette = "Dark2") +
    theme_minimal() +
    labs(colour = "Year") +
    ylab("Cumulative Sockeye Escapement")
  
  # ggplot(sx.cumesc, aes(x=Date, y=cum_sum,group=Year, colour = Year))+
  #   geom_line() +
  #   scale_color_brewer(palette = "Dark2") +
  #   theme_minimal() +
  #   ylab("Cumulative Sockeye Escapement")
}


