#daily and cumulative escapement for 2021-2025
# highlight 2025 in black

tyee.sx.daily <- fread("data/current_year/tyee data 2025.csv") %>%
  select(Date,"2025"=esctyee)

recent.sx.esc <- fread("data/common/tyee_daily_sockeye_escapement_1970-2024.csv") 

all.sx.data <- left_join(recent.sx.esc, tyee.sx.daily, by="Date") %>%
  mutate(Date=as.Date(Date))

# daily escapement
sx.daily.recent <- all.sx.data %>%
  pivot_longer("1970":"2025",names_to="Year",values_to="Fish") %>%
  mutate(Year=as.numeric(Year)) %>%
  mutate(Index=replace_na(Fish,0)) %>%
  filter(Year %in% c("2021", "2022", "2023", "2024", "2025"))

# cumulative escapement
sx.cumesc <- all.sx.data %>%
  pivot_longer("1970":"2025",names_to="Year",values_to="Fish") %>%
  group_by(Year)%>%
  mutate(Year=as.numeric(Year)) %>%
  mutate(Index=replace_na(Fish,0)) %>%
  filter(Year %in% c("2021", "2022", "2023", "2024", "2025")) %>%
  mutate(cum_sum=cumsum(replace_na(Fish, 0)))

# daily esc 2021-2025 plot
sx.daily.recent %>%
  mutate(Year = as.factor(Year)) %>%
  ggplot(aes(x=Date, y=Fish,group=Year, colour = Year)) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()

# daily cumulative esc 2021-2025 plot
sx.cumesc %>%
  mutate(Year = as.factor(Year)) %>%
  ggplot(aes(x=Date, y=cum_sum,group=Year, colour = Year)) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()





