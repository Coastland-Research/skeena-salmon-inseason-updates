
make.recent.esc.plots <- function(recent.daily, recent.cumulative, x.date)

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

sx.esc.daily.recent <- make.recent.esc.plot(sx.daily.recent,0,recent.daily,"2025-06-10",x.date)
sx.cumesc.recent <- make.recent.cum.plot(sx.cumesc, 0, recent.cumulative, "2025-06-10",x.date)
