
creel_catch <- read.csv("data/current_year/recreational creel/rec_creel_catch.csv") %>%
  mutate(Month = factor(Month, levels = c("May", "June", "July", "August")))
         
# creel_effort <- read.csv("data/current_year/recreational creel/rec_creel_effort.csv")

view(creel_catch)

creel_catch %>%
  filter(Species == "Chinook") %>%
  ggplot(aes(x = Year, y = Value, fill = Month))+
  geom_col()+
  scale_fill_brewer(palette = "Spectral")+
  ggtitle("Chinook recreational catch estimates")+
  facet_grid(Month~Disposition)+
  theme_minimal()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))+
  scale_x_continuous(breaks = seq(2015, 2026, by = 1))

creel_catch %>%
  filter(Species == "Coho") %>%
  ggplot(aes(x = Year, y = Value, fill = Month))+
  scale_fill_brewer(palette = "Spectral")+
  geom_col()+
  ggtitle("Coho recreational catch estimates")+
  facet_grid(Month~Disposition)+
  theme_minimal()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))+
  scale_x_continuous(breaks = seq(2015, 2026, by = 1))

creel_catch %>%
  filter(Species == "Pacific Halibut") %>%
  ggplot(aes(x = Year, y = Value, fill = Month))+
  geom_col()+
  scale_fill_brewer(palette = "Spectral")+
  ggtitle("Pacific Halibut recreational catch estimates")+
  facet_grid(Month~Disposition)+
  theme_minimal()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))+
  scale_x_continuous(breaks = seq(2015, 2026, by = 1))
