
creel_catch <- read.csv("data/current_year/recreational creel/rec_creel_catch.csv") %>%
  mutate(Month = factor(Month, levels = c("May", "June", "July", "August")))%>%
  rename(Estimate = Value)
         
creel_effort <- read.csv("data/current_year/recreational creel/rec_creel_effort.csv")

effort_long <- creel_effort %>%
  mutate(Disposition = "Effort",
         Species = NA_character_) %>%
  rename(Estimate = Effort) %>%
  select(Year, Species, Month, Disposition, Estimate)

species <- unique(creel_catch$Species)

effort_long <- tidyr::crossing(
  Species = species,
  effort_long %>% select(-Species))

combined_creel <- bind_rows(creel_catch, effort_long)

(chinook_creel <- combined_creel %>%
    mutate(Month = forcats::fct_rev(Month)) %>%
  filter(Species == "Chinook") %>%
  ggplot(aes(x = Year, y = Estimate, fill = Month))+
  geom_col()+
  scale_fill_brewer(palette = "Spectral")+
  ggtitle("Chinook Salmon")+
  facet_grid(Month~Disposition)+
  theme_minimal()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))+
  scale_x_continuous(breaks = seq(2015, 2026, by = 1)))

(coho_creel <- combined_creel %>%
    mutate(Month = forcats::fct_rev(Month)) %>%
  filter(Species == "Coho") %>%
  ggplot(aes(x = Year, y = Estimate, fill = Month))+
  scale_fill_brewer(palette = "Spectral")+
  geom_col()+
  ggtitle("Coho Salmon")+
  facet_grid(Month~Disposition)+
  theme_minimal()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))+
  scale_x_continuous(breaks = seq(2015, 2026, by = 1)))

(halibut_creel <- combined_creel %>%
    mutate(Month = forcats::fct_rev(Month)) %>%
  filter(Species == "Pacific Halibut") %>%
  ggplot(aes(x = Year, y = Estimate, fill = Month))+
  geom_col()+
  scale_fill_brewer(palette = "Spectral")+
  ggtitle("Pacific Halibut")+
  facet_grid(Month~Disposition)+
  theme_minimal()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))+
  scale_x_continuous(breaks = seq(2015, 2026, by = 1)))
