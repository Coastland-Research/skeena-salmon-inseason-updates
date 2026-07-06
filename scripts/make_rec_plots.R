
# creel_catch <- read.csv("data/current_year/recreational creel/rec_creel_catch.csv") %>%
#   mutate(Month = factor(Month, levels = c("May", "June", "July", "August")))%>%
#   rename(Estimate = Value)
#          
# creel_effort <- read.csv("data/current_year/recreational creel/rec_creel_effort.csv")
make.creel.plot <- function(creel_catch, creel_effort){
  
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
  
  chinook_creel <- combined_creel %>%
    mutate(Month = forcats::fct_rev(Month)) %>%
  filter(Species == "Chinook") %>%
  ggplot(aes(x = Year, y = Estimate, fill = Month))+
    geom_col(aes(colour = Year == 2026),
             linewidth = 0.6)+
    scale_fill_brewer(palette = "Spectral") +
    scale_colour_manual(
      values = c("FALSE" = NA, "TRUE" = "black"),
      guide = "none")+
  ggtitle("Chinook Salmon")+
  facet_grid(Month~Disposition)+
  theme_minimal()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_text(margin = margin(t = 15)))+
  scale_x_continuous(breaks = seq(2015, 2026, by = 1))
  
  coho_creel <- combined_creel %>%
    mutate(Month = forcats::fct_rev(Month)) %>%
  filter(Species == "Coho") %>%
  ggplot(aes(x = Year, y = Estimate, fill = Month))+
    geom_col(aes(colour = Year == 2026),
             linewidth = 0.6)+
    scale_fill_brewer(palette = "Spectral") +
    scale_colour_manual(
      values = c("FALSE" = NA, "TRUE" = "black"),
      guide = "none")+
  ggtitle("Coho Salmon")+
  facet_grid(Month~Disposition)+
  theme_minimal()+
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.x = element_text(margin = margin(t = 15)))+
    scale_x_continuous(breaks = seq(2015, 2026, by = 1))
  
  halibut_creel <- combined_creel %>%
    mutate(Month = forcats::fct_rev(Month)) %>%
  filter(Species == "Pacific Halibut") %>%
  ggplot(aes(x = Year, y = Estimate, fill = Month))+
  geom_col(aes(colour = Year == 2026),
           linewidth = 0.6)+
    scale_fill_brewer(palette = "Spectral") +
    scale_colour_manual(
      values = c("FALSE" = NA, "TRUE" = "black"),
      guide = "none")+
  ggtitle("Pacific Halibut")+
  facet_grid(Month~Disposition)+
  theme_minimal()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.x = element_text(margin = margin(t = 15)))+  
    scale_x_continuous(breaks = seq(2015, 2026, by = 1))
  
  print(chinook_creel)
  print(coho_creel)
  print(halibut_creel)
}


# Summary plot ------------------------------------------------------------

# Catch (sum across months)
make.creel.summary.plot <- function(creel_catch, creel_effort){
  
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

  catch_summary <- combined_creel %>%
  filter(Disposition != "Effort") %>%
  group_by(Year, Species, Disposition) %>%
  summarise(Estimate = sum(Estimate), .groups = "drop")
  
  effort_summary <- combined_creel %>%
  filter(Disposition == "Effort") %>%
  distinct(Year, Month, Estimate) %>%     # remove duplicated effort rows
  group_by(Year) %>%
  summarise(Estimate = sum(Estimate), .groups = "drop") %>%
  mutate(
    Species = "Effort",
    Disposition = "Effort")
  
  summary_df <- bind_rows(catch_summary, effort_summary)
  
  summary_df$Species <- factor(
  summary_df$Species,
  levels = c("Effort", "Chinook", "Coho", "Pacific Halibut"))
  
  ggplot(summary_df,
       aes(x = Year, y = Estimate, colour = Disposition, group = Disposition)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_grid(Species ~ ., scales = "free_y") +
  scale_colour_manual(
    values = c("Retained" = "#2B83BA",
               "Released" = "#ABDDA4",
               "Effort"    = "#D7191C"))+
  scale_x_continuous(breaks = 2017:2026) +
  labs(x = "Year", y = "Annual estimate", colour = NULL) +
  theme_minimal() +
  theme(strip.text.y = element_text(), legend.position = "top")
}


# CPUE plot ---------------------------------------------------------------

make.creel.cpue.plot <- function(creel_catch, creel_effort){
  
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

  annual_catch <- combined_creel %>%
  filter(Disposition %in% c("Released", "Retained")) %>%
  group_by(Year, Species) %>%
  summarise(Catch = sum(Estimate),
    .groups = "drop")
  
  annual_effort <- combined_creel %>%
  filter(Disposition == "Effort") %>%
  distinct(Year, Month, Estimate) %>%
  group_by(Year) %>%
  summarise(Effort = sum(Estimate), .groups = "drop")
  
  cpue <- annual_catch %>%
  left_join(annual_effort, by = "Year") %>%
  mutate(CPUE = Catch / Effort)
  
  species_cols <- c(
  "Chinook" = brewer.pal(4, "Spectral")[1],       
  "Coho" = brewer.pal(4, "Spectral")[3],
  "Pacific Halibut" = brewer.pal(4, "Spectral")[4])
  
  ggplot(cpue,
       aes(Year, CPUE, colour = Species)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~Species, ncol = 3, scales = "free_y") +
  scale_colour_manual(values = species_cols) +
  scale_x_continuous(breaks = seq(2015, 2026, by = 1)) +
  labs(x = "Year", y = "CPUE (fish per angler-trip)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
}


