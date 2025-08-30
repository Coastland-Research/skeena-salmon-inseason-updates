library(data.table)
library(tidyverse)
library(ggpubr)

options(scipen=1000)

current.index <- 42.44
multiplier <- 245
todate<-as.Date("2025-08-28")
todate.label<-"August 28"

daily <- fread("data/common/tyee_daily_indices_steelhead_1956-2024.csv", header = TRUE)

index_2025 <- fread("data/current_year/tyee data 2025.csv", header = TRUE) %>%
  select(c(Date, steelhead)) %>%
  rename("2025" = steelhead)

# add 2025 index values to common data
data <- left_join(daily, index_2025, by = "Date")

data <- data %>%
  mutate_if(is.character, as.numeric) %>%
  pivot_longer(`1956`:`2025`, names_to="Year", values_to="Index") %>%
  mutate(Year = as.numeric(Year)) %>%
  group_by(Year) %>%
  mutate(Index = replace_na(Index,0),
         CumIndex = cumsum(Index),
         Fish = Index*multiplier,
         CumFish = CumIndex*multiplier,
         FinalIndex = sum(Index,na.rm=TRUE),
         FinalFish = sum(Fish,na.rm=TRUE),
         p = CumIndex/FinalIndex,
         isittoday=ifelse(Date==todate,"today","nottoday"))

# storage dataframe
results <- data.frame(Date=as.Date(character()),
                      med=double(), p25=double(), p75=double(),
                      medFish=double(), p25Fish=double(), p75Fish=double(),
                      r2=double(), pval=double(),
                      lm_med=double(), lm_p25=double(), lm_p75=double(),
                      stringsAsFactors=FALSE)

# Loop across each day starting July 3

dates2025 <- unique(data$Date[format(data$Date, "%Y")=="2025"])
dates2025 <- sort(dates2025)
dates2025 <- dates2025[dates2025 >= as.Date("2025-07-03")]


todate


for (todate in dates2025) {
  
  todays.data <- data %>%
    filter(Date == todate)
  
  # get the cumulative 2025 index up to this date
  current.index <- todays.data %>% 
    filter(Year == 2025) %>% 
    pull(CumIndex)
  
  # proportion model
  med.today <- round(median(todays.data$p, na.rm=TRUE), 5)
  p25 <- round(quantile(todays.data$p, .25, na.rm=TRUE), 5)
  p75 <- round(quantile(todays.data$p, .75, na.rm=TRUE), 5)
  
  med.fish <- round(multiplier * current.index / med.today, 0)
  p25.fish <- round(multiplier * current.index / p25, 0)
  p75.fish <- round(multiplier * current.index / p75, 0)
  
  # lm model
  dat <- data.frame(count = todays.data$CumIndex, final = todays.data$FinalIndex)
  m <- lm(final ~ count, data = dat)
  
  s <- summary(m)
  r2 <- s$r.squared
  pval <- coef(s)[2,4]
  
  # predicted final fish for *current cumulative count*
  currentx <- current.index * multiplier
  pred_current <- predict(
    m, 
    newdata = data.frame(count = currentx),
    interval = "prediction",
    level = 0.75
  )
  
  med.lm <- pred_current[1,"fit"]
  p25.lm <- pred_current[1,"lwr"]
  p75.lm <- pred_current[1,"upr"]
  
  # store results
  results <- rbind(results, data.frame(
    Date = as.Date(todate),
    med = med.today,
    p25 = p25,
    p75 = p75,
    medFish = med.fish,
    p25Fish = p25.fish,
    p75Fish = p75.fish,
    r2 = round(r2, 3),
    pval = round(pval, 5),
    lm_med = round(med.lm, 0),
    lm_p25 = round(p25.lm, 0),
    lm_p75 = round(p75.lm, 0)
  ))
}


#plot
results$Date <- as.Date(results$Date)

# Scale proportion model to run size
# (assuming FinalIndex is the final run size from your dataset)
final_run_size <- max(data$FinalFish, na.rm=TRUE)

plot_data <- results %>%
  mutate(
    med_prop_scaled = medFish,
    p25_prop_scaled = p25Fish,
    p75_prop_scaled = p75Fish,
    med_lm_scaled = lm_med * final_run_size,
    p25_lm_scaled = lm_p25 * final_run_size,
    p75_lm_scaled = lm_p75 * final_run_size
  )


ggplot(plot_data, aes(x=Date)) +
  # # Proportion model (blue)
  geom_ribbon(aes(ymin=p25_prop_scaled, ymax=p75_prop_scaled),
               fill="blue", alpha=0.2) +
  geom_line(aes(y=med_prop_scaled), color="blue", size=1) +

  # Regression model (red)
  geom_ribbon(aes(ymin=lm_p25, ymax=lm_p75),
              fill="red", alpha=0.2) +
  geom_line(aes(y=lm_med), color="red", size=1) +

  labs(title="Run size predictions using proportion model and regression model",
       y="Predicted Run Size",
       caption="Blue = proportion model | Red = lm model") +
  ylim(0,60000)+
  theme_bw()


ggsave("steelhead/model outputs lm and proportion model with uncertainty.png",
       units="in",dpi=300,height=6,width=6)
