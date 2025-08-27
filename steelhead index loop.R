library(data.table)
library(tidyverse)
library(ggpubr)

current.index <- 42.08
multiplier <- 245
todate<-as.Date("2025-08-24")
todate.label<-"August 24"

daily <- fread("data/common/tyee_daily_indices_steelhead_1956-2024.csv", header = TRUE)

data <- daily %>%
  mutate_if(is.character, as.numeric) %>%
  pivot_longer(`1956`:`2024`, names_to="Year", values_to="Index") %>%
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

# Create storage dataframe
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

for (todate in dates2025) {
  
  todays.data <- data %>%
    filter(Date==todate)
  
  if(nrow(todays.data)==0) next
  
  # proportion model
  med.today <- median(todays.data$p, na.rm=TRUE)
  p25 <- quantile(todays.data$p, .25, na.rm=TRUE)
  p75 <- quantile(todays.data$p, .75, na.rm=TRUE)
  
  med.fish <- multiplier*current.index/med.today
  p25.fish <- multiplier*current.index/p25
  p75.fish <- multiplier*current.index/p75
  
  # lm model
  dat <- data.frame(count=todays.data$CumFish, final=todays.data$FinalFish)
  m <- lm(final ~ count, data=dat)
  
  s <- summary(m)
  r2 <- s$r.squared
  pval <- coef(s)[2,4]
  
  # predicted final fish for current count
  currentx <- current.index*multiplier
  pred_current <- predict(
    m, 
    newdata=data.frame(count=currentx),
    interval="prediction",
    level=0.75
  )
  
  med.lm  <- pred_current[1,"fit"]
  p25.lm  <- pred_current[1,"lwr"]
  p75.lm  <- pred_current[1,"upr"]
  
  # store results
  results <- rbind(results, data.frame(
    Date = as.Date(todate),
    med = round(med.today,3),
    p25 = round(p25,3),
    p75 = round(p75,3),
    medFish = round(med.fish,0),
    p25Fish = round(p25.fish,0),
    p75Fish = round(p75.fish,0),
    r2 = round(r2,3),
    pval = round(pval,5),
    lm_med = round(med.lm,0),
    lm_p25 = round(p25.lm,0),
    lm_p75 = round(p75.lm,0)
  ))
}

head(results)

# Plot both models 
# Proportion model results
prop_df <- results %>%
  select(Date, medFish, p25Fish, p75Fish) %>%
  mutate(model="Proportion")

# lm model results
lm_df <- results %>%
  select(Date, lm_med, lm_p25, lm_p75) %>%
  rename(medFish=lm_med, p25Fish=lm_p25, p75Fish=lm_p75) %>%
  mutate(model="Regression")

# Combine into one plotting dataframe
plot_df <- bind_rows(prop_df, lm_df)

# Plot with ribbons
ggplot(plot_df, aes(x=Date, color=model, fill=model)) +
  geom_line(aes(y=medFish), size=1) +
  geom_ribbon(aes(ymin=p25Fish, ymax=p75Fish), alpha=0.2, color=NA) +
  scale_color_manual(values=c("Proportion"="blue","Regression"="red")) +
  scale_fill_manual(values=c("Proportion"="blue","Regression"="red")) +
  labs(y="Estimated Final Escapement",
       x="Date",
       title="Steelhead Run Forecasts (Proportion vs Regression Models)") +
  theme_bw()
