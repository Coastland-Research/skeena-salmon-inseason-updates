library(data.table)
library(tidyverse)
library(ggpubr)

current.index <- 42.08
multiplier <- 245
todate<-as.Date("2025-08-27")
todate.label<-"August 27"

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

for (todate in dates2025) {
  
  todays.data <- data %>%
    filter(Date==todate)
  
  #if(nrow(todays.data)==0) next
  
  # # proportion model
  # med.today <- median(todays.data$p, na.rm=TRUE)
  # p25 <- quantile(todays.data$p, .25, na.rm=TRUE)
  # p75 <- quantile(todays.data$p, .75, na.rm=TRUE)
  # 
  # med.fish <- multiplier*current.index/med.today
  # p25.fish <- multiplier*current.index/p25
  # p75.fish <- multiplier*current.index/p75
  
  med.today<-round(median(todays.data$p,na.rm=TRUE),5)
  p25<-round(quantile(todays.data$p,.25,na.rm=TRUE),5)
  p75<-round(quantile(todays.data$p,.75,na.rm=TRUE),5)
  
  #estimate number of fish at todates proportions
  med.fish<-round(multiplier*current.index/med.today,0)
  p25.fish<-round(multiplier*current.index/p25,0)
  p75.fish<-round(multiplier*current.index/p75,0)
  
  # lm model
  dat <- data.frame(count=todays.data$CumIndex, final=todays.data$FinalIndex)
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
    med = round(med.today,5),
    p25 = round(p25,5),
    p75 = round(p75,5),
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


#plot
results$Date <- as.Date(results$Date)

# Scale proportion model to run size
# (assuming FinalIndex is the final run size from your dataset)
final_run_size <- max(data$FinalFish, na.rm=TRUE)

plot_data <- results %>%
  mutate(
    med_prop_scaled = med * final_run_size,
    p25_prop_scaled = p25 * final_run_size,
    p75_prop_scaled = p75 * final_run_size,
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
  theme_minimal()
