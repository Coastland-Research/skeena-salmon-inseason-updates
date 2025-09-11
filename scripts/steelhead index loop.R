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

for (todate in dates2025) {
  
  todays.data <- data %>%
    filter(Date == todate)
  
  # get the cumulative 2025 index up to this date
  current.index <- todays.data %>% 
    filter(Year == 2025) %>% 
    pull(CumIndex)
  
  # proportion model ---------------------------------------
  med.today <- round(median(todays.data$p, na.rm=TRUE), 5)
  p25 <- round(quantile(todays.data$p, .25, na.rm=TRUE), 5)
  p75 <- round(quantile(todays.data$p, .75, na.rm=TRUE), 5)
  
  med.fish <- round(multiplier * current.index / med.today, 0)
  p25.fish <- round(multiplier * current.index / p25, 0)
  p75.fish <- round(multiplier * current.index / p75, 0)
  
  # regression model ---------------------------------------
  dat <- data %>%
    filter(Date == todate, Year < 2025) %>%
    select(CumIndex, FinalIndex)
  
  m <- lm(FinalIndex ~ CumIndex, data = dat)
  s <- summary(m)
  r2 <- s$r.squared
  pval <- coef(s)[2,4]
  
  # prediction for current cumulative index
  pred_current <- predict(
    m, 
    newdata = data.frame(CumIndex = current.index),
    interval = "prediction",
    level = 0.75
  )
  
  med.lm <- pred_current[1,"fit"] * multiplier
  p25.lm <- pred_current[1,"lwr"] * multiplier
  p75.lm <- pred_current[1,"upr"] * multiplier

  newx <- seq(min(dat$CumIndex,na.rm=TRUE),
              max(dat$CumIndex,na.rm=TRUE), by=10)
  pred_interval <- predict(
    m,
    newdata=data.frame(CumIndex=newx),
    interval="prediction",
    level=0.75
  )
  pred_interval <- as.data.frame(pred_interval)
  pred_interval$CumIndex <- newx
  pred_interval$Date <- todate   # tag with current forecast date if storing

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

# plot ------------------------------------------------------
ggplot(results, aes(x=Date)) +
  # proportion model (blue)
  geom_ribbon(aes(ymin=p25Fish, ymax=p75Fish),
              fill="blue", alpha=0.2) +
  geom_line(aes(y=medFish), color="blue", size=1) +
  
  # regression model (red)
  geom_ribbon(aes(ymin=lm_p25, ymax=lm_p75),
              fill="red", alpha=0.2) +
  geom_line(aes(y=lm_med), color="red", size=1) +
  
  labs(title="Run size predictions using proportion model and regression model",
       y="Predicted Run Size (fish)",
       caption="Blue = proportion model | Red = regression model") +
  ylim(0,60000) +
  theme_bw()


ggsave("steelhead/model outputs lm and proportion model with uncertainty.png",
       units="in",dpi=300,height=6,width=6)

