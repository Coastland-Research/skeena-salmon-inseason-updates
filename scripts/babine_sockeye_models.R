#babine fence counts model

library(readxl)
library(tidyverse)
library(data.table)

species.in="Large Sockeye"
historical<-read_excel("data/common/babine fence counts 1946-2025 compiled 20240717.xlsx",
                       sheet=species.in)%>%
  mutate(Date=as.Date(Date))%>%
  mutate_at(vars("1946":"2025"), ~replace(., is.na(.), 0)) %>%
  mutate(Date=as.Date(paste("2026",month(Date),day(Date),sep="-")))

current<-fread("data/current_year/babine fence 2026.csv") %>%
  select(Date,"2026"=species.in)%>%
  mutate(Date=as.Date(paste("2026",month(Date),day(Date),sep="-")))

daily.index<-left_join(historical,current,by="Date")

gg.daily<-daily.index%>%
  mutate_if(is.character, as.numeric) %>%
  pivot_longer(`1946`:`2025`,names_to="Year",values_to="Fish") %>%
  mutate(Year=as.numeric(Year)) %>%
  mutate(Index=replace_na(Fish,0))

gg.daily.quants<-gg.daily %>%
  group_by(Date) %>%
  summarise(per10 = quantile(Fish,.1, na.rm = TRUE), 
            per25 = quantile(Fish,.25, na.rm = TRUE), 
            per50 = quantile(Fish,.50, na.rm = TRUE),
            per75 = quantile(Fish,.75, na.rm = TRUE),
            per90 = quantile(Fish,.9, na.rm = TRUE))%>%
  select(Date,per10,per25,per50,per75,per90)%>%
  pivot_longer("per10":"per90",names_to="Q",values_to="Index")%>%
  group_by(Q)%>%
  mutate(Fish=(Index))%>%
  mutate(qgroup=case_when(Q=="per10"|Q=="per90"~"10/90th",
                          Q=="per25"|Q=="per75"~"25/75th",
                          Q=="per50"~"Median"))

gg.daily.cum<-daily.index%>%
  pivot_longer(`1946`:`2025`,names_to="Year",values_to="Fish") %>%
  mutate(Fish=replace_na(Fish,0))%>%
  group_by(Year)%>%
  mutate(cum_sum=cumsum(Fish))

### Babine sockeye model
historical2 <- historical %>%
  pivot_longer(cols = `1946`:`2025`,names_to = "Year",values_to = "Count") %>%
  mutate(Year = as.numeric(Year)) %>%
  arrange(Year, Date) %>%
  group_by(Year) %>%
  mutate(Count = replace_na(Count, 0),
    cum_fence = cumsum(Count),final_return = max(cum_fence),
    run_prop = cum_fence / final_return) %>%
  ungroup()

babine_timing <- historical2 %>%
  group_by(Date) %>%
  summarise(
    mean_prop = mean(run_prop, na.rm = TRUE),
    p25 = quantile(run_prop, 0.25, na.rm = TRUE),
    p75 = quantile(run_prop, 0.75, na.rm = TRUE),
    .groups = "drop")

babine_forecast <- current %>% 
  left_join(babine_timing, by = "Date") %>%
  mutate(
    daily_count = replace_na(`2026`, 0),
    daily_cum = cumsum(daily_count),
    rtlate = lag(mean_prop, 7),
    rtearly = lead(mean_prop, 7),
    Average = daily_cum / mean_prop,
    Early = daily_cum / rtearly,
    Late = daily_cum / rtlate) %>%
  rename(`2026_count` = `2026`)

babine.model <- babine_forecast %>%
  select(Date, Early, Average, Late) %>%
  pivot_longer(Early:Late,names_to = "Timing",values_to = "Estimate") %>%
  filter(Date <= fence.day)

todays.babine.estimates <- babine_forecast %>%
  filter(Date == fence.day)

cumfencetodate <- babine_forecast %>%
  filter(Date <= fence.day) %>%
  slice_tail(n = 1) %>%
  pull(daily_cum)

early <- round(todays.babine.estimates$Early, 0)
average <- round(todays.babine.estimates$Average, 0)
late <- round(todays.babine.estimates$Late, 0)

rtearly <- paste0(round(todays.babine.estimates$rtearly * 100, 1), "%")
rtaverage <- paste0(round(todays.babine.estimates$mean_prop * 100, 1), "%")
rtlate <- paste0(round(todays.babine.estimates$rtlate * 100, 1), "%")

babine.table <- tibble(
  "Run-timing" = c("Early", "Average", "Late"),
  "Run to Babine to Date" = cumfencetodate,
  "% of Run Through" = c(rtearly, rtaverage, rtlate),
  "Babine Estimate" = c(early, average, late))


# Linear model ------------------------------------------------------------
historical3 <- historical %>%
  pivot_longer(cols = "1946":"2025",
    names_to = "Year",
    values_to = "Count") %>%
  mutate(Year = as.numeric(Year)) %>%
  arrange(Year, Date) %>%
  group_by(Year) %>%
  mutate(cum_count = cumsum(Count))

final_totals <- historical3 %>%
  group_by(Year) %>%
  summarise(FinalCount = max(cum_count))

historical4 <- historical3 %>%
  filter(format(Date, "%m-%d") == format(fence.day, "%m-%d")) %>%
  select(Year, cum_count) %>%
  left_join(final_totals, by = "Year")

fit <- lm(FinalCount ~ cum_count, data = historical4)

today_count <- babine_forecast %>%
  filter(Date == fence.day) %>%
  pull(daily_cum)

newdat <- tibble(cum_count = seq(min(historical4$cum_count), max(historical4$cum_count),
                                 length.out = 200))

pred <- predict(fit, newdata = newdat, interval = "prediction", level= 0.90)
pred_df <- bind_cols(newdat, as.data.frame(pred))

today_pred <- predict(fit, newdata = data.frame(cum_count = today_count),
                      interval = "prediction", level = 0.90)

today_pred <- as.data.frame(today_pred)

modsum <- summary(fit)
r2 <- modsum$r.squared
pval <- coef(modsum)[2,4]

model_label <- paste0("R² = ", round(r2,3),
                      "\nP = ", format(pval, scientific = TRUE, digits = 3))

pred_label <- paste0("Forecast = ", format(round(today_pred$fit,0), big.mark=","),"\n90% PI: ",
  format(round(today_pred$lwr,0), big.mark=",")," - ",format(round(today_pred$upr,0), big.mark=","))


# Linear model by date ----------------------------------------------------
historical3 <- historical %>%
  pivot_longer(cols = "1946":"2025",names_to = "Year",values_to = "Count") %>%
  mutate(Year = as.numeric(Year)) %>%
  arrange(Year, Date) %>%
  group_by(Year) %>%
  mutate(
    Count = replace_na(Count, 0),
    cum_count = cumsum(Count)) %>%
  ungroup()

# Final return for each historical year
final_totals <- historical3 %>%
  group_by(Year) %>%
  summarise(
    FinalCount = max(cum_count),
    .groups = "drop")

# Add final return to each historical daily observation
historical_model_data <- historical3 %>%
  left_join(final_totals, by = "Year")

# Dates over which to generate 2026 predictions
prediction_dates <- babine_forecast %>%
  filter(Date >= as.Date("2026-07-01"), Date <= fence.day) %>%
  select(Date, daily_cum)

# Run a separate linear model for each date
daily_predictions <- lapply(seq_len(nrow(prediction_dates)), function(i) {
  this_date <- prediction_dates$Date[i]
  this_count <- prediction_dates$daily_cum[i]
  historical_day <- historical_model_data %>%
    filter(format(Date, "%m-%d") == format(this_date, "%m-%d"))
  # Fit historical relationship for this date
  fit <- lm(FinalCount ~ cum_count, data = historical_day)
  newdata <- data.frame(cum_count = this_count)
  pred <- predict(fit, newdata = newdata, se.fit = TRUE)
  pred_se <- sqrt(pred$se.fit^2 + summary(fit)$sigma^2)
  t90 <- qt(0.90, df = fit$df.residual)
  t75 <- qt(0.75, df = fit$df.residual)
  tibble(Date = this_date,
    daily_cum = this_count,
    prediction = as.numeric(pred$fit),
    # 10-90% prediction interval
    lwr10 = as.numeric(pred$fit - t90 * pred_se),
    upr90 = as.numeric(pred$fit + t90 * pred_se),
    # 25-75% prediction interval
    lwr25 = as.numeric(pred$fit - t75 * pred_se),
    upr75 = as.numeric(pred$fit + t75 * pred_se),
    r2 = summary(fit)$r.squared,
    pval = coef(summary(fit))[2, 4],
    n = nrow(historical_day))
})
  
daily_predictions <- bind_rows(daily_predictions)


