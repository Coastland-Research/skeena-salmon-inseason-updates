# Code to run Tyee probability-based escapement model for July 1 - todays date
# source("scripts/tyee inseason run prob estimator clean.R")
library(data.table)
library(MASS)
library(tidyverse)

options(scipen=10000)

# historical in-season data
dfindex<-read.csv("data/common/tyee_daily_indices_sockeye_1956-2025.csv", 
                  header = TRUE, check.names = FALSE)

# Q data
dftyeeQ<-read.csv("data/common/tyee Q.csv",header=TRUE,sep=",")

years <- intersect(names(dfindex), as.character(dftyeeQ$Year))

dfindex <- dfindex %>%
  rename_with(~paste0("ind.", .x), all_of(years))

for (yr in years) {
  q <- dftyeeQ$q[dftyeeQ$Year == as.numeric(yr)]
  
  dfindex[[paste0("est.", yr)]] <-
    dfindex[[paste0("ind.", yr)]]/q
}

info_cols <- c("Date", "MONTH", "DAY", "month-day")

year_cols <- as.character(sort(as.numeric(years)))

paired_cols <- unlist(lapply(year_cols, function(y) {
  c(paste0("ind.", y), paste0("est.", y))
}))

dfindex <- dfindex %>%
  select(all_of(info_cols), all_of(paired_cols)) %>%
  mutate(Date = format(as.Date(Date), "%d-%b"))

#read in run-timing from Tyee cumulative percent
dfRT<-read.csv("data/common/tyee cumulative percent 1970-2018.csv",check.names=FALSE,
               header=TRUE,sep=",")

# read current index data
current <- fread("data/current_year/tyee data 2026.csv") %>%
  mutate(Date = as.IDate(Date))%>%
  select(Date,"Index"=sockeye) %>%
  drop_na(Index)

# read in catch data
gillnet <- fread("data/current_year/commercial catch 2026-gillnet.csv") %>%
  select(Date, catch = `Sockeye (Kept)`)

seine <- fread("data/current_year/commercial catch 2026-seine.csv") %>%
  select(Date, catch = `Sockeye (Kept)`)

demo <- fread("data/current_year/fns demo catches 2026.csv") %>%
  select(Date = date, catch = sockeye)

catch_daily <- bind_rows(gillnet, seine, demo) %>%
  group_by(Date) %>%
  summarise(
    catch = sum(catch, na.rm = TRUE),
    .groups = "drop") %>%
  arrange(Date) %>%
  mutate(cum_catch = cumsum(catch))

current_date <- max(current$Date)
cdate <- format(current_date,"%d-%b")

catchabilitymean <- "meanlast10"
catchabilitymean <- "2026"

catchability <- dftyeeQ$catchability

# Fit logistic distribution to catchability
cfit <- fitdistr(catchability, "logistic")

# Select the location parameter
if (catchabilitymean == "overall") {
  meanc <- cfit$estimate[1]
  } else if (catchabilitymean == "meanlast10") {
    meanc <- mean(dftyeeQ$catchability[dftyeeQ$Year >= 2016 & dftyeeQ$Year <= 2025], na.rm = TRUE)
    } else if (catchabilitymean == "2026") {
      meanc <- 1227
      }

# Create distribution for 1/Q
cdistfull <- rlogis(20000, location = meanc, scale = cfit$estimate[2])/0.82

# Remove negative values
cdistpos <- cdistfull[cdistfull > 0]

# Sample from positive distribution
cdist <- sample(cdistpos, 10000, replace = TRUE)

# Set run-timing assumption
rt <- "Average"

if (rt == "Average") {
  rt.adj <- 0
  } else if (rt == "Late") {
    rt.adj <- -7
    }

dfRT$rt_match <- format(as.Date(dfRT$Date, format = "%d-%b"),"%m-%d")
dfRT$rt_date <- format(as.Date(dfRT$Date, format = "%d-%b"))
current$rt_match <- format(current$Date,"%m-%d")

# Find July 1 and last day in the 2026 current-year data
season_start <- min(which(!is.na(current$Index)))
startrunday <- which(format(current$Date, "%m-%d") == "07-01")[1]
endday <- max(which(!is.na(current$Index)))

results <- data.frame(
  Date = current$Date[startrunday:endday],
  median = NA_real_,
  p25 = NA_real_,
  p75 = NA_real_,
  p10 = NA_real_,
  p90 = NA_real_)

results <- results %>%
  left_join(catch_daily, by = "Date") %>%
  mutate(cum_catch = replace_na(cum_catch,0))

for (j in seq_len(nrow(results))) {
  i <- startrunday + j - 1
  
  cum.index <- sum(current$Index[season_start:i],na.rm = TRUE)
  index_dist <- cum.index * cdist
  rt_match <- format(results$Date[j],"%m-%d")
  rt_row <- which(dfRT$rt_match == rt_match)
  rt_row <- rt_row+rt.adj
  
  if (
    length(rt_row) == 0 ||
    rt_row < 1 ||
    rt_row > nrow(dfRT)) {
    next
  }
  
  rt_year_cols <- as.character(1985:2018)
  daily <- as.numeric(dfRT[rt_row,rt_year_cols])
  daily <- daily[is.finite(daily) &daily > 0]
  
  if (length(daily) < 5) {
    next
  }
  
  dailyfit <- tryCatch(fitdistr(daily,"gamma"), error = function(e) NULL)
  if (is.null(dailyfit)) {
    next
  }
  
  RTdist <- rgamma(10000,
    shape = dailyfit$estimate["shape"],
    rate = dailyfit$estimate["rate"])
  
  esc_estimate <- index_dist / RTdist
  
  marine.gillnet<-269625
  marine.seine<-60161
  marine.demo<-20000
  marine.fsc<-30000
  
  # catch <- marine.gillnet + marine.seine + marine.demo + marine.fsc
  # esc_estimate <- esc_estimate + catch
  
  catch_today <- results$cum_catch[j]
  esc_estimate <- esc_estimate + catch_today
  
  esc_estimate <- esc_estimate[
    esc_estimate <
      quantile(esc_estimate, 0.99, na.rm = TRUE)]
  
  results$median[j] <- median(esc_estimate,na.rm = TRUE)
  results$p25[j] <- quantile(esc_estimate,0.25,na.rm = TRUE)
  results$p75[j] <- quantile(esc_estimate,0.75,na.rm = TRUE)
  results$p10[j] <- quantile(esc_estimate,0.10,na.rm = TRUE)
  results$p90[j] <- quantile(esc_estimate,0.90,na.rm = TRUE)
  
}

summary(results)

# Current-date escapement distribution ------------------------------------
# current_i <- endday
# index.data<-dfindex$ind.2025
# 
# # Cumulative index through current date
current_i <- endday
cum.index <- sum(current$Index[season_start:current_i], na.rm = TRUE)

# Q uncertainty
index_dist <- cum.index * cdist

# Find run-timing row for current date
rt_row <- which(dfRT$rt_date == current_date)

# If using "Late" timing, shift 7 days earlier
rt_row <- rt_row + rt.adj

# Historical run-timing observations
daily <- as.numeric(dfRT[rt_row, rt_year_cols])
daily <- daily[is.finite(daily) & daily > 0]

# Fit gamma
dailyfit <- fitdistr(daily,"gamma")

# Simulate run timing
RTdist <- rgamma(10000, shape = dailyfit$estimate[1], rate = dailyfit$estimate[2])

# Calculate escapement distribution
esc.estimate <- index_dist / RTdist

# Add catch
esc.estimate <- esc.estimate + catch

# Remove extreme values
esc.estimate <- esc.estimate[esc.estimate < quantile(esc.estimate, 0.99, na.rm = TRUE)]

p10 <- quantile(esc.estimate,0.10)
p25 <- quantile(esc.estimate,0.25)
p50 <- median(esc.estimate)
p75 <- quantile(esc.estimate,0.75)
p90 <- quantile(esc.estimate,0.90)

#png(file = paste0("Tyee inseason histogram ",cdate,".png"),
#  units = "in",height = 4,width = 6,res = 300)

hist(esc.estimate,breaks = 60,col = "grey80",border = "white",
  main = paste0("Tyee In-Season TRTC Estimate\n","2026 to ",cdate," : ", rt," Timing"),
  xlab = "Number of sockeye",ylab = "Frequency")

abline(v = p50,lwd = 3,lty = 2)
abline(v = c(p10, p90),lwd = 2,lty = 2)
abline(v = c(p25, p75),lwd = 2,lty = 3)

legend("topright", legend = c(paste0("Median = ",round(p50)),
    paste0("P25–P75 = ",round(p25),"–", round(p75)),
    paste0("P10–P90 = ",round(p10),"–", round(p90))),bty = "n")

#dev.off()

# July 1 - Current date time series plot
ggplot(results,aes(x = Date)) +
geom_ribbon(
  aes(ymin = p10,ymax = p90),
  fill = "grey85") +
geom_ribbon(aes(ymin = p25,ymax = p75),fill = "grey60") +
geom_line(aes(y = median),linewidth = 1.2) +
geom_vline(
  xintercept = current_date,
  linetype = "dashed") +
labs(x = "Date",
  y = "Escapement Estimate",
  title = "Tyee In-Season TRTC Estimate",
  subtitle = paste0("July 1 to ",cdate,
    " | ",rt," run timing")) +
  theme_bw()

