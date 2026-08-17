# Code to run Tyee probability-based escapement model for July 1 - todays date
source("scripts/tyee inseason run prob estimator clean.R")
library(data.table)

current <- fread("data/current_year/tyee data 2026.csv") %>%
  mutate(Date = as.IDate(Date))%>%
  select(Date,"Index"=sockeye)

#### change this to todays date
current_date <- max(
  current$Date[!is.na(index.data)]
)

cdate <- format(
  current_date,
  "%d-%b")

cdate
####

catchabilitymean <- "meanlast10"
catchability <- dftyeeQ$catchability


# Fit logistic distribution to catchability
cfit <- fitdistr(catchability, "logistic")


# Select the location parameter
if (catchabilitymean == "overall") {
  meanc <- cfit$estimate[1]
  } else if (catchabilitymean == "meanlast10") {
    meanc <- mean(
    catchability[16:25],
    na.rm = TRUE)
    } else if (catchabilitymean == "2020") {
      meanc <- 1058
      }


# Create distribution for 1/Q
cdistfull <- rlogis(20000,
  location = meanc,
  scale = cfit$estimate[2])

# Remove negative values
cdistpos <- cdistfull[
  cdistfull > 0]

# Sample from positive distribution
cdist <- sample(cdistpos,
  10000,
  replace = TRUE)

# Set run-timing assumption
# "Average" = use run timing as observed
# "Late"    = shift run timing 7 days earlier

rt <- "Late"

if (rt == "Average") {
  rt.adj <- 0
  } else if (rt == "Late") {
    rt.adj <- -7
    }

# dfRT$rt_date <- as.Date(
#   paste0("2026-", dfRT$Date),
#   format = "%Y-%d-%b")

dfRT$rt_match <- format(
  as.Date(dfRT$Date, format = "%d-%b"),
  "%m-%d")

current$rt_match <- format(
  current$Date,
  "%m-%d")

# Find July 1 in the 2026 current-year data
startrunday <- which(
  format(current$Date, "%m-%d") == "07-01")[1]

# Find the last available Tyee day
endday <- max(which(!is.na(current$Index)))

results <- data.frame(
  Date = current$Date[startrunday:endday],
  median = NA_real_,
  p25 = NA_real_,
  p75 = NA_real_,
  p10 = NA_real_,
  p90 = NA_real_)

for (j in seq_len(nrow(results))) {
  i <- startrunday + j - 1
  
  cum.index <- sum(
    current$Index[1:i],
    na.rm = TRUE)
  
  index_dist <- cum.index * cdist
  
  rt_match <- format(
    results$Date[j],
    "%m-%d")
  
  rt_row <- which(
    dfRT$rt_match == rt_match)
  rt_row <- rt_row+rt.adj
  
  if (
    length(rt_row) == 0 ||
    rt_row < 1 ||
    rt_row > nrow(dfRT)
  ) {
    next
  }
  
  daily <- as.numeric(
    dfRT[
      rt_row,
      18:51])
  
  daily <- daily[
    is.finite(daily) &
      daily > 0]
  
  if (length(daily) < 5) {
    next
  }
  
  dailyfit <- tryCatch(
    fitdistr(
      daily,
      "gamma"),
    error = function(e) NULL)
  if (is.null(dailyfit)) {
    next
  }
  
  RTdist <- rgamma(
    10000,
    shape = dailyfit$estimate["shape"],
    rate = dailyfit$estimate["rate"])
  
  esc_estimate <- index_dist / RTdist
  
  catch <- 0
  
  esc_estimate <- esc_estimate + catch
  
  esc_estimate <- esc_estimate[
    esc_estimate <
      quantile(
        esc_estimate,
        0.99,
        na.rm = TRUE)]
  
  results$median[j] <- median(esc_estimate,na.rm = TRUE)
  
  results$p25[j] <- quantile(esc_estimate,0.25,na.rm = TRUE)
  
  results$p75[j] <- quantile(esc_estimate,0.75,na.rm = TRUE)
  
  results$p10[j] <- quantile(esc_estimate,0.10,na.rm = TRUE)
  
  results$p90[j] <- quantile(esc_estimate,0.90,na.rm = TRUE)
  
}

head(results)
tail(results)

summary(results)

