
library(data.table)
library(tidyverse)
library(quantreg)

## 5 yr proportion versus Tyee performance

data<-fread("data/tyee performance vs 5yr.csv")

ggplot(data,aes(x=`5yr`,y=rawperror))+
  geom_point()+
  geom_hline(yintercept=0,color="blue")+
  geom_smooth(method="lm")+
  labs(x="proportion 5-yr old sockeye", y = "tyee raw percent error")+
  theme_bw()

summary( lm(y ~ x) )

#quantile regression
y <- data$rawperror
x <- data$`5yr`

fit <- rq(y ~ x, data=data, tau= 0.5)
summary(fit)

fit_multi <- rq(y ~ x, data = my_data, tau = c(0.1, 0.5, 0.9))
summary(fit_multi)
plot(summary(fit_multi))
