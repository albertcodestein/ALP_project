library(ggplot2)
library(tidyverse)
library(pastecs)
library(lubridate)

#dataset for linear regression

lreg <- temp %>% filter(is.transaction == 1) %>% mutate(is.weekend = ifelse(day %in% c("Saturday","Sunday"),1,0))

#linear regression model

regcoef2 <- lm(logRevenue ~ pageviews + pageviews_sqr + log(visitNumber) + as.factor(medium) + as.factor(is.weekend), 
               data = lreg)

summary(regcoef2)

pred.revenue <- predict.lm(regcoef2, temp)

exppred <- pred.revenue * pred.logit

RMSE <- sqrt(mean((exppred - temp$logRevenue)^2))