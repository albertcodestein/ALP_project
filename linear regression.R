library(ggplot2)
library(tidyverse)
library(lubridate)


#dataset for linear regression USA

lreg_USA <- logistic %>% filter(bounces == 0, !is.na(pageviews) == T, is.transaction == 1, is.USA == 1)

############################################ linear regression model USA ###################################################

regcoef_USA <- lm(logRevenue ~ pageviews + pageviews_sqr + log(visitNumber) + as.factor(medium) + as.factor(is.weekend) +
                        as.factor(isMobile) + as.factor(is.holiday) + as.factor(isMobile)*as.factor(medium) + as.factor(peakhour), 
               data = lreg_USA)

summary(regcoef_USA)

#----------------------------------------------------------------------------------------------------------------------------

#dataset for linear regression non_USA

lreg_non_USA <- logistic %>% filter(bounces == 0, !is.na(pageviews) == T, is.transaction == 1, is.USA == 0)

############################################ linear regression model non_USA ###################################################

regcoef_non_USA <- lm(logRevenue ~ pageviews + pageviews_sqr + log(visitNumber) + as.factor(is.weekend) +
                          as.factor(isMobile), 
                  data = lreg_non_USA)

summary(regcoef_non_USA)


############################################ revenuew prediction ################################################################


#USA

pred.revenue_USA <- predict.lm(regcoef_USA, USA_data)

exptrn_USA <- pred.revenue_USA * pred.logit_USA

#not USA

pred.revenue_non_USA <- predict.lm(regcoef_non_USA, non_USA_data)

exptrn_non_USA <- pred.revenue_non_USA * pred.logit_non_USA

#RMSE

exptrn_total <- c(exptrn_USA, exptrn_non_USA)
full_data <- rbind.data.frame(USA_data, non_USA_data)

sqrt(mean((exptrn_total - full_data$logRevenue)^2))

#overall aggregated RMSE

aggr_nobounce <- full_data %>% select(fullVisitorId, logRevenue) %>% mutate(predicted_revenue = exptrn_total)

aggr_bounce <- logistic %>% filter(bounces == 1, !is.na(pageviews) == T) %>% select(fullVisitorId, logRevenue) %>% mutate(predicted_revenue = 0)

aggr <- rbind.data.frame(aggr_nobounce, aggr_bounce)

aggr_final <- aggr %>% group_by(fullVisitorId) %>% 
                        summarise(predicted_revenue = sum(predicted_revenue), logRevenue = sum(logRevenue))

sqrt(mean((aggr_final$predicted_revenue - aggr_final$logRevenue )^2))
