library(ggplot2)
library(tidyverse)
library(pastecs)
library(lubridate)
library(randomForest)
library(ROCR)


#dataset for logistic regression

logistic <- read.csv("init_clean_training.csv")

#modifying the variables

logistic <- logistic %>% mutate_at(vars(hits:transactionRevenue, visitNumber, fullVisitorId), function(x) as.numeric(x))

logistic <- logistic %>% mutate_at(vars(bounces, newVisits), function(x) ifelse(is.na(x) == T,0,1))

logistic <- logistic %>% mutate(transactionRevenue = ifelse(is.na(transactionRevenue) == T,0,transactionRevenue/(10^6)))

logistic$logRevenue <- log(logistic$transactionRevenue + 1)

logistic$isTrueDirect <- ifelse(is.na(logistic$isTrueDirect) == T,0,1)

#transaction yes/no variable

logistic$is.transaction <- ifelse(logistic$transactionRevenue == 0,0,1)

#transaction count

temp <- logistic %>% select(c("fullVisitorId", "visitNumber", "is.transaction")) %>% 
        arrange(fullVisitorId, visitNumber)

count <- 0
for(index in 1:nrow(temp)){
        temp$trncount[index] <- count
        if(index + 1 > nrow(temp)) break()
        
        if(temp$fullVisitorId[index] == temp$fullVisitorId[index+1] && temp$is.transaction[index] == 1) count <- count + 1
        if(temp$fullVisitorId[index] != temp$fullVisitorId[index+1]) count <- 0
}

#adding transacount to main dataset

logistic <- logistic %>% arrange(fullVisitorId, visitNumber) %>% mutate(trncount = temp$trncount)

#distribution of visitore by continent

logistic %>% group_by(continent) %>% summarise(length(visits))

#distribution of paying customers by continent and country

logistic %>% group_by(continent) %>% summarise(sum(is.transaction))

logistic %>% group_by(country) %>% summarise(sum(is.transaction)) %>% arrange(desc(`sum(is.transaction)`))

#trncount per medium

logistic%>% group_by(medium) %>% summarise(length(pageviews))

#creating dummy variable for USA

logistic$is.USA <- ifelse(logistic$country == "United States",1,0)

#creating day week quarter and year variables

logistic <- logistic %>% mutate(day = weekdays(ymd(date)), year = year(ymd(date)),month = months(ymd(date)))

#adding square terms for pageview and trncount

logistic <- logistic %>% mutate(trncount_sqr = (trncount)^2, pageviews_sqr = (pageviews)^2, is.weekend = ifelse(day %in% c("Saturday","Sunday"),1,0))

#logistic regression model

temp <- logistic %>% filter(bounces == 0, medium != "(not set)", !is.na(pageviews) == T)

regcoef <- glm(is.transaction ~ trncount + pageviews + as.factor(newVisits) + as.factor(deviceCategory) + as.factor(medium) +
                       as.factor(is.USA) + as.factor(isTrueDirect) + as.factor(month) + trncount_sqr + pageviews_sqr 
               data = temp, family = binomial(link = "logit"))

summary(regcoef)

pred.logit <- predict.glm(regcoef, newdata = temp, type = "response")

table(temp$is.transaction, pred.logit > 0.035)


#-----------------------------------------------------------------------------------------------------------------------------#
#Adding peak  hour and month variables to the regression
logistic$time <- as.POSIXct(logistic$visitStartTime,origin = "1970-01-01")
logistic$timeCST <- with_tz(logistic$time,"America/Chicago")
logistic$hour <- hour(logistic$timeCST)
logistic$peakhour <- if_else(logistic$hour > 7 & logistic$hour < 21, 1, 0)
table(logistic$peak)



logistic %>% group_by(month) %>% summarise(sum(is.transaction)) %>% arrange(desc(`sum(is.transaction)`))



temp1 <- logistic %>% filter(bounces == 0, medium != "(not set)", !is.na(pageviews) == T)
regcoef1 <- glm(is.transaction ~ trncount + pageviews + as.factor(newVisits) + as.factor(deviceCategory) 
                + as.factor(medium) + as.factor(is.USA) + as.factor(isTrueDirect) + as.factor(month) + 
                trncount_sqr + pageviews_sqr + as.factor(peakhour),
               data = temp1, family = binomial(link = "logit"))

summary(regcoef1)

pred.logit1 <- predict.glm(regcoef1, newdata = temp1, type = "response")

table(temp1$is.transaction, pred.logit1 > 0.035)

#adding month - p value is more than 0.05 but accuracy increases significantly. 
logistic$peakmonth <- as.numeric((if_else(logistic$month %in% c("May", "August", "December"), 1, 0)))


temp2 <- logistic %>% filter(bounces == 0, medium != "(not set)", !is.na(pageviews) == T)
regcoef2 <- glm(is.transaction ~ trncount + pageviews + as.factor(newVisits) + as.factor(deviceCategory) + 
                        as.factor(medium) +as.factor(is.USA) + as.factor(isTrueDirect) + 
                        trncount_sqr + pageviews_sqr + as.factor(peakhour) + as.factor(peakmonth),
                data = temp2, family = binomial(link = "logit"))

summary(regcoef2)

pred.logit2 <- predict.glm(regcoef2, newdata = temp2, type = "response")

table(temp2$is.transaction, pred.logit2 > 0.035)

#-----------------------------------------------------------------------------------------------------------------------------#

