library(ggplot2)
library(tidyverse)
library(pastecs)
library(lubridate)
library(randomForest)
library(ROCR)


#dataset for logistic regression

raw <- read.csv("init_clean_training.csv")
attach(raw)
raw <- raw[is.na(bounces)==T,]

keeps <- c("X","channelGrouping","date","fullVisitorId","visitNumber","visitStartTime","browser",
                   "operatingSystem","isMobile","deviceCategory","continent","subContinent","country",
                   "hits","pageviews","bounces","newVisits","transactionRevenue","medium",
                   "isTrueDirect")

raw <- raw[keeps]

#modifying the variables

raw <- raw %>% mutate_at(vars(hits:transactionRevenue, visitNumber, fullVisitorId), function(x) as.numeric(x))

raw <- raw %>% mutate_at(vars(bounces, newVisits), function(x) ifelse(is.na(x) == T,0,1))

raw <- raw %>% mutate(transactionRevenue = ifelse(is.na(transactionRevenue) == T,0,transactionRevenue))

raw$logRevenue <- log(raw$transactionRevenue + 1)

raw$isTrueDirect <- ifelse(is.na(raw$isTrueDirect) == T,0,1)

#transaction yes/no variable

raw$is.transaction <- ifelse(raw$transactionRevenue == 0,0,1)

#transaction count

temp <- raw %>% select(c("fullVisitorId", "visitNumber", "is.transaction")) %>% 
        arrange(fullVisitorId, visitNumber)

count <- 0
for(index in 1:nrow(temp)){
        temp$trncount[index] <- count
        if(index + 1 > nrow(temp)) break()
        
        if(temp$fullVisitorId[index] == temp$fullVisitorId[index+1] && temp$is.transaction[index] == 1) count <- count + 1
        if(temp$fullVisitorId[index] != temp$fullVisitorId[index+1]) count <- 0
}

#adding transacount to main dataset

raw <- raw %>% arrange(fullVisitorId, visitNumber) %>% mutate(trncount = temp$trncount)


#adding hours and months

raw$time <- as.POSIXct(raw$visitStartTime,origin = "1970-01-01")
raw$timeCST <- with_tz(raw$time,"America/Chicago")
raw$hour <- hour(raw$timeCST)
raw$peakhour <- if_else(raw$hour > 7 & raw$hour < 21, 1, 0)
raw$USA <- ifelse(raw$country == "United States",1,0)

#creating day week quarter and year variables

raw <- raw %>% mutate(day = weekdays(ymd(date)), year = year(ymd(date)),month = months(ymd(date)))

#adding square terms for pageview and trncount

raw <- raw %>% mutate(trncount_sqr = (trncount)^2, pageviews_sqr = (pageviews)^2, is.weekend = ifelse(day %in% c("Saturday","Sunday"),1,0))

#writing to csv

write.csv(raw, "relevant_data.csv")
