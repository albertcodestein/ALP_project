library(tidyverse)
library(jsonlite)
library(Hmisc)
library(dplyr)
library(lattice)
library(ggplot2)

#creating data set
a <- read.csv("train_forclass.csv")
colnames(a)
a_device <- paste("[", paste(a$device, collapse = ","), "]") %>% fromJSON(flatten = T)
a_geo <- paste("[", paste(a$geoNetwork, collapse = ","), "]") %>% fromJSON(flatten = T)
a_traffic <- paste("[", paste(a$trafficSource, collapse = ","), "]") %>% fromJSON(flatten = T)
a_totals <- paste("[", paste(a$totals, collapse = ","), "]") %>% fromJSON(flatten = T)
colnames(a)
b <- a[-c(3, 5, 8, 9)]
c <- cbind(a_device, a_geo, a_totals, a_traffic)
d <- cbind(b, c)


#remove and transform variables

keeps = c("channelGrouping","date","fullVisitorId","visitNumber","visitStartTime","browser",
          "operatingSystem","isMobile","deviceCategory","continent","subContinent","country",
          "hits","pageviews","bounces","newVisits","transactionRevenue","campaign","medium",
          "isTrueDirect","adwordsClickInfo.criteriaParameters","adwordsClickInfo.page",
          "adwordsClickInfo.slot","adwordsClickInfo.gclId","adwordsClickInfo.adNetworkType")

keepdata <- d[keeps]

attach(keepdata)

#converting variables to numeric

keepdata <- keepdata %>% mutate_at(vars(hits, pageviews, bounces, fullVisitorId, visitNumber, visitStartTime,
                                        transactionRevenue), function(x) as.numeric(x))

#converting transaction revenue

keepdata <- keepdata %>% mutate(transactionRevenue = ifelse(is.na(transactionRevenue),0,transactionRevenue/10^6))

#creating log revenue

keepdata <- keepdata %>% mutate(LogRevenue = log(transactionRevenue + 1))

#creating IStransaction variable

keepdata <- keepdata %>% mutate(IStransaction = ifelse(transactionRevenue == 0,0,1))

#variable for transaction count

temp <- keepdata %>% select(c("fullVisitorId", "visitNumber", "IStransaction")) %>% 
        arrange(fullVisitorId, visitNumber)

count <- 0
for(index in 1:nrow(temp)){
        temp$trncount[index] <- count
        if(index + 1 > nrow(temp)) break()
        if(temp$fullVisitorId[index] == temp$fullVisitorId[index+1] && temp$IStransaction[index] == 1) count <- count + 1
        if(temp$fullVisitorId[index] != temp$fullVisitorId[index+1]) count <- 0
}

keepdata <- merge(temp[,c("fullVisitorId", "trncount")], keepdata)
as.li

write.csv(keepdata, "New_raw_data.csv")