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


keepdata$transactionRevenue[is.na(keepdata$transactionRevenue)]<-0
keepdata$transactionRevenue<-as.numeric(keepdata$transactionRevenue)
keepdata$transaction<-ifelse(keepdata$transactionRevenue>0,1,0)

keepdata$hits <- as.numeric(keepdata$hits)
keepdata$pageviews <- as.numeric(keepdata$pageviews)
keepdata$bounces <- as.numeric(keepdata$bounces)
keepdata$visitNumber <- as.numeric(keepdata$visitNumber)


keepdata$transactionRevenue1 <- keepdata$transactionRevenue+1
keepdata$transactionRevenue1 <- keepdata$transactionRevenue/1000000
keepdata$logRevenue<- as.double(log(keepdata$transactionRevenue1))


write.csv(keepdata, "New_raw_data.csv")