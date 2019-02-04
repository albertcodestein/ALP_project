#loading all necessary libraries
library(stringr)
library(tidyverse)
library(jsonlite)
library(rowr)
library(lubridate)
library(dplyr)

#download data
getwd()
setwd("C:/Users/Bill/Documents/ALP")
data<-read.csv("train_forclass.csv", stringsAsFactors = F)
View(data)

#making function that will convert the necessary columns in a JSON format columns
ParseJSONColumn <- function(x)  {
  str_c("[ ", str_c(x, collapse = ",", sep=" "), " ]")  %>% 
    fromJSON(flatten = T) %>% 
    as.tibble()
}

#converting data using function and turning data into DF
clean_data <- data  %>% 
  select(trafficSource, totals, geoNetwork, device)  %>% 
  map_dfc(.f = ParseJSONColumn)

#dropping the converted columns from original data set
drop<-c("trafficSource", "totals", "geoNetwork", "device")
data<-data[,!names(data) %in% drop]

#combining seperated columns with original data to make clean data file
clean_data <- bind_cols(data,clean_data)

#starting to examine which columns to keep and which to remove by looking at unique values
#dropping columns that will have too many unique values
drop2<-c("fullVisitorId", "sessionId","visitId","visitStartTime",
         "networkDomain","keyword", "transactionRevenue","adwordsClickInfo.gclId","keyword")

#creating loop to find unique values and creating dataframe to analyze
df<-data.frame()
var_names <- colnames(clean_data[,!names(clean_data) %in% drop2])
for(i in var_names){
  #x<-unique(clean_data[,i])
  #y<-assign(i,x)
  df<-cbind.fill(df,assign(i,unique(clean_data[,i])),fill=NA)
}

df<-df[,2:48]
colnames(df)<-var_names
View(df)

#converting date column into actual date
clean_data$date<-ymd(clean_data$date)
#converting dates to week 
clean_data$week<-week(clean_data$date)
#converting dates to day of the week
clean_data$dow<-wday(clean_data$date,label=T)



#Working unique values trying to figure out which rows to keep.
length(unique(clean_data$visitId))
length(unique(clean_data$fullVisitorId))
length(unique(clean_data$sessionId))
length(unique(clean_data$keyword))


sum(is.na(clean_data$adwordsClickInfo.adNetworkType))


#looking at VisitNumber and fullVisitId
as.character(clean_data[which(clean_data$visitNumber==312),"fullVisitorId"])
x<-clean_data[which(clean_data$fullVisitorId==1957458976293878016),]
View(x)

#working on this to replace "not provided" selections also testing github
library("dplyr")
clean_data2<-na_if(clean_data,"(not provided)")

#purchase incidence
clean_data$transactionRevenue<-as.integer(clean_data$transactionRevenue)
clean_data$transactionRevenue<-is.na(transactionRevenue)
clean_data$purchase_incd<-0
clean_data$purchase_incd<-ifelse(clean_data$transactionRevenue > 0,1,0)
