library(ggplot2)
library(tidyverse)
library(lubridate)

#dataset for logistic regression

logistic <- read.csv("init_clean_training.csv")

#reformating the variables and log transformation log transaction revenue

logistic <- logistic %>% mutate_at(vars(hits:transactionRevenue, visitNumber, fullVisitorId), 
                                   function(x) as.numeric(x)) #converting variables to numeric format

logistic <- logistic %>% mutate_at(vars(bounces, newVisits, isTrueDirect), 
                                   function(x) ifelse(is.na(x) == T,0,1)) #creating dummy variables

logistic <- logistic %>% mutate(transactionRevenue = 
                                        ifelse(is.na(transactionRevenue) == T,0,transactionRevenue)) #converting NAs to 0s

logistic$logRevenue <- log(logistic$transactionRevenue + 1) #log transformation of transaction revenue

#dummy variable to indicate if a transaction has taken place

logistic$is.transaction <- ifelse(logistic$transactionRevenue == 0,0,1)

######################################### creating variable for previous transaction count #############################

temp <- logistic %>% select(c("fullVisitorId", "visitNumber", "is.transaction")) %>% 
        arrange(fullVisitorId, visitNumber)

count <- 0
for(index in 1:nrow(temp)){
        temp$prevtrncount[index] <- count
        if(index + 1 > nrow(temp)) break()
        
        if(temp$fullVisitorId[index] == temp$fullVisitorId[index+1] && temp$is.transaction[index] == 1) count <- count + 1
        if(temp$fullVisitorId[index] != temp$fullVisitorId[index+1]) count <- 0
}

#adding transacount to main dataset

logistic <- logistic %>% arrange(fullVisitorId, visitNumber) %>% mutate(prevtrncount = temp$prevtrncount)

#----------------------------------------------------------------------------------------------------------------------------

#distribution of paying customers by continent and country

logistic %>% group_by(continent) %>% summarise(sum(is.transaction))

logistic %>% group_by(country) %>% summarise(sum(is.transaction)) %>% arrange(desc(`sum(is.transaction)`))

#trncount per medium

logistic%>% group_by(medium) %>% summarise(length(pageviews))

#creating dummy variable for USA

logistic$is.USA <- ifelse(logistic$country == "United States",1,0)

#creating day week month and year variables

logistic <- logistic %>% mutate(day = weekdays(ymd(date)), year = year(ymd(date)),month = months(ymd(date)))

#adding holiday, weekend and square terms for pageview, trncount and visitnumber 

logistic <- logistic %>% mutate(prevtrncount_sqr = (prevtrncount)^2, pageviews_sqr = (pageviews)^2, is.weekend = ifelse(day %in% c("Saturday","Sunday"),1,0)
                                , visitNumbersqr = visitNumber^2, is.holiday = ifelse(month == "December",1,0))

logistic <- logistic %>% filter(medium != "(not set)")

#creating variable for peak hour

logistic$time <- as.POSIXct(logistic$visitStartTime,origin = "1970-01-01")
logistic$timeCST <- with_tz(logistic$time,"America/Chicago")
logistic$hour <- hour(logistic$timeCST)
logistic$peakhour <- if_else(logistic$hour > 7 & logistic$hour < 21, 1, 0)
table(logistic$peak)

######################################## logistic regression model FOR usa ###############################################

USA_data <- logistic %>% filter(bounces == 0, !is.na(pageviews) == T, is.USA == 1)

USA_reg <- glm(is.transaction ~ prevtrncount + prevtrncount_sqr + pageviews + pageviews_sqr + visitNumber + visitNumbersqr + as.factor(peakhour) +
                                as.factor(newVisits) + as.factor(isMobile) + as.factor(medium) + as.factor(month) + as.factor(is.weekend) +
                                as.factor(newVisits) * pageviews +
                                as.factor(medium) * prevtrncount
                       
               ,data = USA_data, family = binomial(link = "logit"))

summary(USA_reg)

#prediction and confusion matric for USA data

pred.logit_USA <- predict.glm(USA_reg, USA_data, type = "response")

table(USA_data$is.transaction, pred.logit_USA > 0.5)

########################################logistic regression for non-usa countries#############################################

non_USA_data <- logistic %>% filter(bounces == 0, !is.na(pageviews) == T, is.USA == 0)

non_USA_reg <- glm(is.transaction ~ prevtrncount + prevtrncount_sqr + pageviews + pageviews_sqr +
                       as.factor(newVisits) + as.factor(isMobile) +
                       as.factor(isTrueDirect) +
                       as.factor(newVisits) * pageviews
               
               ,data = non_USA_data, family = binomial(link = "logit"))

summary(non_USA_reg)

#prediction and confusion matrix for non_USA data

pred.logit_non_USA <- predict.glm(non_USA_reg, non_USA_data, type = "response")

table(non_USA_data$is.transaction, pred.logit_non_USA > 0.5)
