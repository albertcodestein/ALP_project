library(ggplot2)
library(tidyverse)
library(pastecs)
library(lubridate)
library(randomForest)
library(ROCR)
library(boot)

#dataset for logistic regression

logistic <- read.csv("relevant_data.csv")

#logistic$USA <- ifelse(logistic$country == "United States",1,0)

#creating day week quarter and year variables

#logistic <- logistic %>% mutate(day = weekdays(ymd(date)), year = year(ymd(date)),month = months(ymd(date)))

#adding square terms for pageview and trncount

#logistic <- logistic %>% mutate(trncount_sqr = (trncount)^2, pageviews_sqr = (pageviews)^2, is.weekend = ifelse(day %in% c("Saturday","Sunday"),1,0))

## 70% of the sample size

smp_size <- floor(0.70 * nrow(logistic))

## set the seed to make your partition reproducible

set.seed(123)
train_ind <- sample(seq_len(nrow(logistic)), size = smp_size)
train <- logistic[train_ind, ]
test <- logistic[-train_ind, ]


#build the regression

class_reg <- glm(is.transaction ~ trncount  + trncount_sqr + pageviews + as.factor(newVisits) + isMobile + as.factor(medium) +
                as.factor(USA) + as.factor(isTrueDirect)  + pageviews_sqr + peakhour + as.factor(month), 
                data = train, family = binomial(link = "logit"))

summary(class_reg)

train.class_reg <- predict(class_reg, newdata = train, type = "response")
test.class_reg <- predict(class_reg, newdata = test, type = "response")
table(train$is.transaction, train.class_reg > 0.1) %>% prop.table(1)
table(test$is.transaction, test.class_reg > 0.1) %>% prop.table(1)

