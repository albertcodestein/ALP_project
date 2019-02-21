library(ggplot2)
library(tidyverse)
library(pastecs)
library(lubridate)
library(randomForest)
library(ROCR)
library(boot)

#dataset for logistic regression

logistic <- read.csv("relevant_data.csv")
attach(logistic)

## 70% of the sample size

smp_size <- floor(0.70 * nrow(logistic))

## set the seed to make your partition reproducible

set.seed(123)
train_ind <- sample(seq_len(nrow(logistic)), size = smp_size)
train <- logistic[train_ind, ]
test <- logistic[-train_ind, ]


#Build the classifier

class_reg <- glm(is.transaction ~ trncount  + trncount_sqr + pageviews + as.factor(newVisits) + isMobile + as.factor(medium) +
                as.factor(USA) + as.factor(isTrueDirect)  + pageviews_sqr + peakhour + as.factor(month), 
                data = train, family = binomial(link = "logit"))

summary(class_reg)

train.class_reg <- predict(class_reg, newdata = train, type = "response")
test.class_reg <- predict(class_reg, newdata = test, type = "response")

table(train$is.transaction, train.class_reg > 0.05) 
table(test$is.transaction, test.class_reg > 0.05) 

table(train$is.transaction, train.class_reg > 0.05) %>% prop.table(1)
table(test$is.transaction, test.class_reg > 0.05) %>% prop.table(1)

#Linear Regression

linear_reg <- lm(logRevenue ~ trncount + trncount_sqr + pageviews + pageviews_sqr + peakhour + as.factor(newVisits) + isMobile +
                         + as.factor(medium) + as.factor(USA) + as.factor(isTrueDirect)  + as.factor(month), data = train)

train.pred_rev <- predict.lm(linear_reg,newdata = train)
test.pred_rev <- predict.lm(linear_reg, newdata = test)
train.cond_rev <- train.pred_rev*train.class_reg
test.cond_rev <- test.pred_rev*test.class_reg

RMSE_train <- sqrt(mean((train.cond_rev - train$logRevenue)^2, na.rm = T))
RMSE_test <- sqrt(mean((test.cond_rev - test$logRevenue)^2, na.rm = T))

print(RMSE_train)
print(RMSE_test)

