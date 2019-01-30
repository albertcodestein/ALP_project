library(tidyverse)
library(jsonlite)

#Reading training dataset

training.set <- read.csv("train_forclass.csv")

colnames(training.set)

#Cleaning Json data columns into seperate datafarmes

training.set_device <- 
                paste("[", paste(training.set$device, collapse = ","), "]") %>% fromJSON(flatten = T)
training.set_geo <- 
                paste("[", paste(training.set$geoNetwork, collapse = ","), "]") %>% fromJSON(flatten = T)
training.set_traffic <- 
                paste("[", paste(training.set$trafficSource, collapse = ","), "]") %>% fromJSON(flatten = T)
training.set_totals <- 
                paste("[", paste(training.set$totals, collapse = ","), "]") %>% fromJSON(flatten = T)

#Replacing Json data columns with clean columns

training.set <- training.set %>% select(-c("device", "geoNetwork", "trafficSource", "totals"))
training.set <- cbind(training.set, training.set_device, training.set_geo, training.set_traffic, training.set_totals)

colnames(training.set)
str(training.set)

#writing clean dataset to csv file

write.csv(training.set, "init_clean_training.csv")
