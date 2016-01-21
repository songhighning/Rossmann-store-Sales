#Alex's Script on
#Rossman Store Sale Competition
#Date: 2015 12 5
#this requires 
#rossman_v5.r's model saved in Model\\clf51.RDATA

rm(list=ls())

library(readr)
library(randomForest)
library(data.table)
setwd("E:\\Kaggle\\12 14 2015 Rossman Store Sales (HOME)\\")
set.seed(616)
train <- read.csv("train.csv" )
test  <- read.csv("test.csv")
store <- read.csv("store.csv")

train$Date <- as.Date(train$Date)
test$Date <- as.Date(test$Date)

#join store information into the training and test set
train <- merge(train,store)
test <- merge(test,store)

#fill competitionopen since with the 1900 jan
store[which(is.na(store$CompetitionOpenSinceYear)),]$CompetitionOpenSinceYear <- as.numeric(1990)
store[which(is.na(store$CompetitionOpenSinceMonth)),]$CompetitionOpenSinceMonth <- as.numeric(1)

#fill in NA with 1 , since store that are closed do not get penalized for predicted sales
test[is.na(test$Open),]$Open <- 1


#display # of NA per column
sapply(train,function(x) sum(is.na(x)))
apply(is.na(test),2,sum)

## There are some NAs in the integer columns so conversion to zer
train[is.na(train)]   <- 0
test[is.na(test)]   <- 0

# looking at only stores that were open in the train set
# may change this later
train <- train[ which(train$Open=='1'),]

# seperating out the elements of the date column for the train set
train$month <- as.integer(format(train$Date, "%m"))
train$year <- as.integer(format(train$Date, "%y"))
train$day <- as.integer(format(train$Date, "%d"))
# seperating out the elements of the date column for the test set
test$month <- as.integer(format(test$Date, "%m"))
test$year <- as.integer(format(test$Date, "%y"))
test$day <- as.integer(format(test$Date, "%d"))

#
load("Model\\clf51.RDATA")
plot(clf)
varImpPlot(clf)
pred <- exp(predict(clf, train)) -1

#diagnose predicted
evaluation<-function(y,yhat){
  pos <- which(y >0)
  return (sqrt((sum((y[pos] - yhat[pos])/(y[pos]))^2)/length(yhat[pos])))
}

aggregate(Sales ~ Store,train,sum)
