#Alex's Script on
#Rossman Store Sale Competition
#Date: 2015 12 5

rm(list=ls())

library(readr)
library(randomForest)
library(data.table)
setwd("E:\\Kaggle\\12 14 2015 Rossman Store Sales (HOME)\\")
set.seed(616)

sigmoid <- function(t){
  (1/(1+exp(-t)))
}

#2 s ( -10 X ) if x > 0
#2 s ( 10 X ) if x <= 0
makeClosenessFactor <- function(x){
  ifelse(x <= 0, 2*sigmoid(x/10),2*sigmoid(-x/10))
}

cat("reading the train and test data\n")
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

# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)
#train <- train[,-c(3,8)]

# seperating out the elements of the date column for the test set
test$month <- as.integer(format(test$Date, "%m"))
test$year <- as.integer(format(test$Date, "%y"))
test$day <- as.integer(format(test$Date, "%d"))

#Change competitionDistance to 0 if competitionOpenSince is greater than the date
train$CompetitionOpenSinceDate<-(as.Date(paste(as.character(train$CompetitionOpenSinceYear),
                                               as.character(train$CompetitionOpenSinceMonth),"01",sep = "-")))
test$CompetitionOpenSinceDate<-(as.Date(paste(as.character(test$CompetitionOpenSinceYear),
                                              as.character(test$CompetitionOpenSinceMonth),"01",sep = "-")))

train$CompetitionOpenSinceDifference<- as.integer(train$Date - train$CompetitionOpenSinceDate)
test$CompetitionOpenSinceDifference<- as.integer(test$Date - test$CompetitionOpenSinceDate)

train[which(is.na(train$CompetitionOpenSinceDifference)),]$CompetitionOpenSinceDifference <- 0
test[which(is.na(test$CompetitionOpenSinceDifference)),]$CompetitionOpenSinceDifference <- 0
#train[which(train$CompetitionOpenSinceDifference <0),]$CompetitionDistance = 0
#test[which(test$CompetitionOpenSinceDifference <0),]$CompetitionDistance = 0



train_lr <- train
train_lr$logSales <- log(train$Sales+1)


train_lr$SrDistance <- train$CompetitionDistance^2
test_lr <- test
test_lr$SrDistance <- test$CompetitionDistance^2

#Close factor 
#[2015-12-05 11:58:03 PM] Mike Lin: 1/ max(1,sqrt( | difference | )
train_lr$CloseFactor <- makeClosenessFactor(train_lr$CompetitionOpenSinceDifference)

test_lr$CloseFactor <- makeClosenessFactor(test_lr$CompetitionOpenSinceDifference)

#CompetitionFactor = CloseFactor / max(1, sqrt(distance) )
train_lr$CompetitionFactor <- train_lr$CloseFactor / ifelse(train_lr$SrDistance < 1,1,train_lr$SrDistance)
test_lr$CompetitionFactor <- test_lr$CloseFactor / ifelse(test_lr$SrDistance < 1, 1 , test_lr$SrDistance)

############ 1st model best model
############  0.12998
############
xnam <- c("Store","DayOfWeek","Promo","StoreType","Assortment","CompetitionDistance","CompetitionOpenSinceMonth",
          "CompetitionOpenSinceYear","Promo2SinceWeek","Promo2SinceYear","PromoInterval","month","year","day")

#random forest
train_rf <- train_lr[,c(xnam,'logSales')]
clf <- randomForest(train_rf[,xnam], 
                    train_rf$logSales,
                    mtry=5,
                    ntree=50,
                    sampsize=100000,
                    do.trace=TRUE)

plot(clf)
varImpPlot(clf)


#make prediction
test_rf <- test_lr[,xnam]
pred <- exp(predict(clf, test_rf)) -1
submission <- data.frame(Id=test$Id, Sales=pred)

cat("saving the submission file\n")
write_csv(submission, "rfv51.csv")
save(clf,file = "Model\\clf51.RDATA")
############ 
############ 
############  1st model end/

############ 2nd model: a simpler model with unimportant variables removed
############ removed feature: Assortment,Promo2SinceYear,PromoInterval,month,year
############ 0.14466

xnam <- c("Store","DayOfWeek","Promo","StoreType","CompetitionDistance","CompetitionOpenSinceMonth",
          "CompetitionOpenSinceDifference","CompetitionOpenSinceYear","day")

#random forest
train_rf <- train_lr[,c(xnam,'logSales')]
clf <- randomForest(train_rf[,xnam], 
                    train_rf$logSales,
                    mtry=5,
                    ntree=50,
                    sampsize=100000,
                    do.trace=TRUE)

plot(clf)
varImpPlot(clf)


#make prediction
test_rf <- test_lr[,xnam]
pred <- exp(predict(clf, test_rf)) -1
submission <- data.frame(Id=test$Id, Sales=pred)

cat("saving the submission file\n")
write_csv(submission, "rfv52.csv")
save(clf,file = "Model\\clf52.RDATA")

############ 
############ 
############  2nd model end/

############ 3rd model, transform categorical variables: month, DayOfWeek
############
############
xnam <- c("Store","DayOfWeek","Promo","StoreType","Assortment","CompetitionDistance","CompetitionOpenSinceMonth",
          "CompetitionOpenSinceYear","Promo2SinceWeek","Promo2SinceYear","PromoInterval","month","year","day")


train_rf <- train_lr[,c(xnam,'logSales')]
###
train_rf$DayOfWeek <- as.factor(train_rf$DayOfWeek)
train_rf$month <- as.factor(train_rf$month)

###
clf <- randomForest(train_rf[,xnam], 
                    train_rf$logSales,
                    mtry=5,
                    ntree=50,
                    sampsize=100000,
                    do.trace=TRUE)

plot(clf)
varImpPlot(clf)


#make prediction
test_rf <- test_lr[,xnam]
test_rf$DayOfWeek <- as.factor(test_rf$DayOfWeek)
test_rf$month <- as.factor(test_rf$month)
pred <- exp(predict(clf, test_rf)) -1
submission <- data.frame(Id=test$Id, Sales=pred)

cat("saving the submission file\n")
write_csv(submission, "rfv53.csv")
save(clf,file = "Model\\clf53.RDATA")
############ 
############ 
############  3rd model end/
