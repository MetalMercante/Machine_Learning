library(dplyr)
library(tidyr)
library(lubridate)
library(caret)

train <- read.csv('D:\\Google_Drive\\R\\Machine_learning\\Project\\pml-training.csv', stringsAsFactors = F,  na.strings=c('#DIV/0', '', 'NA'))
validation <- read.csv('D:\\Google_Drive\\R\\Machine_learning\\Project\\pml-testing.csv', stringsAsFactors = F,  na.strings=c('#DIV/0', '', 'NA'))

train2 <- train
sum(train=='#DIV/0', na.rm=TRUE)

validation2 <- validation
classe <- train2$classe

train2 <- train2 %>% select(-X,-user_name, -raw_timestamp_part_1, -raw_timestamp_part_2, -cvtd_timestamp, -new_window, -num_window, -classe)


train2[] <- lapply(train2, as.numeric)
train2$classe <- classe

na_count <-sapply(train2, function(y) sum(length(which(is.na(y)))))
na_count[na_count>0]

na_pos <- na_count>19000
cols <- data.frame(COL=seq(1:length(train2)), TF=na_pos)
train3 <- train2[,cols$COL[cols$TF=='FALSE']]

sapply(train3, function(y) sum(length(which(is.na(y)))))

inTrain <- createDataPartition(train3$classe, p=0.7, list=F) 
train4 <- train3[inTrain,]
test <- train3[-inTrain,]

library(randomForest)
set.seed(12345)
modFit <- randomForest(factor(classe) ~ . , data=train4,ntree=1000,importance=TRUE, do.trace=T)

plot(modFit,main='randomForest error rate')
varImpPlot(modFit)


pred2 <- predict(modFit,newdata=test)
CM2 <- confusionMatrix(test$classe, pred2)
CM2

setwd()
saveRDS(modFit,'D:\\Google_Drive\\R\\Machine_learning\\Project\\modelFit.RDS' )
