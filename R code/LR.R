#### EDA ####
#Importing the data
traindata<-read.csv("D:/R-Data Analysis/Final Project/data/mgis489_train.csv")
testdata<-read.csv("D:/R-Data Analysis/Final Project/data//mgis489_test.csv")
dim(testdata)

#Checking the structure and dimension of traindata
str(traindata)
dim(traindata)

#Doing necessary changes to dimensions of traindata
traindata$fraud<-as.factor(traindata$fraud)
#str(traindata)

#Checking the position of missing value
which(is.na(traindata))

#Rough Imputation Of Missing Values
library(randomForest)
traindata<-na.roughfix(traindata)
which(is.na(traindata))

#Checking the structure and dimension of testdata
dim(testdata)
str(testdata)

#Checking the position of missing value
which(is.na(testdata))

#Rough Imputation Of Missing Values
testdata<-na.roughfix(testdata)
which(is.na(testdata))


## training set
traindata$trustLevel <- as.factor(traindata$trustLevel)
TotalItem <- traindata$totalScanTimeInSeconds * traindata$scannedLineItemsPerSecond
Train <- data.frame(traindata, TotalItem = TotalItem)
Train <- Train[,-c(3,6,7,9)]


## testing set
testdata$trustLevel <- as.factor(testdata$trustLevel)
TotalItem <- testdata$totalScanTimeInSeconds * testdata$scannedLineItemsPerSecond
Test <- data.frame(testdata, TotalItem = TotalItem)
Test <- Test[,-c(3,6,7,9)]

## true value

formula1 <- "fraud~trustLevel + TotalItem + lineItemVoids + scansWithoutRegistration + 
totalScanTimeInSeconds "

formula2 <- "fraud~trustLevel + TotalItem + lineItemVoids + scansWithoutRegistration + 
totalScanTimeInSeconds + I( totalScanTimeInSeconds * valuePerSecond ) +
I( totalScanTimeInSeconds * valuePerSecond^2 )"

formula3 <- "fraud~trustLevel + TotalItem + lineItemVoids + scansWithoutRegistration + 
totalScanTimeInSeconds + I( totalScanTimeInSeconds * valuePerSecond ) +
I( totalScanTimeInSeconds * valuePerSecond^(3.5) )"

fit1 <- glm(formula=formula1,data=Train,family=binomial(link=logit))
fit2 <- glm(formula=formula2,data=Train,family=binomial(link=logit))
fit3 <- glm(formula=formula3,data=Train,family=binomial(link=logit))
P1 <- predict(fit1, Test, type ="response")
P2 <- predict(fit2, Test, type ="response")
P3 <- predict(fit3, Test, type ="response")
P_M1 <- 0.4*P1 + 0.6*P2
P_M2 <- 0.4*P1 + 0.6*P3
P_M3 <- 0.3*P1 + 0.1*P2 + 0.6*P3

Pred1 <- ifelse(P_M1>5/7,1,0)
Pred2 <- ifelse(P_M2>5/7,1,0)
Pred3 <- ifelse(P_M3>5/7,1,0)
Pred_df <- data.frame(Pred1,Pred2,Pred3)

# Majority Vote
Pred_df$vote <- apply(Pred_df, 1, function(p){
  re <- ifelse(sum(p) > 1,1,0)
})

submit <- as.data.frame(Pred_df$vote)
colnames(submit) <- 'fraud'
write.csv(submit, file='final.csv')
