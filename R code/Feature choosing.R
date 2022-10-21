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



#### feature selection ####
library(tidyverse)
library(caret)
fraud <- as.character(traindata$fraud)
traindata2 <- na.omit(traindata)
featurePlot(x = traindata2[, 1:10], 
            y = fraud, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

####* rfe
set.seed(100)
options(warn=-1)
subsets <- c(1:3, 6, 9)
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)
lmProfile <- rfe(x=traindata[, 1:9], y=trainData$Purchase,
                 sizes = subsets,
                 rfeControl = ctrl)
lmProfile





