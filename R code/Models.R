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



#### Random Forest #### 
interaction1 = traindata$valuePerSecond*traindata$scannedLineItemsPerSecond
my_forest = randomForest(fraud ~ trustLevel + totalScanTimeInSeconds + grandTotal + lineItemVoids + scansWithoutRegistration + quantityModifications + quantityModifications + scannedLineItemsPerSecond + valuePerSecond + lineItemVoidsPerPosition + interaction1, data = traindata, ntree=500,
                         importance=TRUE)
#can rank importance of features in the forest
importance(my_forest)
varImpPlot(my_forest)
#make prediction using my_forest and test data set
forest_prediction <- predict(my_forest, testdata)
#Saving in a dataframe
final_solution <- data.frame(transactionID = testdata$transactionID, fraud = forest_prediction )
#Saving in a csv file
write.csv(final_solution, "RF_solution.csv")



#### Neural Net ####
library(nnet)
nn.output <- matrix(0,nrow(traindata),2)
nn.output[which(traindata[,10]==1),1] <- 1
nn.output[which(traindata[,10]==1),2] <- 0
nn.output[which(traindata[,10]==2),1] <- 0
nn.output[which(traindata[,10]==2),2] <- 2
model <- nnet(traindata[,-10], 
              nn.output, 
              size = 4, softmax=TRUE, maxit=200, )
summary(model)
#Saving in a dataframe
final_solution2 <- data.frame(transactionID = testdata$transactionID, fraud = forest_prediction )
#Saving in a csv file
write.csv(final_solution, "RF_solution.csv")

# 将数据集划分为大小相近的k个子集
cvsamp <- function(k, datasize){
  cvlist <- list()
  subsetSize <- datasize/k
  datarow <- 1:datasize
  for (i in 1:k-2) {
    group <- sample(datarow, subsetSize)
    cvlist <- append(cvlist, list(group))
    datarow <- setdiff(datarow, group)
  }
  cvlist <- append(cvlist, list(datarow))
  cvlist <- cvlist[-length(cvlist)]
}

k <- 10
cvlist <- cvsamp(k = k,datasize = nrow(traindata))
cvlist

# 计算k次训练得到的模型的测试误差大小
error <- function(k,hp){
  err <- 0
  for (i in 1:k){
    cvlist <- cvsamp(k = k,datasize = nrow(traindata))
    trainset <- traindata[-unlist(cvlist[i]),]
    testset <- traindata[unlist(cvlist[i]),]
    Y=class.ind(trainset[,10])
    ytrue <- testset$fraud
    
    model <- nnet(trainset[,-10],Y,softmax=TRUE,
                  size=hp[1],maxit=hp[2])
    Ypred <- predict(model,testset[,-10])
    ypred <- max.col(Ypred)
    err <- err + sum(ypred != ytrue)/nrow(testset)
  } 
  return(err/k)
}

#控制隐藏层节点数
size <- 1:10
errlist <- vector()
for (i in size){
  errlist <- c(errlist, error(k,c(i,200)))
}
plot(size, errlist)

#控制迭代次数
maxit <- seq(1,500,10)
errlist <- vector()
for (i in maxit){
  errlist <- c(errlist, error(k,c(4,i)))
}
plot(maxit[2:length(maxit)], errlist[2:length(errlist)],
     type = "b")

#predict
fraud.predict <- predict(model,testdata,type = "class")
nn.table = table(testdata$transactionID,fraud.predict)
nn.table
#模糊矩阵
confusionMatrix(nn.table)
#Saving in a dataframe
final_solution2 <- data.frame(transactionID = testdata$transactionID, fraud = forest_prediction )
#Saving in a csv file
write.csv(final_solution, "RF_solution2.csv")