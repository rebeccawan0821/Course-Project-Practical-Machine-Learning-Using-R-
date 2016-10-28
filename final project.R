rm(list=ls())
install.packages("rmarkdown")
##############Couresera Practical Machine Learning Project############################
library(caret)
library(rpart)
pml.training <- read.csv("C:/Users/siqiwan/Desktop/Machine Learning/pml-training.csv")
View(pml.training)
pml.testing <- read.csv("C:/Users/siqiwan/Desktop/Machine Learning/pml-testing.csv")
View(pml.testing)


###count raw data missing values
Missing=function(x)
{
  a=as.vector(colnames(x))
  b=seq(from=1,to=ncol(x),by=1)
  result=data.frame()
  for (i in b)
  {
    c=sum(is.na(x[,i]))
    result=rbind(result,c)
  }
  output=data.frame(a,result)
  colnames(output)=c("Column Name","No. of Missing Values")
  output
}
Missing(pml.training)
Missing(pml.testing)
pml.training<- pml.training[,-c(1,2,5,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,50,51,52,53,54,55,56,57,58,59,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,103,104,105,106,107,108,109,110,111,112,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,141,142,143,144,145,146,147,148,149,150)]
pml.testing<- pml.testing[,-c(1,2,5,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,50,51,52,53,54,55,56,57,58,59,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,103,104,105,106,107,108,109,110,111,112,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,141,142,143,144,145,146,147,148,149,150,160)]

##data slicing
inTrain <- createDataPartition(y=pml.training$classe,p=0.6,list=FALSE)
training <- pml.training[inTrain,]
testing <- pml.training[-inTrain,]
dim(training)
dim(testing)
str(training)

##random forest model
modfit2 <- randomForest(classe~ .,data=training,importance=TRUE)
modfit2

predict2 <- predict(modfit2,testing)
predict2
table(predict2,testing$classe)


##machting the predictors levels
common <- intersect(names(training), names(pml.testing)) 
for (p in common) { 
  if (class(training[[p]]) == "factor") { 
    levels(pml.testing[[p]]) <- levels(training[[p]]) 
  } 
}

##final prediction
predict <- predict(modfit2,pml.testing)
predict
