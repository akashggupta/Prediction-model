---
title: "Manner of the Exercise"
author: "Akash Gupta"
date: "6 June 2019"
output: html_document
---

```{r}
library(caret)
library(rpart)
library(rattle)
```
# Loading the data
```{r cache =TRUE}
training<-read.csv("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
testing<-read.csv("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
```
# Processing and Cleaning the data
```{r cache=TRUE}
# As we see in excel that our data contains some odd value like "NA", "", "#DIV/0!", so we convert all these value to na
# Here we are going to use na.strings() function to convert all the odd values to na
setwd("~/")
training<-read.csv("pml-training.csv",na.strings = c("NA", "", "#DIV/0!"))
testing<-read.csv("pml-testing.csv",na.strings = c("NA", "", "#DIV/0!"))

# Now we are going to remove all the na values
training<-training[,colSums(is.na(training))==0]
testing<-testing[,colSums(is.na(testing))==0]

# Now we are also going to remove first seven columns because they conatins only personal which do not help in our prediction
training<-training[,-c(1:7)]
testing<-testing[,-c(1:7)]

```
# Partitioing the training data into training and testing data
```{r}
training$classe<-as.factor(training$classe)
train<-createDataPartition(training$classe, p = 0.6, list = FALSE)
training1<-training[train,]
testing1<-training[-train,]
```
# Preparing model

## 1.)Using Decision tree
```{r,cache =TRUE}
model1<-train(classe~.,data=training1,method="rpart")

```
```{r cache=TRUE}
confusionMatrix(testing1$classe,predict(model1,newdata=testing1))
```
Here we ,can see that the accuracy of this model is 0.49 which is not that good so we try another approach 

## 2.) Using Random forest
```{r cache=TRUE}
model2<-train(classe~.,data=training1,method="rf",verbose=TRUE,ntree=128)

```
```{r cache=TRUE}
confusionMatrix(testing1$classe,predict(model2,newdata=testing1))
```
Here, we can see that the accurarcy of this model is very close to 1(0.98) , so this is the best model

# Prediction of the test set
```{r}
predict(model2$finalModel,newdata=testing)
```
# Varaible of Importance
```{r}
varImp(model2)
```









