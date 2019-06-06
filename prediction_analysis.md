``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 3.5.3

    ## Loading required package: lattice

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 3.5.2

``` r
library(rpart)
library(rattle)
```

    ## Warning: package 'rattle' was built under R version 3.5.3

    ## Rattle: A free graphical interface for data science with R.
    ## Version 5.2.0 Copyright (c) 2006-2018 Togaware Pty Ltd.
    ## Type 'rattle()' to shake, rattle, and roll your data.

Loading the data
================

``` r
training<-read.csv("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
testing<-read.csv("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
```

Processing and Cleaning the data
================================

``` r
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

Partitioing the training data into training and testing data
============================================================

``` r
training$classe<-as.factor(training$classe)
train<-createDataPartition(training$classe, p = 0.6, list = FALSE)
training1<-training[train,]
testing1<-training[-train,]
```

Preparing model
===============

1.)Using Decision tree
----------------------

``` r
model1<-train(classe~.,data=training1,method="rpart")
```

``` r
confusionMatrix(testing1$classe,predict(model1,newdata=testing1))
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 2040   28  157    0    7
    ##          B  642  504  372    0    0
    ##          C  640   45  683    0    0
    ##          D  582  225  479    0    0
    ##          E  217  193  405    0  627
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.4912          
    ##                  95% CI : (0.4801, 0.5023)
    ##     No Information Rate : 0.5252          
    ##     P-Value [Acc > NIR] : 1               
    ##                                           
    ##                   Kappa : 0.3346          
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.4950  0.50653  0.32586       NA  0.98896
    ## Specificity            0.9485  0.85199  0.88087   0.8361  0.88699
    ## Pos Pred Value         0.9140  0.33202  0.49927       NA  0.43481
    ## Neg Pred Value         0.6293  0.92241  0.78188       NA  0.99891
    ## Prevalence             0.5252  0.12682  0.26714   0.0000  0.08081
    ## Detection Rate         0.2600  0.06424  0.08705   0.0000  0.07991
    ## Detection Prevalence   0.2845  0.19347  0.17436   0.1639  0.18379
    ## Balanced Accuracy      0.7217  0.67926  0.60336       NA  0.93798

Here we ,can see that the accuracy of this model is 0.49 which is not that good so we try another approach

2.) Using Random forest
-----------------------

``` r
model2<-train(classe~.,data=training1,method="rf",verbose=TRUE,ntree=128)
```

``` r
confusionMatrix(testing1$classe,predict(model2,newdata=testing1))
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 2231    1    0    0    0
    ##          B   18 1497    3    0    0
    ##          C    0    9 1355    4    0
    ##          D    1    0   16 1267    2
    ##          E    0    0    4    8 1430
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9916          
    ##                  95% CI : (0.9893, 0.9935)
    ##     No Information Rate : 0.2868          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9894          
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9916   0.9934   0.9833   0.9906   0.9986
    ## Specificity            0.9998   0.9967   0.9980   0.9971   0.9981
    ## Pos Pred Value         0.9996   0.9862   0.9905   0.9852   0.9917
    ## Neg Pred Value         0.9966   0.9984   0.9964   0.9982   0.9997
    ## Prevalence             0.2868   0.1921   0.1756   0.1630   0.1825
    ## Detection Rate         0.2843   0.1908   0.1727   0.1615   0.1823
    ## Detection Prevalence   0.2845   0.1935   0.1744   0.1639   0.1838
    ## Balanced Accuracy      0.9957   0.9950   0.9906   0.9939   0.9984

Here, we can see that the accurarcy of this model is very close to 1(0.98) , so this is the best model

Prediction of the test set
==========================

``` r
predict(model2,newdata=testing)
```

    ##  [1] B A B A A E D B A A B C B A E E A B B B
    ## Levels: A B C D E

Varaible of Importance
======================

``` r
varImp(model2)
```

    ## rf variable importance
    ## 
    ##   only 20 most important variables shown (out of 52)
    ## 
    ##                      Overall
    ## roll_belt            100.000
    ## pitch_forearm         58.137
    ## yaw_belt              50.370
    ## magnet_dumbbell_y     44.453
    ## roll_forearm          44.085
    ## pitch_belt            41.768
    ## magnet_dumbbell_z     41.270
    ## accel_dumbbell_y      23.938
    ## magnet_dumbbell_x     16.249
    ## roll_dumbbell         16.081
    ## accel_forearm_x       15.089
    ## magnet_forearm_z      14.089
    ## magnet_belt_z         13.845
    ## total_accel_dumbbell  13.825
    ## accel_dumbbell_z      13.630
    ## magnet_belt_y         13.576
    ## gyros_belt_z          10.995
    ## magnet_belt_x         10.338
    ## accel_belt_z          10.071
    ## yaw_arm                9.448
