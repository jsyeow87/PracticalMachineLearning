Assignment
Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

Data
The training data for this project are available here: [https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv]

The test data are available here: [https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv]

The data for this project come from this source: [http://groupware.les.inf.puc-rio.br/har]. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.

Preliminary Coding
Save the data sets into the working directory

train <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!", ""))
test <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!", ""))
Cleaning up of dataset
Delete columns with missing values

train <-train[,colSums(is.na(train)) == 0]
test <-test[,colSums(is.na(test)) == 0]
Remove irrelevant variables

train   <-train[,-c(1:7)]
test <-test[,-c(1:7)]
To look at the train dataset feature and rough data distribution

dim(train)
## [1] 19622    53
table(train$classe)
## 
##    A    B    C    D    E 
## 5580 3797 3422 3216 3607
Data partitioning
library(caret)
## Loading required package: lattice
## Loading required package: ggplot2
set.seed(1111)
trainset <- createDataPartition(train$classe, p = 0.75, list = FALSE)
Training <- train[trainset, ]
Validation <- train[-trainset, ]
Prediction model
For classification and regression, the Random Forest algorithm is employed

library(randomForest)
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
Model <- randomForest(classe ~ ., data = Training, importance = TRUE, ntrees = 10)
Validation of model
Accuracy of Training set

modeltraining <- predict(Model, Training)
print(confusionMatrix(modeltraining, Training$classe))
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 4185    0    0    0    0
##          B    0 2848    0    0    0
##          C    0    0 2567    0    0
##          D    0    0    0 2412    0
##          E    0    0    0    0 2706
## 
## Overall Statistics
##                                      
##                Accuracy : 1          
##                  95% CI : (0.9997, 1)
##     No Information Rate : 0.2843     
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 1          
##  Mcnemar's Test P-Value : NA         
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
## Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
## Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Prevalence             0.2843   0.1935   0.1744   0.1639   0.1839
## Detection Rate         0.2843   0.1935   0.1744   0.1639   0.1839
## Detection Prevalence   0.2843   0.1935   0.1744   0.1639   0.1839
## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000
Accuracy of Validation set

modelvalidation <- predict(Model, Validation)
print(confusionMatrix(modelvalidation, Validation$classe))
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1393    2    0    0    0
##          B    2  944    3    0    0
##          C    0    3  852    9    1
##          D    0    0    0  794    1
##          E    0    0    0    1  899
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9955          
##                  95% CI : (0.9932, 0.9972)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9943          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9986   0.9947   0.9965   0.9876   0.9978
## Specificity            0.9994   0.9987   0.9968   0.9998   0.9998
## Pos Pred Value         0.9986   0.9947   0.9850   0.9987   0.9989
## Neg Pred Value         0.9994   0.9987   0.9993   0.9976   0.9995
## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
## Detection Rate         0.2841   0.1925   0.1737   0.1619   0.1833
## Detection Prevalence   0.2845   0.1935   0.1764   0.1621   0.1835
## Balanced Accuracy      0.9990   0.9967   0.9966   0.9937   0.9988
Prediction on test set
ptest <- predict(Model, test)
ptest
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
Submission of output answers
answers <- as.vector(ptest)


pml_write_files = function(x){
   n = length(x)
   for(i in 1:n){
      filename = paste0("problem_id_",i,".txt")
      write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
   }
}

pml_write_files(answers)
