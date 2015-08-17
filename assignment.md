train <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!", ""))

test <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!", ""))

dim(train)

table(train$classe)

library(caret)

set.seed(123456)
trainset <- createDataPartition(train$classe, p = 0.8, list = FALSE)
Training <- train[trainset, ]
Validation <- train[-trainset, ]

# exclude near zero variance features
nzvcol <- nearZeroVar(Training)
Training <- Training[, -nzvcol]

# exclude columns with m40% ore more missing values exclude descriptive
# columns like name etc
cntlength <- sapply(Training, function(x) {
   sum(!(is.na(x) | x == ""))
})
nullcol <- names(cntlength[cntlength < 0.6 * length(Training$classe)])
descriptcol <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", 
                 "cvtd_timestamp", "new_window", "num_window")
excludecols <- c(descriptcol, nullcol)
Training <- Training[, !names(Training) %in% excludecols]

library(randomForest)

rfModel <- randomForest(classe ~ ., data = Training, importance = TRUE, ntrees = 10)

ptraining <- predict(rfModel, Training)
print(confusionMatrix(ptraining, Training$classe))

pvalidation <- predict(rfModel, Validation)
print(confusionMatrix(pvalidation, Validation$classe))

ptest <- predict(rfModel, test)
ptest

answers <- as.vector(ptest)


pml_write_files = function(x){
   n = length(x)
   for(i in 1:n){
      filename = paste0("problem_id_",i,".txt")
      write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
   }
}

pml_write_files(answers)
