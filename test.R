install.packages("caret")
install.packages("rmarkdown")
install.packages("data.table")
install.packages("list")

#library(data.table)
library(caret)
library(list)

training <- read.csv("pml-training.csv", stringsAsFactors=FALSE, na.strings = c("NA", ""))
head(training)

## when I look over training data, there is a empty cell
## so, I set this cell as NA

training_tf <- ifelse(is.na(training), 1, 0)
training_tf_cml <- colSums(training_tf)
training_tf_cml_nor <- training_tf_cml / dim(training_tf)[1]

## I only care the variable, which has less than 0.1 NA ratio.
hist(training_tf_cml_nor)
validColumns <- training_tf_cml_nor < 0.1
training2 <- training[, validColumns]

dim(training2)

## and I ignore the variable which has meaningless data.
training3 <- training2[,8:60]
dim(training3)
head(training3)

## data balance check
type_list <- unique(training3$classe)
table(training3$classe)
min_sample <- min(table(training3$classe))

training4 <- NULL
for(str in type_list) {
  ttt <- training3[training3$classe==str,]

  tmp <- sample(c(1:(dim(ttt)[1])), min_sample, replace=FALSE)
  tmp2 <- ttt[tmp,]

  if(is.null(training4)) {
    training4 <- tmp2
  }else {
    print(table(tmp2$classe))
    training4 <- rbind(training4, tmp2)
  }
}

table(training4$classe)


## make training data to make model
inTrain <- createDataPartition(y=training4$classe, p=0.7, list=FALSE)
m_train <- training4[inTrain,]
m_test  <- training4[-inTrain,]

table(m_train$classe)
table(m_test$classe)


## training with lda
modelLda <- train(classe ~., data=m_train, method="lda")
modelLda

## Accuracy with training data is 69.1%
## so, I try another method

## training with rt
modelRf <- train(classe ~., data=m_train, method="rf")
modelRf$finalModel

m_pred <- predict(modelRf$finalModel, newdata=m_test)
a <- cbind(m_test$classe, as.character(m_pred))
m_pred_tf <- ifelse(m_test$classe == as.character(m_pred), 1, 0)

table(m_pred_tf)[2]/sum(table(m_pred_tf))
## 99.3%
