install.packages("rmarkdown")
install.packages("data.table")

library(data.table)

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

## and i ignore the variable which has meaningless data.
training3 <- training2[,8:60]
dim(training3)




