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

## I only care the variable, which has less than 0.1 ratio.
training2 <- training[,training_tf_cml_nor < 0.1]

dim(training2)

dim(training)
training2_tf <- ifelse(is.finite(training2), 1, 0)



hist(training_tf_cml_nor)
## so i drop the data...


training_tf <- training[is.na(training)]

dim(is.na(training))
is.data.table(training[is.na(training)])

head(training_tf)
colSums(training_tf)


