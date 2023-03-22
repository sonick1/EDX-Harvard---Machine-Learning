library(dslabs)
library(dplyr)
library(lubridate)
library(caret)
data(heights)

head(heights)

# Set the seed
set.seed(1)
test_index <- createDataPartition(heights$sex,times = 1,p = 0.5,list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

s <- seq(1,100,3)

knn_func <- function(n){
  knn_fit <- knn3(sex ~ ., data = train_set, k =n)
  
  y_hat_knn <- predict(knn_fit,test_set,type = "class")
  
  confusionMatrix(y_hat_knn,train_set$sex)$overall[["Accuracy"]]
  
  F_meas(y_hat_knn,reference = as.factor(test_set$sex))
  }

# Question 1
F_means <- sapply(s,knn_func)

max(F_means)


# Question 2
library(dslabs)
library(caret)
data("tissue_gene_expression")

head(tissue_gene_expression)

test_index <- createDataPartition(tissue_gene_expression$y,times = 1, p = 0.5,list = FALSE)
test_set_x<- tissue_gene_expression$x[test_index,]
train_set_x <- tissue_gene_expression$x[-test_index,]
test_set_y <- tissue_gene_expression$y[test_index]
train_set_y <- tissue_gene_expression$y[-test_index]

k <- seq(1, 11, 2)
accuracy_vector <- sapply(k, function(k){
knn_fit <- knn3(train_set_x,train_set_y,k = k)
y_hat_knn <- predict(knn_fit,test_set_x,test_set_y,type = "class")
confusionMatrix(y_hat_knn,test_set_y)$overall[["Accuracy"]]
})
accuracy_vector
