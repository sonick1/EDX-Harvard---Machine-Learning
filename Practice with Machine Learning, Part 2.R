library(caret)
library(tidyverse)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
# line of code
# Q7
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

# Q8

foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x > i ,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)

# Q9

predictions <- foo(train[,3])
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]
y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)

# Q10
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}
predictions <- apply(test[,-5],2,foo)
sapply(predictions,max)	

# Q11
plot(iris,pch=21,bg=iris$Species)

rangedValues_length <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
rangedValues_width <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)

length_prediction <- sapply(rangedValues_length,function(i){
  y_hat <-  ifelse(train$Petal.Length > i,'virginica','versicolor')
  mean(y_hat == train$Species)
})

length_cutoff <- rangedValues_length[which.max(length_prediction)] # 4.7

width_prediction <- sapply(rangedValues_width,function(i){
  y_hat <-  ifelse(train$Petal.Width > i,'virginica','versicolor')
  mean(y_hat == train$Species)
})

width_cutoff <- rangedValues_width[which.max(width_prediction)]

y_hat <- ifelse(train$Petal.Length > length_cutoff | train$Petal.Width > width_cutoff,'virginica','versicolor')
mean(y_hat == test$Species)
