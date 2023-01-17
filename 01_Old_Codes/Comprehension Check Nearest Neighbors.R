library(dslabs)
library(tidyverse)
library(caret)
data("heights")
h <- heights
h$height <- as.factor(round(h$height,0))

set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later

#Q1

s <- seq(1, 101, 3)
test_index <- createDataPartition(h$height, times = 1, p = 0.5, list = FALSE)
train_set <- h %>% slice(-test_index)
test_set <- h %>% slice(test_index)

F_1 <- sapply(s,function(k){
knn_fit <- knn3(sex ~ ., data = train_set, k = k)
y_hat_knn <- predict(knn_fit, test_set, type = "class")
F_val <- F_meas(data = y_hat_knn, reference = as.factor(test_set$sex))
})
l
max(F_1)
which.max(F_1)
plot(s,F_1)
s[which.max(F_1)]

# Q2
library(dslabs)
library(caret)
data("tissue_gene_expression")

y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})

