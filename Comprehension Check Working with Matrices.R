library(tidyverse)
library(dslabs)
mnist <- read_mnist()
length(mnist)
 
s <- mnist$train$images 
t <- mnist$train$labels

dim(x)
x

y <- rowMeans(x >= 50 & x <= 205)

y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")
mean(y) # proportion of pixels
