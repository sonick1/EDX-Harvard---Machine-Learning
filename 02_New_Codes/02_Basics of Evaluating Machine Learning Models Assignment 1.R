library(dslabs)
library(tidyverse)
read_mnist()
length(read_mnist())


mnist <- read_mnist()
ncol(mnist$train$images)
