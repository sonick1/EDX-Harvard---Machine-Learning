library(dslabs)
library(caret)

data(mnist_27)

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

# Question 1
sum(indexes[[1]] == 3) #1
sum(indexes[[1]] == 4) #4
sum(indexes[[1]] == 7) #0

# Question 2
re_3 <- sapply(indexes,function(x) sum(x==3))
sum(re_3)
# 11

# Question 3
y <- rnorm(100, 0, 1)
set.seed(1)

qu_75 <- replicate(10000,{
  y <- rnorm(100, 0, 1)
  x <- quantile(y, 0.75)
})
qu_75
mean(qu_75)
sd(qu_75)

# Question 4

set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)

# set.seed(1) # if R 3.5 or earlier
indexes <- createResample(y, 10000)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)

# Question 5
