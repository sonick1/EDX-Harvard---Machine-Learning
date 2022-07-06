library(dslabs)
library(caret)
data(mnist_27)
# set.seed(1995) # if R 3.5 or earlier
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)

# Q1
sum(indexes$Resample01 == 3)
sum(indexes$Resample01 == 4)
sum(indexes$Resample01 == 7)


# Q2
x <- sapply(indexes,function(ind){
  sum(ind == 3)
})
x
sum(x)

#Q3
set.seed(1, sample.kind="Rounding") # if R 3.6 or later

y <- rnorm(100, 0, 1)
y

B <- 10000
s <- replicate(B,{
  y <- rnorm(100, 0, 1)
  q <-quantile(y, 0.75)
})
mean(s)
sd(s)

# set.seed(1) # # if R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
B <- 3
q_75 <- replicate(B, {
  y <- rnorm(100, 0, 1)
  sum(round(y,0) == 1)
})

q_75
mean(q_75)
sd(q_75)


# Q4
# set.seed(1) # if R 3.5 or earlier
# set.seed(1) # if R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)

# set.seed(1) # if R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(y, 10000)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)
