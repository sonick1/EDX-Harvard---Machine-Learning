library(tidyverse)
library(caret)

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

plot(dat)
# Question 1

set.seed(1)
RMSE_vec <- replicate(n,{
  test_index <- createDataPartition(dat$y,times = 1,p = 0.5,list = FALSE)
  test_set <- dat[test_index, ]
  train_set <- dat[-test_index, ]
  lm.fit <- lm(data = train_set,formula = y ~ x)
  prediction <- predict(lm.fit,test_set)
  RSS <- (prediction - test_set$y)^2
  MSE <- sum(RSS)/nrow(test_set)
  RMSE <- sqrt(MSE)
  RMSE
  })
RMSE_vec

mean(RMSE_vec)
sd(RMSE_vec)

# Question 2

RMSE_func <- function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  RMSE_v <- replicate(100,{
  test_index <- createDataPartition(dat$y,times = 1,p = 0.5,list = FALSE)
  test_set <- dat[test_index, ]
  train_set <- dat[-test_index, ]
  lm.fit <- lm(data = train_set,formula = y ~ x)
  prediction <- predict(lm.fit,test_set)
  RSS <- (prediction - test_set$y)^2
  MSE <- mean(RSS)
  RMSE <- sqrt(MSE)
  RMSE
})
  c(avg = mean(RMSE_v), sd = sd(RMSE_v))
}

n <- c(100, 500, 1000, 5000, 10000)
sapply(n, RMSE_func)

# Question 4

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
RMSE_vec <- replicate(n,{
  test_index <- createDataPartition(dat$y,times = 1,p = 0.5,list = FALSE)
  test_set <- dat[test_index, ]
  train_set <- dat[-test_index, ]
  lm.fit <- lm(data = train_set,formula = y ~ x)
  prediction <- predict(lm.fit,test_set)
  RSS <- (prediction - test_set$y)^2
  MSE <- sum(RSS)/nrow(test_set)
  RMSE <- sqrt(MSE)
  RMSE
})
RMSE_vec

mean(RMSE_vec)
sd(RMSE_vec)

# Question 6
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

cor(dat)

test_index <- createDataPartition(dat$y,times = 1,p = 0.5,list = FALSE)
test_set <- dat[test_index, ]
train_set <- dat[-test_index, ]

lm.fit1 <- lm(data = train_set,formula = y ~ x_1)
y_hat1 <- predict(lm.fit1, newdata = test_set)
sqrt(mean((y_hat1-test_set$y)^2))

lm.fit2 <- lm(data = train_set,formula = y ~ x_2)
y_hat2 <- predict(lm.fit2, newdata = test_set)
sqrt(mean((y_hat2-test_set$y)^2))

lm.fit3 <- lm(data = train_set,formula = y ~ x_1 + x_2)
y_hat3 <- predict(lm.fit3, newdata = test_set)
sqrt(mean((y_hat3-test_set$y)^2))

# Question 8

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)

test_index <- createDataPartition(dat$y,times = 1,p = 0.5,list = FALSE)
test_set <- dat[test_index, ]
train_set <- dat[-test_index, ]

lm.fit1 <- lm(data = train_set,formula = y ~ x_1)
y_hat1 <- predict(lm.fit1, newdata = test_set)
sqrt(mean((y_hat1-test_set$y)^2))

lm.fit2 <- lm(data = train_set,formula = y ~ x_2)
y_hat2 <- predict(lm.fit2, newdata = test_set)
sqrt(mean((y_hat2-test_set$y)^2))

lm.fit3 <- lm(data = train_set,formula = y ~ x_1 + x_2)
y_hat3 <- predict(lm.fit3, newdata = test_set)
sqrt(mean((y_hat3-test_set$y)^2))
