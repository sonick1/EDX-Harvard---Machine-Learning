library(dslabs)
library(dplyr)
library(lubridate)
library(caret)
data(reported_heights)


dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

# Question 1
tab <- table(x,y)
tab

tab[1,1]/(tab[1,1] + tab[1,2])

tab[2,1]/(tab[2,1] + tab[2,2])

dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))

prop.table(tab,1)

# Question 2

y_hat <- ifelse(x == "inclass","Female","Male") %>% factor(levels = levels(y))
mean(y == y_hat)

# Question 3

table(y_hat,y)

# Question 4

sensitivity(y_hat,y)

# Question 5

specificity(y_hat,y)

# Question 6

mean(y == "Female")

# Question 7

data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(76)
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

# Question 8
head(train)

foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by = 0.1)
  sapply(rangedValues,function(i){
  y_hat <- ifelse(x > i,'virginica','versicolor')
  mean(y_hat == train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)

# Question 9

rangedValues <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by = 0.1)
accuracy_cutoffs <- sapply(rangedValues,function(i){
  y_hat <- ifelse(train$Petal.Width > i,'virginica','versicolor')
  mean(y_hat == train$Species)
})
cutoff_max <- rangedValues[which.max(accuracy_cutoffs)]
y_hat_test <- ifelse(test$Petal.Width > cutoff_max,'virginica','versicolor')
mean(y_hat_test == test$Species)


# Alternate
predictions <- foo(train[,4])
rangedValues <- seq(range(train[,4])[1], range(train[,4])[2], by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,4]>cutoffs[1], 'virginica', 'versicolor')
mean(y_hat==test$Species)

# Question 10

# Same as Q8

# Question 11
plot(iris, pch=21, bg=iris$Species)

predictions <- apply(train[,-5],2,foo)
predictions

rangedValues_Petal_width <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],0.1)
rangedValues_Petal_length <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],0.1)
cutoff_Petal_width <- rangedValues_Petal_width[which.max(predictions$Petal.Width)]
cutoff_Petal_length <- rangedValues_Petal_length[which.max(predictions$Petal.Length)]

y_hat_both <- ifelse((test$Petal.Width > cutoff_Petal_width) & (test$Petal.Length > cutoff_Petal_length),
                     'virginica', 'versicolor')
mean(y_hat_both == test$Species)
