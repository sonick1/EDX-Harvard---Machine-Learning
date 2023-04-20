library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

head(titanic_clean)

# Question 1

set.seed(42)
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)

train_set <- titanic_clean %>% slice(-test_index)
test_set <- titanic_clean %>% slice(test_index)

nrow(train_set)
nrow(test_set)

mean(train_set$Survived == 1)

# Question 2

set.seed(3)
y_pred <- sample(c(0,1),length(test_set$Survived),replace = TRUE)
mean(test_set$Survived == y_pred)

# Question 3a

train_set %>% group_by(Sex) %>% summarize(Survived = mean(Survived == 1))
table(train_set$Survived,train_set$Sex)

# Question 3b
y_hat_Sex <- ifelse(test_set$Sex == "female",1,0)
mean(test_set$Survived == y_hat_Sex)

# Question 4a
train_set %>% group_by(Pclass) %>% summarize(Survived = mean(Survived == 1))

# Question 4b
y_hat_pclass <- ifelse(test_set$Pclass == 1,1,0)
mean(test_set$Survived == y_hat_pclass)

# Question 4c
train_set %>% group_by(Sex,Pclass) %>% summarize(Survived = mean(Survived == 1))

# Question 4d
y_hat_both <- ifelse(test_set$Sex == "female" & test_set$Pclass %in% c(1,2),1,0)
mean(test_set$Survived == y_hat_both)

# Question 5a
confusionMatrix(as.factor(y_hat_Sex), as.factor(test_set$Survived))
confusionMatrix(as.factor(y_hat_pclass), as.factor(test_set$Survived))
confusionMatrix(as.factor(y_hat_both), as.factor(test_set$Survived))

# Question 6
F_meas(data=factor(test_set$Survived), reference = factor(y_hat_Sex))
F_meas(data=factor(test_set$Survived), reference = factor(y_hat_pclass))
F_meas(data=factor(test_set$Survived), reference = factor(y_hat_both))

# Question 7
set.seed(1)
train_loess <- train(Survived ~ Fare, method = "gamLoess", data = train_set)
train_loess

# Question 8
set.seed(1)
train_loess <- train(Survived ~ Age , method = "glm", data = train_set)
y_hat_loess <- predict(train_loess,test_set)
confusionMatrix(test_set$Survived,y_hat_loess)

set.seed(1)
train_loess <- train(Survived ~ Sex + Pclass + Fare + Age , method = "glm", data = train_set)
y_hat_loess <- predict(train_loess,test_set)
confusionMatrix(test_set$Survived,y_hat_loess)

set.seed(1)
train_loess <- train(Survived ~ ., method = "glm", data = train_set)
y_hat_loess <- predict(train_loess,test_set)
confusionMatrix(test_set$Survived,y_hat_loess)

# Question 9
set.seed(6)
train_knn <- train(Survived ~ .,
                   method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))

train_knn$bestTune

# Question 9b
plot(train_knn)

# Question 9c
y_hat_knn <- predict(train_knn,test_set)
confusionMatrix(test_set$Survived,y_hat_knn)

# Question 10
set.seed(8)
train_control <- trainControl(method = "cv", number = 10)
cv_model <- train(Survived~., data = train_set,
                  trControl = train_control,
                  method = "knn",
                  tuneGrid = data.frame(k = seq(3, 51, 2)))
cv_model$bestTune

cv_knn_preds <- predict(cv_model, test_set)
mean(cv_knn_preds == test_set$Survived)

# Question 11
set.seed(10)
train_rpart <- train(Survived ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                     data = train_set)
train_rpart$bestTune
rpart_preds <- predict(train_rpart, test_set)
mean(rpart_preds == test_set$Survived)

ggplot(train_rpart)

plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

# Question 12

train_rf <- train(Survived ~ .,
                  method = "rf",
                  data = train_set,
                  tuneGrid = data.frame(mtry = seq(1:7)))
train_rf$bestTune
rf_preds <- predict(train_rf, test_set)
mean(rf_preds == test_set$Survived)


