options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
library(ggplot2)
library(ggcorrplot)
data(brca)

# Question 1
nrow(brca$x)

ncol(brca$x)

mean(brca$y == "M")

names <- colnames(brca$x)

which(names == names(which.max(apply(brca$x,2,mean))))

which(names == names(which.min(apply(brca$x,2,sd))))

# Question 2

brca_mean <- apply(brca$x,2,mean)
brca_sd <- apply(brca$x,2,sd)

brca$x_scale <- sweep(brca$x,2,brca_mean,"-")
brca$x_scale <- sweep(brca$x_scale,2,brca_sd,"/")

sd(brca$x_scale[,1])
median(brca$x_scale[,1])

# Question 3

pca <- prcomp(brca$x_scale)
summary(pca)

# Question 4

data.frame(pca$x[,1:2], tumor=brca$y) |> 
  ggplot(aes(PC1,PC2, fill = tumor))+
  geom_point(cex=3, pch=21) +
  coord_fixed(ratio = 1)

# Question 5
data.frame(type = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot() 
# Question 6
set.seed(1) 
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- brca$x_scale[test_index,]
test_y <- brca$y[test_index]
train_x <- brca$x_scale[-test_index,]
train_y <- brca$y[-test_index]

train_data <- data.frame(train_x, y = as.factor(train_y))
test_data <- data.frame(test_x, y = as.factor(test_y))
mean(train_y == "B")
mean(test_y == "B")

# Question 7

train_glm <- train(y ~.,train_data, method='glm')
train_glm

y_hat_glm <- predict(train_glm, test_data, type = "raw")
accuracy_glm <- confusionMatrix(y_hat_glm, test_y)$overall[["Accuracy"]]


# Question 8
set.seed(5)
train_loess <- train(y ~.,train_data, method='gamLoess')
train_loess

y_hat_loess <- predict(train_loess, test_data, type = "raw")
accuracy_loess <- confusionMatrix(y_hat_loess, test_y)$overall[["Accuracy"]]

# Quesion 9

set.seed(7)
k_val <- seq(3,21,2)
train_knn <- train(y ~ ., method = "knn", 
                   data = train_data,
                   tuneGrid = data.frame(k = k_val))

y_hat_knn <- predict(train_knn, test_x, type = "raw")
ggplot(train_knn, highlight = TRUE)

# Best tune
train_knn$bestTune

accuracy_knn <- confusionMatrix(predict(train_knn, test_x, type = "raw"),
                                test_data$y)$overall["Accuracy"]

# Question 10a
set.seed(9)
train_rf <- train(y ~ ., method = "rf", data = train_data,
                  tuneGrid = data.frame(mtry = c(3, 5, 7, 9)),
                  importance = TRUE)

y_hat_rf <- predict(train_rf, test_x, type = "raw")

train_rf$bestTune

accuracy_rf <- confusionMatrix(y_hat_rf,test_y)$overall["Accuracy"]
caret::varImp(train_rf, scale=FALSE)

# Question 11a

ensemble <- cbind(glm = y_hat_glm == "B", loess = y_hat_loess == "B", 
                  rf = y_hat_rf == "B", knn = y_hat_knn == "B")

ensemble_preds <- ifelse(rowMeans(ensemble) >= 0.5, "B", "M")
accuracy_ensemble <- mean(ensemble_preds == test_y)

table(accuracy_glm,accuracy_loess,accuracy_knn,accuracy_rf,accuracy_ensemble)
