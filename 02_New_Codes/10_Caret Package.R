library(rpart)
library(dslabs)
set.seed(1991)
data(tissue_gene_expression)

# Question 1
fit <- with(tissue_gene_expression, train(x, y, method = "rpart",
                                          tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))
ggplot(fit)
fit$results

# Question 2

fit <- with(tissue_gene_expression, train(x, y, method = "rpart",
                                          tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),
                                         control = rpart.control(minsplit = 0)))
ggplot(fit)
fit$results
plot(fit)

# Question 3
plot(fit$finalModel, margin = 0.1)
text(fit$finalModel, cex = 0.75)

# Question 4

fit <- with(tissue_gene_expression, train(x, y, method = "rf",
                                          tuneGrid = data.frame(mtry = seq(50, 200, 25)),
                                          nodesize = 1))
ggplot(fit)
fit$results
plot(fit)
