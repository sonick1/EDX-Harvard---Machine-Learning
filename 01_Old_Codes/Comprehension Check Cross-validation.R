library(tidyverse)
library(caret)

# set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

# Q1
fit <- train(x_subset, y, method = "glm")
fit$results

install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)

# Q2
pvals <- tt$p.value

# Q3
ind <- which(pvals <= 0.01)
length(ind)

# Q4

x_subset <- x[,ind]
fit <- train(x_subset, y, method = "glm")
fit$results

# Q5
k = seq(101, 301, 25)

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

# Q7
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(1, 7, 2)))
ggplot(fit)
data("tissue_gene_expression")
fit <- with(tissue_gene_expression, train(x, y, method = "knn", tuneGrid = data.frame( k = seq(1, 7, 2))))
ggplot(fit)
fit$results
