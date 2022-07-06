library(dslabs)
data(tissue_gene_expression)
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

head(tissue_gene_expression)
class(tissue_gene_expression)


# Q2
d <- as.matrix(dist(tissue_gene_expression$x))

ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

#Q3
image(d,axes = FALSE, xlab="", ylab="")
text(expand.grid(1:ncol(d), 1:ncol(d)), sprintf("%0.1f", d), cex=0.6)
