library(dslabs)
data("tissue_gene_expression")
# This dataset includes a matrix x:
  
dim(tissue_gene_expression$x)
# This matrix has the gene expression levels of 500 genes from 189 biological samples representing seven different tissues. The tissue type is stored in y:
  
table(tissue_gene_expression$y)

d <- dist(tissue_gene_expression$x)
as.matrix(d)[c(1,2,39,40,73,74), c(1,2,39,40,73,74)]



####### knn
# Previously, we used logistic regression to predict sex based on height. Now we
#  are going to use knn to do the same. Set the seed to 1, then use the caret
#  package to partition the dslabs "heights" data into a training and test set of
#  equal size. Use the sapply function to perform knn with k values of
#  seq(1, 101, 3) and calculate F1 scores with the F_meas function using the
#  default value of the argument relevant.
library(caret)
library(tidyverse)
data("heights")

# create train and test sets
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(heights$sex, times=1, p=0.5, list=FALSE)
test <- heights[test_index,]
train <- heights[-test_index,]

y <- train$sex
x <- train$height

# perform knn with a range of k values
ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train, k = k)
  y_hat <- predict(fit, test, type = "class") %>% factor(levels = levels(train$sex))
  F_meas(data = y_hat, reference = test$sex)
})

plot(ks, F_1)

# What is the max value of F_1?
max(F_1)

# At what value of k does the max occur?
ks[which.max(F_1)]


# -------------
# Split the data into training and test sets, and report the accuracy you obtain.
# Try it for k = 1, 3, 5, 7, 9, 11. Set the seed to 1 before splitting the data.
library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")
set.seed(1, sample.kind = "Rounding")
testindex_gene <- createDataPartition(tissue_gene_expression$y, times=1, p=0.5, list=FALSE)

train_set_x <- tissue_gene_expression$x[-testindex_gene,] #matrix
train_set_y <- tissue_gene_expression$y[-testindex_gene] #factor

test_set_x <- tissue_gene_expression$x[testindex_gene,] #matrix
test_set_y <- tissue_gene_expression$y[testindex_gene] #factor

trainset_gene <- list('x' = train_set_x, 'y' = train_set_y)
testset_gene <- list('x' = test_set_x, 'y' = test_set_y)

# perform knn with a range of k values
ks <- seq(from=1, to=13, by=2)
F_1 <- sapply(ks, function(k){
  fit <- knn3(y ~ x, data=trainset_gene, k=k)
  y_hat <- predict(fit, testset_gene, type = "class") %>%
    factor(levels = levels(trainset_gene$y))
  F_meas(data=y_hat, reference=testset_gene$y)
})

plot(ks, F_1)

# What is the max value of F_1?
max(F_1)

# At what value of k does the max occur?
ks[which.max(F_1)]



fit <- knn3(y ~ x, data=trainset_gene, k=4)
y_hat <- predict(fit, testset_gene, type = "class") %>%
  factor(levels = levels(trainset_gene$y))
F_meas(data=y_hat, reference=testset_gene$y)

confusionMatrix(y_hat, testset_gene$y)







