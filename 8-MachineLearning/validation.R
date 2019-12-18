# cross validation comprehension check
library(tidyverse)
library(dslabs)
library(caret)

############################# QUESTION 1 ##########################
# create set of random predictors
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

# test correlation between x_subset and y (< 0.50 because non-correlated)
fit <- train(x_subset, y, method = "glm")
fit$results


############################# QUESTION 2 ##########################
# Search predictors for most predictive of the outcome
install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)

# create array of p-values
pvals <- tt$p.value


############################# QUESTION 3 ##########################
# Create an index ind with the column numbers of the predictors that were
#  "statistically significantly" associated with y. Use a p-value cutoff
#  of 0.01 to define "statistically significantly."
# How many predictors survive this cutoff?
ind <- which(pvals < 0.01)
length(ind)


############################# QUESTION 4 ##########################
# Now re-run the cross-validation after redefinining x_subset to be the
#  subset of x defined by the columns showing "statistically significant"
#  association with y.
x_subset <- x[ ,ind]

fit <- train(x_subset, y, method = "glm")
fit$results


############################# QUESTION 5 ##########################
# Re-run the cross-validation again, but this time using kNN. Try out the
#  following grid k = seq(101, 301, 25) of tuning parameters. Make a plot
#  of the resulting accuracies.
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)


############################# QUESTION 7 ##########################
# Use the train function with kNN to select the best k for predicting tissue
#  from gene expression on the tissue_gene_expression dataset from dslabs.
#  Try k = seq(1,7,2) for tuning parameters. For this question, do not split
#  the data into test and train sets (understand this can lead to overfitting,
#  but ignore this for now).
# What value of k results in the highest accuracy?
data("tissue_gene_expression")
fit <- with(tissue_gene_expression, train(x, y, method = "knn", tuneGrid = data.frame( k = seq(1, 7, 2))))
ggplot(fit)
fit$results


#######
########## BOOTSTRAP COMPREHENSION CHECK #
#######
############################# QUESTION 1 ##########################
# The createResample function can be used to create bootstrap samples.
#  For example, we can create 10 bootstrap samples for the mnist_27
#  dataset like this:
set.seed(1995,  sample.kind="Rounding")
indexes <- createResample(mnist_27$train$y, 10)
# How many times do 3, 4, and 7 appear in the first resampled index?
sum(indexes$Resample01 == 3)
sum(indexes$Resample01 == 4)
sum(indexes$Resample01 == 7)


############################# QUESTION 2 ##########################
# We see that some numbers appear more than once and others appear no
#  times. This has to be this way for each dataset to be independent.
#  Repeat the exercise for all the resampled indexes.
# What is the total number of times that 3 appears in all of the resampled indexes?
threes = sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(threes)


############################# QUESTION 3 ##########################
# Generate a random dataset: 
y <- rnorm(100, 0, 1)
# Estimate the 75th quantile, which we know is
qnorm(0.75)
# with the sample quantile:
quantile(y, 0.75)

# Set the seed to 1 and perform a Monte Carlo simulation with 10,000 repetitions,
#  generating the random dataset and estimating the 75th quantile each time.
#  What is the expected value and standard error of the 75th quantile?
quant <- replicate(10000, {
  y <- rnorm(100,0,1)
  quantile(y, 0.75)
})

mean(quant)
sd(quant)


############################# QUESTION 4 ##########################
# In practice, we can't run a Monte Carlo simulation...
# Set the seed to 1 again after generating y and use 10 bootstrap samples
#  to estimate the expected value and standard error of the 75th quantile.
set.seed(1, sample.kind="Rounding")
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind="Rounding")
indexes <- createResample(y, 10)

# use indexes from dataset y and calculate 75% quantile
tt = sapply(indexes, function(ind){
  quantile(y[ind], 0.75)
})
mean(tt)
sd(tt)


############################# QUESTION 5 ##########################
# Same as 4 but now 10000 times
set.seed(1, sample.kind="Rounding")
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind="Rounding")
indexes <- createResample(y, 10000)

# use indexes from dataset y and calculate 75% quantile
tt = sapply(indexes, function(ind){
  quantile(y[ind], 0.75)
})
mean(tt)
sd(tt)



