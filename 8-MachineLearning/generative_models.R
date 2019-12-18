# import libraries and tissue_gene_expression dataset
library(tidyverse)
library(caret)
library(dslabs)
data("tissue_gene_expression")


############################# QUESTION 1 #################################
# Create a dataset of samples from just cerebellum and hippocampus and a predictor
#  matrix with 10 randomly selected columns using the following code:
set.seed(1993, sample.kind="Rounding")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
# Use the train function to estimate the accuracy of LDA. For this question,
#  use the entire tissue_gene_expression dataset: do not split it into training
#  and test sets (understand this can lead to overfitting).
# What is the accuracy?
train_lda <- train(x, y, method = "lda")
confusionMatrix.train(train_lda)


############################# QUESTION 2 #################################
# In this case, LDA fits two 10-dimensional normal distributions. Look at the
#  fitted model by looking at the finalModel component of the result of train.
#  Notice there is a component called means that includes the estimated means
#  of both distributions. Plot the mean vectors against each other and determine
#  which predictors (genes) appear to be driving the algorithm.

# Which TWO genes appear to be driving the algorithm?
train_lda$finalModel

t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()


############################# QUESTION 3 #################################
# Repeat the exercise in Q1 with QDA.
#  set up data
set.seed(1993, sample.kind="Rounding")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
# Use the train function to estimate the accuracy of QDA. What is the accuracy?
train_qda <- train(x, y, method = "qda")
confusionMatrix.train(train_qda)


############################# QUESTION 4 #################################
# Which TWO genes drive the algorithm when using QDA instead of LDA?
train_qda$finalModel

t(train_qda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()


############################# QUESTION 5 #################################
# One thing we saw in the previous plots is that the values of the predictors
#  correlate in both groups: some predictors are low in both groups and others
#  high in both groups. The mean value of each predictor found in colMeans(x)
#  is not informative or useful for prediction and often for purposes of
#  interpretation, it is useful to center or scale each column. This can be
#  achieved with the preProcess argument in train. Re-run LDA with
#  preProcess = "center". Note that accuracy does not change, but it is now
#  easier to identify the predictors that differ more between groups than based
#  on the plot made in Q2.
train_lda_centered <- train(x, y, method = "lda", preProcess = "center")
confusionMatrix.train(train_lda_centered)

# Which TWO genes drive the algorithm after performing the scaling?
train_lda_centered$finalModel

t(train_lda_centered$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()


############################# QUESTION 6 #################################
# Repeat the LDA analysis from Q5 but using all tissue types. Use the following
#  code to create your dataset:
set.seed(1993, sample.kind="Rounding")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

# What is the accuracy of this model?
train_lda <- train(x, y, method = "lda")
confusionMatrix.train(train_lda)








