library(tidyverse)
library(caret)
library(dslabs)

############################# QUESTION 1 ##############################
# Use the training set to build a model with several of the models
#  available from the caret package. We will test out 10 of the most
#  common machine learning models in this exercise:
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn",
            "gamLoess", "multinom", "qda", "rf", "adaboost")

# Apply all of these models using train with all the default parameters.
#  Run the following code to train the various models:
set.seed(1, sample.kind = "Rounding")
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models


############################# QUESTION 2 ##############################
# Now that you have all the trained models in a list, use sapply or map
#  to create a matrix of predictions for the test set. You should end
#  up with a matrix with length(mnist_27$test$y) rows and length(models)
#  columns.

# What are the dimensions of the matrix of predictions?
preds <- sapply(fits, function(fits){ 
    predict(fits, mnist_27$test)
}) 

dim(preds)


############################# QUESTION 3 ##############################
# Compute accuravy for each model. Report the mean accuracy.
accuracy_matrix <- ifelse(preds == mnist_27$test$y, 1, 0)
mean(accuracy_matrix)


############################# QUESTION 4 ##############################
# Build an ensemble prediction by majority vote and compute the accuracy
#  of the ensemble. What is the accuracy of the ensemble?
# user function for calculating the mode
getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

majority_preds <- apply(preds, 1, getMode)
mean(ifelse(majority_preds == mnist_27$test$y, 1, 0))

fits$results["Accuracy"]


############################# QUESTION 6 ##############################
# We could use the minimum accuracy estimates obtained from cross
#  validation with the training data for each model. Obtain these
#  estimates and save them in an object. Report the mean of these
#  training set accuracy estimates.
# What is the mean of these training set accuracy estimates?
acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)

############################# QUESTION 7 ##############################
# Now let's only consider the methods with an estimated accuracy of
#  greater than or equal to 0.8 when constructing the ensemble.
# What is the accuracy of the ensemble now? 
# rerun using only models from before
models <- c("glm", "naive_bayes", "knn", "gamLoess", "qda", "rf")

set.seed(1, sample.kind = "Rounding")
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

fits$glm
fits$naive_bayes
fits$knn
fits$gamLoess
fits$qda
fits$rf
(0.8010386 + 0.8179446 + 0.8229989 + 0.8412514 + 0.8321801 + 0.8144985)/6


