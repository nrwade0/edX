library(tidyverse)
library(dslabs)
library(lubridate)
library(caret)

data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type


########################### QUESTION 1 ###########################
# compute proportion of women in online and inclass types
dat %>% filter(type == 'online' & sex == 'Female') %>%
  nrow


########################### QUESTION 2 ###########################
# Report the accuracy of predictions of sex based on type
y_hat <- ifelse(x == 'inclass', "Female", "Male") %>% factor()
mean(y == y_hat)


########################### QUESTION 3/4/5/6 ###########################
# Show confusion matric with sensitivity, specifcity, and prevalence
confusionMatrix(y_hat, factor(dat$sex))


########################### QUESTION 7 ###########################
library(purrr)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species


set.seed(2, sample.kind = "Rounding")
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]


########################### QUESTION 8 ###########################
# Perform a simple search to find the cutoff that produces the highest accuracy,
#  predicting virginica if greater than the cutoff and versicolor otherwise.
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5], 2, foo)
dataa <- sapply(predictions,max)


########################### QUESTION 9 ###########################
# Perform a simple search to find the cutoff that produces the highest accuracy,
#  predicting virginica if greater than the cutoff and versicolor otherwise.
range <- seq(min(train$Petal.Length), max(train$Petal.Length), by=0.1)
range[18]

foobar <- function(x){
  y_hat <- ifelse(x>4.7,'virginica','versicolor')
  mean(y_hat==test$Species)
}
predictions <- apply(test[,-5], 2, foobar)
dataa <- sapply(predictions, max)


########################### QUESTION 10 ###########################
# Given that we know the test data, we can treat it like we did our training data
#  to see if the same feature with a different cutoff will optimize our predictions.
#  Which feature best optimizes our overall accuracy?
foofoo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}
predictions <- apply(test[,-5], 2, foofoo)
dataa <- sapply(predictions, max)


########################### QUESTION 10 ###########################
plot(iris,pch=21,bg=iris$Species)
# Optimize the the cutoffs for Petal.Length and Petal.Width separately in the train
#  dataset by using the seq function with increments of 0.1. Then, report the overall
#  accuracy when applied to the test dataset by creating a rule that predicts virginica
#  if Petal.Length is greater than the length cutoff OR Petal.Width is greater than
#  the width cutoff, and versicolor otherwise.
foofoofoo <- function(x){
  y_hat <- ifelse(test$Petal.Length>4.7 | test$Sepal.Length>6.7,'virginica','versicolor')
  mean(y_hat==test$Species)
}

# What is the overall accuracy for the test data now?
predictions <- apply(test[,-5], 2, foofoofoo)
dataa <- sapply(predictions, max)


