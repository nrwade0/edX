library(tidyverse)
library(caret)
set.seed(1, sample.kind="Rounding")

# set up data
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))


################################ QUESTION 1 #############################
# Within a replicate loop,
#  (1) partition the dataset into test and training sets with p=0.5 and using dat$y
#      to generate your indices,
#  (2) train a linear model predicting y from x,
#  (3) generate predictions on the test set, and
#  (4) calculate the RMSE of that model.
# Then, report the mean and standard deviation (SD) of the RMSEs from all 100 models.
nick <- replicate(n, {
  test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  model <- lm(y ~ x, data=train)
  y_hat <- predict.lm(model, test)
  RMSE(test$y, y_hat)
})

mean(nick)
sd(nick)


################################ QUESTION 2 #############################
# Write a function that takes a size n, then
#  (1) builds a dataset using the code provided in Q1 but with n observations
#      instead of 100 and without the set.seed(1),
#  (2) runs the replicate loop that you wrote to answer Q1, which builds 100
#      linear models and returns a vector of RMSEs, and
#  (3) calculates the mean and standard deviation.
n <- c(100, 500, 1000, 5000, 10000)

# function for question 2
ques2 <- function(n){
  # build data set
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  # builds 100 linear models, returns vector of RMSEs
  nicks <- replicate(100, {
    test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
    test <- dat[test_index,]
    train <- dat[-test_index,]
    model <- lm(y ~ x, data=train)
    y_hat <- predict.lm(model, test)
    RMSE(test$y, y_hat)
  })
  
  # calculate mean and standard deviation
  mean(nicks)
#  sd(nicks)
}

# apply function to various n's
set.seed(1, sample.kind="Rounding")
wade <- mapply(FUN=ques2, n)


################################ QUESTION 4 #############################
# Now repeat the exercise from Q1, this time making the correlation between x and y
#  larger, as in the following code:
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
set.seed(1)


nick <- replicate(n, {
  test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  model <- lm(y ~ x, data=train)
  y_hat <- predict.lm(model, test)
  RMSE(test$y, y_hat)
})

mean(nick)
sd(nick)


################################ QUESTION 6 #############################
# Note that y is correlated with both x_1 and x_2 but the two predictors are
#  independent of each other, as seen by cor(dat).
# Set the seed to 1, then use the caret package to partition into a test and
#  training set of equal size. Compare the RMSE when using just x_1, just x_2
#  and both x_1 and x_2. Train a linear model for each.

# creat new data set
set.seed(1, sample.kind = "Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
test <- dat[test_index,]
train <- dat[-test_index,]
model <- lm(y ~ x_1 + x_2, data=train)
y_hat <- predict.lm(model, test)
RMSE(test$y, y_hat)
# y ~ x_1 = 0.6708
# y ~ x_2 = 0.6775
# y ~ x_1 + x_2 = 0.3552


################################ QUESTION 6 #############################
# Repeat the exercise from Q6 but now create an example in which x_1 and x_2 are
#  highly correlated.
set.seed(1, sample.kind = "Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
test <- dat[test_index,]
train <- dat[-test_index,]
model <- lm(y ~ x_1 + x_2, data=train)
y_hat <- predict.lm(model, test)
RMSE(test$y, y_hat)
# y ~ x_1 = 0.659
# y ~ x_2 = 0.640
# y ~ x_1 + x_2 = 0.659



################################ QUESTION 1 #############################
# Logistic regression comprehension check

# Make data
set.seed(2, sample.kind="Rounding") 
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

# x depends on a binary outcome of y
dat$train %>% ggplot(aes(x, color = y)) + geom_density()

# Set the seed to 1, then use the make_data function defined above to generate
#  25 different datasets with mu_1 <- seq(0, 3, len=25). Perform logistic regression
#  on each of the 25 different datasets (predict 1 if p>0.5) and plot accuracy
#  (res in the figures) vs mu_1 (delta in the figures).”
set.seed(1, sample.kind="Rounding")
delta <- seq(0, 3, len = 25)
res <- sapply(delta, function(d){
  dat <- make_data(mu_1 = d)
  fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
  y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
  mean(y_hat_glm == dat$test$y)
})
qplot(delta, res)










