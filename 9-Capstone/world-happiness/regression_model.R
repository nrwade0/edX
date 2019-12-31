# World Happiness Ranks dataset found here:
# https://www.kaggle.com/unsdsn/world-happiness

# import libraries
library(tidyverse)
library(caret)
library(DescTools)
library(ggcorrplot)


# ----- GOAL -----
# Can we make a prediction model that will help us predict happiness
#  score based on multiple variables? 



# ----- Import dataset -----
data <- read.csv("2019.csv")



# ----- Explore and visualize data -----

# shows an organized section of data frame
glimpse(data)

# shows structure of the data frame
str(data)

# shows desxcriptive statistics of each category,
#  should show everything you need
summary(data)

# shows highest 10 scores
head(data, n=10)

# shows lowest 10 scores
tail(data, n=10)

# histogram of scores
hist(data$Score, freq=TRUE, col="black", border="white", 
     main="2019 Happiness Scores", xlab="Score", ylab="Count")

# plot gdp per cap vs score
ggplot(data = data, aes(x = Score, y = GDP.per.capita)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)

# plot generosity vs score
ggplot(data = data, aes(x = Score, Generosity)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)

# plot life expectancy vs score
ggplot(data = data, aes(x = Score, Healthy.life.expectancy)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)

# Thus, moving forward, let us explore the correlation between the other predictors (economy, family, trust, etc.) and happiness score
temp <- data[, c(3,4,5,6,7,8,9)]
cormat <- signif(cor(temp), 2)
ggcorrplot(cormat)


# ----- Summation model -----
# find  predicted score by sum method and calculate the corresponding RMSE
sum_model <- data %>% mutate(pred_score = GDP.per.capita +
                               Social.support +
                               Healthy.life.expectancy +
                               Freedom.to.make.life.choices + 
                               Generosity + 
                               Perceptions.of.corruption + 
                               1.85, 
                             RMSE = RMSE(Score, pred_score))

# show top results of the summation model
sum_model %>%
  filter(Overall.rank <= 5) %>%
  select(Overall.rank, Country.or.region, Score, pred_score, RMSE)



# ----- Develop generalized linear model -----
# using generalized linear model to predict score based on
#  all other data points


# --- test for an appropriate probability in data partitioning
ps <- seq(from=.30, to=.90, by=.01)

rmses <- sapply(ps, function(p){
  train_index <- createDataPartition(data$Score,
                                     times=1,
                                     p=p,
                                     list=FALSE)
  train <- data[train_index,]
  test <- data[-train_index,]
  fit <- glm(Score ~ GDP.per.capita +
               Social.support +
               Healthy.life.expectancy + 
               Freedom.to.make.life.choices + 
               Generosity + 
               Perceptions.of.corruption, 
             data = train)
  test <- test %>% 
    mutate(pred_score = predict.glm(fit, newdata=test))
  RMSE(test$Score, test$pred_score)
})

# no real clear winner in terms of best accuracy in probabilities
plot(ps, rmses)
ps[which.min(rmses)]
min(rmses)

rm(ps, rmses)

# ----- Data partitioning -----
# just using p=0.70
train_index <- createDataPartition(data$Score, times=1, p=0.70, list=FALSE)
train <- data[train_index,]
test <- data[-train_index,]



# --- back to our model, p used above in data separation
fit <- glm(Score ~ GDP.per.capita +
             Social.support +
             Healthy.life.expectancy + 
             Freedom.to.make.life.choices + 
             Generosity + 
             Perceptions.of.corruption, 
           data = train)

# add predicted scores to our 'data' data frame
results <- test %>% 
  mutate(pred_score = predict.glm(fit, newdata=test))

# plot predicted scores vs actual scores
# also plot 1 to 1 line
ggplot(data = results, aes(Score, pred_score)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(color='red')

# print rmse
RMSE(results$Score, results$pred_score)

# print coefficients of fitted model
fit$coefficients



# ----- try fitting ALL year data. Country/region and rank is not used
# anyways, just focus on the scores and other components.
# cannot use beyond 2019/2019 because the parameters were changed that year

# ----- Import 2018 dataset and keep only used columns
data18 <- read.csv("2018.csv")
data18$Overall.rank <- NULL
data18$Country.or.region <- NULL

# remove unused columns in 19 data frame for merging
data$Overall.rank <- NULL
data$Country.or.region <- NULL

# turn corruption column from factor to numeric, turn NAs to 0
data18$Perceptions.of.corruption <- as.numeric(as.character(data18$Perceptions.of.corruption))
data18[is.na(data18)] <- 0

# full data set of both 2019, 2018 data
full_data <- rbind(data, data18)



# ------ full data fit -----
# partition full dataset
train_index <- createDataPartition(full_data$Score, 
                                   times=1, 
                                   p=0.70, 
                                   list=FALSE)
train <- full_data[train_index,]
test <- full_data[-train_index,]

# fit a model on train data
fit_full <- glm(Score ~ GDP.per.capita + 
                  Social.support +
                  Healthy.life.expectancy + 
                  Freedom.to.make.life.choices + 
                  Generosity + 
                  Perceptions.of.corruption, 
                data = train)

# add predicted scores to our 'data' data frame
results <- test %>% 
  mutate(pred_score = predict.glm(fit_full, newdata=test))

# plot predicted scores vs actual scores
# also plot 1 to 1 line
ggplot(data = results, aes(Score, pred_score)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(color='red')

# print rmse
RMSE(results$Score, results$pred_score)

# print coefficients of fitted model
fit_full$coefficients



# ----- try fit again without generosity
fit_full_2 <- glm(Score ~ GDP.per.capita + 
                  Social.support +
                  Healthy.life.expectancy + 
                  Freedom.to.make.life.choices + 
                  Perceptions.of.corruption, 
                data = train)

# add predicted scores to our 'data' data frame
results <- test %>% 
  mutate(pred_score = predict.glm(fit_full_2, newdata=test))

# plot predicted scores vs actual scores
# also plot 1 to 1 line
ggplot(data = results, aes(Score, pred_score)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(color='red')


# ----- using rmse instead of cor: https://www.r-bloggers.com/dont-use-correlation-to-track-prediction-performance/
# print residual sum of squares
RMSE(results$Score, results$pred_score)

# print coefficients of fitted model
fit_full_2$coefficients

