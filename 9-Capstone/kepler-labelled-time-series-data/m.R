library(tidyverse)
library(caret)

# https://www.kaggle.com/keplersmachines/kepler-labelled-time-series-data
# https://medium.com/@gabogarza/exoplanet-hunting-with-machine-learning-and-kepler-data-recall-100-155e1ddeaa95

train <- read.csv("exoTrain.csv")
test <- read.csv("exoTest.csv")

train <- train %>% mutate(LABEL = LABEL-1)
test <- test %>% mutate(LABEL = LABEL-1)

model <- glm(LABEL ~ ., data = train, family = binomial)
predict.glm(model, data = test)
