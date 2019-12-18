library(tidyverse)



########################### QUESTION 1 ###########################
# In a previous module, we covered Bayes' theorem and the Bayesian paradigm.
#  Conditional probabilities are a fundamental part of this previous covered rule.

# P(A|B) = P(B|A)*P(A)/P(B)

# Assume: 
#  - The test is positive 85% of the time when tested on a patient with the disease
#    (high sensitivity): P(test+ | disease) = 0.85
#  - The test is negative 90% of the time when tested on a healthy patient (high
#    specificity): P(test- | healthy) = 90%
#  - The disease if prevalent in 2% of the community: P(disease) = 0.02

# Using Bayes' theorem, calculate the probability that you have the disease if the
#  test is positive.
# P(disease | test+) = P(test+ | diseased) * P(diseased)/P(test+), P(test+) = P(test+|disease)P(disease) + P(test+|healthy)P(healthy)
 0.85*0.02/(0.85*0.2 + 0.1*0.98) # = 0.1478


########################### QUESTION 2 ###########################
# What is the probability that a test is positive?
set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))
mean(test == 1)

########################### QUESTION 3 ###########################
# What is the probability that an individual has the disease if the test is negative?
mean(test==0 & disease==1)


########################### QUESTION 4 ###########################
# What is the probability that you have the disease if the test is positive?
#  Remember: calculate the conditional probability the disease is positive assuming a positive test.
mean(disease[test==1] == 1)


########################### QUESTION 5 ###########################
# If the test is positive, what is the relative risk of having the disease?
#  First calculate the probability of having the disease given a positive test,
#  then normalize it against the disease prevalence.
mean(disease[test==1]==1)/mean(disease==1)


########################### QUESTION 6 ###########################
library(dslabs)
data("heights")

ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data=.)








