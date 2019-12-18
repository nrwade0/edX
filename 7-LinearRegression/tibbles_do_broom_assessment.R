library(tidyverse)
library(HistData)
library(broom)
library(Lahman)
data("GaltonFamilies")
set.seed(1, sample.kind = "Rounding")

galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

# Group by pair and summarize the number of observations in each group.
galton %>% group_by(pair) %>%
  summarize(n = n())

# Calculate the correlation coefficients for fathers and daughters,
#  fathers and sons, mothers and daughters and mothers and sons.
galton %>% 
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight")

# ---

# Use the Teams data frame from the Lahman package.
# Fit a multivariate linear regression model to obtain the effects of BB
#  and HR on Runs (R) in 1971. Use the tidy function in the broom package
#  to obtain the results in a data frame.
dat <- Teams %>% filter(yearID == 1971)
fit <- tidy(lm(R ~ BB + HR, data = dat))
fit


# Repeat the above exercise to find the effects of BB and HR on runs (R)
#  for every year from 1961 to 2018 using do and the broom package.
fit <- Teams %>% filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .)))
# Make a scatterplot of the estimate for the effect of BB on runs over time
#  and add a trend line with confidence intervals.
fit %>% filter(term == 'BB') %>% 
  select(yearID, estimate) %>%
  ggplot(aes(yearID, estimate)) + 
  geom_line() + geom_smooth(method="lm")
  

# Fit a linear model on the results from Question 10 to determine the effect
#  of year on the impact of BB.
fit<- Teams %>% filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>% 
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = TRUE))

fit2 <- fit %>% filter(term == "BB")
fit3 <- lm(estimate ~ yearID, data= fit2)

glance(fit3)













