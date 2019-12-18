library(tidyverse)
library(Lahman)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(win_rate = W/G, E_per_game = E/G) %>%
  ggplot(aes(win_rate, E_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(doubles = X2B, triples = X3B) %>%
  ggplot(aes(doubles, triples)) + 
  geom_point(alpha = 0.5)


# --- Correlations


Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  summarize(corr_coeff = cor(AB_per_game, R_per_game))

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(dub_per_game = X2B/G, trip_per_game = X3B/G) %>%
  summarize(corr_coeff = cor(dub_per_game, trip_per_game))


# --- Stratification


set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

# Calculate
# - the mean and standard deviation of mothers' heights,
# - the mean and standard deviation of daughters' heights,
# - the correlaton coefficient between mother and daughter heights.

# Mean of mothers' heights
mu_mom <- mean(female_heights$mother) 

# Standard deviation of mothers' heights
sd_mom <- sd(female_heights$mother)  

# Mean of daughters' heights
mu_dau <- mean(female_heights$daughter)
 
# Standard deviation of daughters' heights
sd_dau <- sd(female_heights$daughter)  

# Correlation coefficient
cc <- cor(female_heights$mother, female_heights$daughter)


# Calculate the slope and intercept of the regression line predicting
#  daughters' heights given mothers' heights. Given an increase in mother's
#  height by 1 inch, how many inches is the daughter's height expected to change?

# Slope of regression line predicting daughters' height from mothers' heights
m <- cc*sd_dau/sd_mom  

# Intercept of regression line predicting daughters' height from mothers' heights
b <- mu_dau - m*mu_mom

# Change in daughter's height in inches given a 1 inch increase in the mother's height
(m*2+b) - (m*1+b) # essentially the slope

# percent variablity in daughter due to mother
cc*cc*100

# The conditional expected value of her daughter's height given the mother's height (60)
x = 60
m*x+b















