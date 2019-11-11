options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

ggplot(titanic, aes(x=Age, group=Sex, fill=Sex, alpha=0.5)) + geom_density()

# Make a density plot of age filled by survival status.
#  Change the y-axis to count and set alpha = 0.2.
# Which age group is the only group more likely to survive than die?
ggplot(titanic, aes(x=Age, fill=Survived, alpha=0.2)) +
  geom_density()

# Filter the data to remove individuals who paid a fare of 0.
#  Make a boxplot of fare grouped by survival status.
#  Try a log2 transformation of fares.
#  Add the data points with jitter and alpha blending.
titanic %>% filter(Fare != 0) %>%
  ggplot(aes(x=Survived, y=Fare)) +
  geom_boxplot() +
  geom_jitter()

# The Pclass variable corresponds to the passenger class. Make three barplots.
#  For the first, make a basic barplot of passenger class filled by survival.
titanic %>% ggplot(aes(x=Pclass, fill=Survived)) +
  geom_bar()
#  For the second, make the same barplot but use the argument
#   position = position_fill() to show relative proportions in
#   each group instead of counts.
titanic %>% ggplot(aes(x=Pclass, fill=Survived)) +
  geom_bar(position = position_fill())
#  For the third, make a barplot of survival filled by passenger
#   class using position = position_fill().
titanic %>% ggplot(aes(x=Survived, fill=Pclass)) +
  geom_bar(position = position_fill())

#Create a grid of density plots for age, filled by survival status,
#  with count on the y-axis, faceted by sex and passenger class.
titanic %>% ggplot(aes(x=Age, y= ..count.., fill=Survived)) +
  geom_density(alpha=0.5) +
  facet_grid(Sex ~ Pclass)










