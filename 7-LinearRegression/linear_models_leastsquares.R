library(tidyverse)
library(HistData)
library(Lahman)
data("GaltonFamilies")

set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)




teams <- Teams %>% filter(yearID %in% 1961:2001)
# fit regression line to predict runs by walks and homeruns
fit <- lm(R ~ BB + HR, data = teams)
fit

# summary statistics
summary(fit)



# plotting the model for heights
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")


model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)
ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

# ---

set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
options(digits = 3)    # report 3 significant digits
female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight) 

# Fit a linear regression model predicting the mothers' heights using daughters' heights.
fit <- lm(mother ~ daughter, data = female_heights)
summary(fit) # summary statistics
# What is the slope of the model? 0.3103
# What the intercept of the model? 44.1785


# Predict mothers' heights using the model.
# What is the predicted height of the first mother in the dataset?
Y_hat <- predict(fit, se.fit = TRUE)
Y_hat[[1]]

# What is the actual height of the first mother in the dataset?
female_heights$mother[[1]]

# ---

# New table of 2002 baseball statistics for walks and singles
# define per plate appearance statistics,
# keeping only players with more than 100 plate appearances.
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

# Now compute a similar table but with rates computed over 1999-2001.
#  Keep only rows from 1999-2001 where players have 100 or more plate appearances,
#  then calculate the average single rate (mean_singles) and average BB rate (mean_bb)
#  per player over those three seasons.
bat_9901 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))
  select(playerID, mean_singles, mean_bb)

# How many players had a single rate mean_singles of greater than 0.2 per plate
#  appearance over 1999-2001?
bat_9901 %>% filter(mean_singles > 0.2) # 46

# How many players had a BB rate mean_bb of greater than 0.2 per plate appearance
#  over 1999-2001?
bat_9901 %>% filter(mean_bb > 0.2) # 3

# Use inner_join to combine the bat_02 table with the table of 1999-2001 rate
#  averages you created in the previous question.
j <- inner_join(bat_02, bat_9901)
# What is the correlation between 2002 singles rates and 1999-2001 average singles rates?
cor(j$singles, j$mean_singles)  

# What is the correlation between 2002 BB rates and 1999-2001 average BB rates?
cor(j$bb, j$mean_bb)  


# Make scatterplots of mean_singles versus singles and mean_bb versus bb.
j %>% ggplot(aes(singles, mean_singles)) + 
  geom_point()
j %>% ggplot(aes(bb, mean_bb)) + 
  geom_point()

# Fit a linear model to predict 2002 singles given 1999-2001 mean_singles.
fit <- lm(singles ~ mean_singles, data = j)
summary(fit) # summary statistics

# Fit a linear model to predict 2002 walks given 1999-2001 mean_bb.
fit <- lm(bb ~ mean_bb, data = j)
summary(fit) # summary statistics










