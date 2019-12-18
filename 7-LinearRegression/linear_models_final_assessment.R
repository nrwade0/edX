library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)


###################### QUESTION 1 a/b/c #####################
# Use runs (R) per game to predict average attendance.
# For every 1 run scored per game, attendance increases by how much?
Teams_small %>% mutate(rpg = R/G) %>%
  lm(avg_attendance ~ rpg, data = .)

# Use home runs (HR) per game to predict average attendance.
# For every 1 home run hit per game, attendance increases by how much?
Teams_small %>% mutate(hrpg = HR/G) %>%
  lm(avg_attendance ~ hrpg, data = .)


# Use number of wins to predict attendance; do not normalize for number of games.
# For every game won in a season, how much does average attendance increase?
Teams_small %>% lm(avg_attendance ~ W, data = .) # slope

# Suppose a team won zero games in a season.
# Predict the average attendance.
Teams_small %>% lm(avg_attendance ~ W, data = .) # intercept


# Use year to predict average attendance.
# How much does average attendance increase each year?
Teams_small %>% lm(avg_attendance ~ yearID, data = .)


###################### QUESTION 2 a #####################
# Game wins, runs per game and home runs per game are positively correlated with attendance.
#  We saw in the course material that runs per game and home runs per game are correlated
#  with each other. Are wins and runs per game or wins and home runs per game correlated?
  
# What is the correlation coefficient for wins and runs per game?
corr <- Teams_small %>% mutate(rpg = R/G)
cor(x = corr$W, y = corr$rpg)

# What is the correlation coefficient for wins and home runs per game?
corr <- Teams_small %>% mutate(hrpg = HR/G)
cor(x = corr$W, y = corr$hrpg)


###################### QUESTION 3 a/b/c #####################
# Stratify Teams_small by wins: divide number of wins by 10 and then round to the nearest
#  integer. Keep only strata 5 through 10, which have 20 or more data points.
strat_teams <- Teams_small %>%
  mutate(strat = round(W/10)) %>%
  group_by(strat) %>%
  filter(strat %in% 5:10)

# How many observations are in the 8 win strata?
strat_teams %>% filter(strat == 8) %>% count()


# Calculate the slope of the regression line predicting average attendance given
#  runs per game for each of the win strata. Which win stratum has the largest
#  regression line slope?
strat_teams %>% filter(strat == 5) %>%
  mutate(rpg = R/G) %>%
  lm(avg_attendance ~ rpg, data = .)

# Calculate the slope of the regression line predicting average attendance given
#  HR per game for each of the win strata. Which win stratum has the largest
#  regression line slope?
strat_teams %>% filter(strat == 5) %>%
  mutate(hrpg = HR/G) %>%
  lm(avg_attendance ~ hrpg, data = .)


###################### QUESTION 4 a #####################
# Fit a multivariate regression determining the effects of runs per game,
#  home runs per game, wins, and year on average attendance. Use the original
#  Teams_small wins column, not the win strata from question 3.

# build model
fit <- Teams_small %>% mutate(rpg = R/G, hrpg = HR/G) %>%
  lm(avg_attendance ~ rpg + hrpg + W + yearID, data = .)

# first prediction
guess <- data.frame(
  rpg = 5,
  hrpg = 1.2,
  W = 80,
  yearID = 2002
)
predict(fit, guess)

# second prediction
guess <- data.frame(
  rpg = 5,
  hrpg = 1.2,
  W = 80,
  yearID = 1960
)
predict(fit, guess)


###################### QUESTION 5 a#####################
# Use your model from Question 4 to predict average attendance for teams in 2002
#  in the original Teams data frame.

pred_teams <- Teams %>% filter(yearID == 2002) %>%
  mutate(rpg = R/G, hrpg = HR/G) %>%
  mutate(pred_attendance = predict(fit, data.frame(rpg = rpg, hrpg = hrpg, W = W, yearID = yearID)))

cor(pred_teams$attendance, pred_teams$pred_attendance)















