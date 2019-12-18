library(tidyverse)
library(dslabs)
library(lubridate)
data("movielens")


############################# QUESTION 1 ################################
# Compute the number of ratings for each movie and then plot it against
#  the year the movie came out. Use the square root transformation on
#  the counts.
# What year has the highest median number of ratings?
movielens %>% group_by(year) %>%
  summarize(n_ratings = n()) %>%
  print(n=82) %>%
  ggplot(aes(year, sqrt(n_ratings))) +
  geom_line()


############################# QUESTION 2 ################################
# What is the average rating for the movie The Shawshank Redemption?
movielens %>% filter(title=="Shawshank Redemption, The") %>%
  summarize(mean_rating = mean(rating))

# What is the average number of ratings per year for the movie Forrest
#  Gump?
movielens %>% filter(title=="Forrest Gump") %>%
  summarize(n_ratings_per_year = n()/(2018-1994))


############################# QUESTION 3 ################################
# Stratify the post-1993 movies by ratings per year and compute their
#  average ratings. To calculate number of ratings per year, use 2018 as
#  the end year. Make a plot of average rating versus ratings per year
#  and show an estimate of the trend.
movielens %>% filter(year >= 1993) %>%
  group_by(movieId) %>%
  mutate(n_ratings_per_year = n()/(2018-1994)) %>%
  mutate(mean_rating = mean(rating)) %>% 
  ggplot(aes(n_ratings_per_year, mean_rating)) + 
  geom_point() + 
  geom_smooth()


############################# QUESTION 6 ################################
# Add a date column
movielens <- mutate(movielens, date = as_datetime(timestamp))
# Compute the average rating for each week and plot this average against
#  day. Hint: use the round_date function before you group_by. What type
#  of trend do you observe?
movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()


############################# QUESTION 8 ################################
# The movielens data also has a genres column. This column includes every
#  genre that applies to the movie. Some movies fall under several genres.
#  Define a category as whatever combination appears in this column. Keep
#  only categories with more than 1,000 ratings. Then compute the average
#  and standard error for each category. Plot these as error bar plots.
# Which genre has the lowest average rating?
movielens %>% group_by(genres) %>% 
  filter(length(rating) >= 1000) %>%
  summarize(avg = mean(rating), sd = sd(rating)) %>%
  filter(avg < 3.5) %>%
  ggplot(aes(x=genres, ymin=avg-sd, ymax=avg+sd)) + 
  geom_errorbar()











