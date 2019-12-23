#########################
# Movielens data set quiz
#########################

# load data sets (edx, validation) and libraries
load("~/Documents/GitHub/edX/9-Capstone/MovieLensProject/data.RData")
library(tidyverse)
library(DescTools)


############################# QUESTION 1 ###############################
# How many rows and columns are there in the edx dataset?
dim(edx)


############################# QUESTION 2 ###############################
# How many zeros were given as ratings in the edx dataset?
edx %>% filter(rating == 0) %>% nrow()

# How many threes were given as ratings in the edx dataset?
edx %>% filter(rating == 3) %>% nrow()


############################# QUESTION 3 ###############################
# How many different movies are in the edx dataset?
edx %>% group_by(movieId) %>% summarize(count = n())


############################# QUESTION 4 ###############################
# How many different users are in the edx dataset?
edx %>% group_by(userId) %>% summarize(count = n())


############################# QUESTION 5 ###############################
# How many movie ratings are in each of the following genres in the edx
#  dataset?
edx %>% filter(genres %like% "%Drama%") %>% summarize(n())
edx %>% filter(genres %like% "%Comedy%") %>% summarize(n())
edx %>% filter(genres %like% "%Thriller%") %>% summarize(n())
edx %>% filter(genres %like% "%Romance%") %>% summarize(n())


############################# QUESTION 6 ###############################
# Which movie has the most ratings?
edx %>% group_by(title) %>%
  summarize(n_ratings = n()) %>%
  arrange(desc(n_ratings))

# How many movies have one rating?
edx %>% group_by(title) %>%
  summarize(n_ratings = n()) %>%
  filter(n_ratings==1) %>%
  count() %>% pull()


############################# QUESTION 7 ###############################
# What are the five most given ratings in order from most to least?
edx %>% group_by(rating) %>%
  summarize(n_ratings = n()) %>%
  arrange(desc(n_ratings))


############################# QUESTION 8 ###############################
# T/F, there are fewer ratings of 3.5 than there are ratings of 3 or 4.
edx %>% group_by(rating) %>%
  summarize(n_ratings = n()) %>%
  arrange(desc(n_ratings))
