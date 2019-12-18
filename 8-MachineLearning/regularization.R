library(tidyverse)
library(caret)
options(digits = 3)


# The exercises in Q1-Q8 work with a simulated dataset for 1000 schools.
#  This pre-exercise setup walks you through the code needed to simulate
#  the dataset.
# An education expert is advocating for smaller schools. The expert bases
#  this recommendation on the fact that among the best performing schools,
#  many are small schools. Let's simulate a dataset for 1000 schools.
#  First, let's simulate the number of students in each school, using the
#  following code:
set.seed(1986, sample.kind="Rounding")
n <- round(2^rnorm(1000, 8, 1))

# Now let's assign a true quality for each school that is completely
#  independent from size. This is the parameter we want to estimate in
#  our analysis. The true quality can be assigned using the following code:
set.seed(1, sample.kind="Rounding")
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

# We can see the top 10 schools using this code: 
schools %>% top_n(10, quality) %>% arrange(desc(quality))

# Now let's have the students in the school take a test. There is random
#  variability in test taking, so we will simulate the test scores as
#  normally distributed with the average determined by the school quality
#  with a standard deviation of 30 percentage points. This code will
#  simulate the test scores:
set.seed(1, sample.kind="Rounding")
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))


################################ QUESTION 1 ################################
# What are the top schools based on the average score? Show just the ID,
#  size, and the average score. Report the ID of the top school and average
#  score of the 10th school.
# What is the ID and it's average score of the top school?
top10_schools <- schools %>% top_n(10, score) %>% arrange(desc(score))


################################ QUESTION 2 ################################
# Compare the median school size to the median school size of the top 10
#  schools based on the score.
median(top10_schools$size)
median(schools$size)


################################ QUESTION 3 ################################
# What is the median school size of the bottom 10 schools based on the score?
btm10_schools <- schools %>% top_n(10, -score) %>% arrange(desc(score))
median(btm10_schools$size)


################################ QUESTION 4 ################################
# We see that the worst schools are also small. Plot the average score
#  versus school size to see what's going on. Highlight the top 10 schools
#  based on the true quality.
ggplot() +
  geom_point(data=schools, aes(x=size, y=score), color="blue") +
  geom_point(data=top10_schools, aes(x=size, y=score), color="red")


################################ QUESTION 5 ################################
# Let's use regularization to pick the best schools. Remember regularization
#  shrinks deviations from the average towards 0. To apply regularization
#  here, we first need to define the overall average for all schools, using
#  the following code:
overall <- mean(sapply(scores, mean))

# Then, we need to define, for each school, how it deviates from that
#  average.
# Write code that estimates the score above the average for each school
#  but dividing by n+alpha instead of n, with n the school size and alpha
#  a regularization parameter. Try alpha=25 .
schools <- schools %>% mutate(reg = overall+size*(score-overall)/(size+25))
schools %>% top_n(10, score) %>% arrange(desc(reg))


################################ QUESTION 6 ################################
# Notice that this improves things a bit. The number of small schools that
#  are not highly ranked is now lower. Is there a better alpha? Using values
#  of alpha from 10 to 250, find the alpha that minimizes the RMSE.
# What value of gives the minimum RMSE?
# rmse = sqrt(1/1000 * sum(quality - estimate)^2)
alphas <- seq(10, 250, 1)
RMSE <- function(alpha){
  temp <- schools %>%
    mutate(score_reg = overall + size*(score - overall)/(size + alpha))
  sqrt(mean((temp$quality - temp$score_reg)^2))
}

rmse <- sapply(alphas, RMSE)
plot(alphas, rmse)
alphas[which.min(rmse)]


################################ QUESTION 7 ################################
# Rank the schools based on the average obtained with the best alpha. Note
#  that no small school is incorrectly included.
# What is the ID of the top school now?
alpha <- 135
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))


################################ QUESTION 8 ################################
# We don't subtract the overall average before shrinking, we actually obtain
#  a very similar result. Confirm this by re-running the code from the
#  exercise in Q6 but without removing the overall mean.
# What value of alpha gives the minimum RMSE here?
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]  





