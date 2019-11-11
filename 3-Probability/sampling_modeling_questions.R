options(digits = 3)

#### Useful Formulas
# Expected values of a random variable
# ap + b(1- p)
# Expected value of the sum of n draws of a random variable
# n * (ap + b(1-p))
# Standard deviation of an urn with two values
# abs(b - a) * sqrt(p(1 - p))
# Standard error of the sum of n draws of a random variable
# sqrt(n) * abs(b - a) * sqrt(p(1 - p))
####


######### QUESTION 1a/b/c/d/e/f #########
# An old version of the SAT college entrance exam had a -0.25 point penalty for
#  every incorrect answer and awarded 1 point for a correct answer. The quantitative
#  test consisted of 44 multiple-choice questions each with 5 answer choices. Suppose
#  a student chooses answers by guessing for all questions on the test.

# What is the probability of guessing correctly for one question?
1/5

# What is the expected value of points for guessing on one question?
1/5 * 1 + -0.25* 4/5

# What is the expected score of guessing on all 44 questions?
mu <- 44 * (1/5 * 1 + -0.25* 4/5)
mu

# What is the standard error of guessing on all 44 questions?
er <- sqrt(44) * abs(1+0.25) * sqrt(1/5*4/5)
er

# Use the Central Limit Theorem to determine the probability that a guessing student
#  scores 8 points or higher on the test.
1-pnorm(8,mu,er)

# Set the seed to 21, then run a Monte Carlo simulation of 10,000 students guessing
#  on the test.
# What is the probability that a guessing student scores 8 points or higher?
set.seed(21)
X <- replicate(10000, {
  sum(sample(c(1,-0.25), size=44, replace=TRUE, prob=c(1/5,4/5)))
})
sum(X>=8)/10000


######### QUESTION 2a/b/c #########
# Suppose that the number of multiple choice options is 4 and that there is no
#  penalty for guessing - that is, an incorrect question gives a score of 0.
# What is the expected value of the score when guessing on this new test?
mu <- 44 * (1/4 * 1 + 0* 3/4)

# What is the probability of scoring over 30 when guessing? Report your answer
#  using scientific notation with 3 significant digits in the format x.xx*10^y.
er <- sqrt(44) * abs(1) * sqrt(1/4*3/4)
1-pnorm(30, mu, er)

# Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05)
#  representing a range of student skills.
# What is the lowest p such that the probability of scoring over 35 exceeds 80%?
p <- seq(0.25, 0.95, 0.05)

fu <- function(p){
  # calculate the expected value at given p
  expected_value <- 44 * (1*p + 0*(1-p))
  # calculate the standard error at given p
  standard_error <- sqrt(44) * abs(1 - 0) * sqrt(p*(1 - p))
  # calculate likelihood of score of 35 or greater
  1-pnorm(35, expected_value, standard_error)
}

sapply(p, FUN=fu)



######### QUESTION 3a/b/c/d/e/f/g #########
# A casino offers a House Special bet on roulette, which is a bet on five pockets
#  (00, 0, 1, 2, 3) out of 38 total pockets. The bet pays out 6 to 1. In other
#  words, a losing bet yields -$1 and a successful bet yields $6. A gambler wants
#  to know the chance of losing money if he places 500 bets on the roulette House
#  Special.
# What is the expected value of the payout for one bet?
(6*5/38 + -1*(1 - 5/38))

# What is the standard error of the payout for one bet?
abs(-1 - 6) * sqrt(5/38*(1 - 5/38))

# What is the expected value of the average payout over 500 bets? Remember there
#  is a difference between expected value of the average and expected value of
#  the sum. Same as one bet.
(6*5/38 + -1*(1 - 5/38))

# What is the standard error of the average payout over 500 bets? Remember there
# is a difference between the standard error of the average and standard error of
# the sum.
(abs(-1 - 6) * sqrt(5/38*(1 - 5/38)))/sqrt(500)

# What is the expected value of the sum of 500 bets?
mu <- 500 * (6*5/38 + -1*(1 - 5/38))
mu

# What is the standard error of the sum of 500 bets?
er <- sqrt(500) * (abs(-1 - 6) * sqrt(5/38*(1 - 5/38)))
er

# Use pnorm with the expected value of the sum and standard error of the sum to
# calculate the probability of losing money over 500 bets,  Pr(𝑋≤0) .
pnorm(0, mu, er)




