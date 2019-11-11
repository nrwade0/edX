options(digits = 3)
library(tidyverse)
library(dslabs)


data(death_prob)
head(death_prob)

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
# The death_prob data frame contains information about the estimated probability
#  of death within 1 year (prob) for different ages and sexes.
# Use death_prob to determine the death probability of a 50 year old female, p.
p <- death_prob %>% filter(age==50, sex=="Female") %>% pull(prob)

# The loss in the event of the policy holder's death is -$150,000 and the gain
#  if the policy holder remains alive is the premium $1,150.
# What is the expected value of the company's net profit on one policy for a
#  50 year old female?
-150000*p + 1150*(1-p)

# Calculate the standard error of the profit on one policy for a 50 year old female.
abs(-150000-1150)*sqrt(p*(1-p))

# What is the expected value of the company's profit over all 1,000 policies for 50
#  year old females?
1000*(-150000*p + 1150*(1-p))

# What is the standard error of the sum of the expected value over all 1,000 policies
#  for 50 year old females?
sqrt(1000)*(abs(-150000-1150)*sqrt(p*(1-p)))

# Use the Central Limit Theorem to calculate the probability that the insurance
#  company loses money on this set of 1,000 policies.
pnorm(0, 1000*(-150000*p + 1150*(1-p)), sqrt(1000)*(abs(-150000-1150)*sqrt(p*(1-p))))



######### QUESTION 2a/b/c/d #########
# Use death_prob to determine the probability of death within one year for
#  a 50 year old male.
p <- death_prob %>% filter(age==50, sex=='Male') %>% pull(prob)

# Suppose the company wants its expected profits from 1,000 50 year old males
#  with $150,000 life insurance policies to be $700,000. Use the formula for
#  expected value of the sum of draws with the following values and solve for the
#  premium b:
# where   E[S] = mu_S = 700000
#         n = 1000
#         p = death probability of 50 year old males
#         a = 150000 loss
#         b = premium to solve
# E[S] = n * (ap + b(1-p))
# --> b = ((E[S]/n) - ap)/(1-p)
b <- ((700000/1000) - -150000*p)/(1-p)

# Using the new 50 year old male premium rate, calculate the standard error of
#  the sum of 1,000 premiums.
err <- sqrt(1000) * abs(b - -150000) *sqrt(p*(1-p))

# What is the probability of losing money on a series of 1,000 policies to 50
# year old males? Use the Central Limit Theorem.
pnorm(0, 1000*(-150000*p + b*(1-p)), err)


######### QUESTION 3a/b/c/d/e/f #########
# a lethal pandemic disease increases the probability of death within 1 year
#  for a 50 year old to .015. Unable to predict the outbreak, the company has
#  sold 1,000 $150,000 life insurance policies for $1,150.
# What is the expected value of the company's profits over 1,000 policies?
mu <- 1000 * (-150000*0.015 + 1150*(1-0.015))

# What is the standard error of the expected value of the company's profits over
#  1,000 policies?
err <- sqrt(1000) * abs(-150000 - 1150) * sqrt(0.015*(1 - 0.015))

# What is the probability of the company losing money?
pnorm(0, mu, err)

# Suppose the company can afford to sustain one-time losses of $1 million, but
#  larger losses will force it to go out of business. What is the probability of
#  losing more than $1 million?
pnorm(-1000000,mu,err)

# Investigate death probabilities p <- seq(.01, .03, .001). What is the lowest
#  death probability for which the chance of losing money exceeds 90%?
p <- seq(0.01, 0.03, 0.001)
f <- function(p){
  mu <- 1000 * (-150000*p + 1150*(1-p))
  err <- sqrt(1000) * abs(-150000 - 1150) * sqrt(p*(1 - p))
  pnorm(0, mu, err)
}
sapply(p, FUN=f)

# Investigate death probabilities p <- seq(.01, .03, .0025). What is the lowest
#  death probability for which the chance of losing over $1 million exceeds 90%?
p <- seq(0.01, 0.03, 0.0025)
f1 <- function(p){
  mu <- 1000 * (-150000*p + 1150*(1-p))
  err <- sqrt(1000) * abs(-150000 - 1150) * sqrt(p*(1 - p))
  pnorm(-1000000, mu, err)
}
sapply(p, FUN=f1)


######### QUESTION 4a/b #########
# Define a sampling model for simulating the total profit over 1,000 loans with
#  probability of claim p_loss = .015, loss of -$150,000 on a claim, and profit
#  of $1,150 when there is no claim. Set the seed to 25, then run the model once.
# What is the reported profit (or loss) in millions (that is, divided by 10^6)?
set.seed(25)
n <- 1000
p_loss <- 0.015

X <- sample(c(0,1), n, replace=TRUE, prob=c((1-p_loss),p_loss))
loss <- -150000*sum(X==1)/10^6 # in millions
profit <- 1150*sum(X==0)/10^6
loss+profit

# Set the seed to 27, then run a Monte Carlo simulation of your sampling model
#  with 10,000 replicates to simulate the range of profits/losses over 1,000 loans.
# What is the observed probability of losing $1 million or more?
set.seed(27)
S <- replicate(10000, {
  X <- sample(c(0, 1), 1000, replace=TRUE, prob=c((1-0.015), 0.015))
  loss <- -150000*sum(X==1)/10^6 # in millions
  profit <- 1150*sum(X==0)/10^6
  loss+profit
})
sum(S<=-1)/10000


######### QUESTION 5a/b/c/d #########
# Suppose that there is a massive demand for life insurance due to the pandemic,
#  and the company wants to find a premium cost for which the probability of losing
#  money is under 5%, assuming the death rate stays stable at p = 0.015
# Calculate the premium required for a 5% chance of losing money given n = 1000 loans
#  probability of death p = 0.015, and loss per claim l=-150000. Save this premium
#  as x for use in further questions.
p <- .015
n <- 1000
l <- -150000
z <- qnorm(.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x

# What is the expected profit per policy at this rate?
l*p + x*(1-p)

# What is the expected profit over 1,000 policies?
n*(l*p + x*(1-p))

# Run a Monte Carlo simulation with B=10000 to determine the probability of losing
#  money on 1,000 policies given the new premium x, loss on a claim of $150,000,
#  and probability of claim p=0.015. Set the seed to 28 before running your 
#  simulation.
# What is the probability of losing money here?
set.seed(28)
S <- replicate(10000, {
  X <- sample(c(0,1), n, replace = TRUE, prob=c((1-p), p))
  loss <- l*sum(X==1)/10^6 # in millions
  profit <- x*sum(X==0)/10^6
  loss+profit
})
sum(S<0)/10000


######### QUESTION 6a/b #########
# The company cannot predict whether the pandemic death rate will stay stable. Set
#  the seed to 29, then write a Monte Carlo simulation that for each of B=10000
#  iterations:
#  - randomly changes p by adding a value between -0.01 and 0.01 with
#    sample(seq(-0.01, 0.01, length = 100), 1)
#  - uses the new random p to generate a sample of n=1000 policies with premium x
#    and loss per claim l=-150000
#  - returns the profit over n policies (sum of random variable)
# The outcome should be a vector of B total profits
set.seed(29)
n <- 1000
B <- 10000
l <- -150000
p <- 0.015
x <- 3268
X <- replicate(B, {
  new_p <- p + sample(seq(-0.01, 0.01, length=100), 1)
  Y <- sample(c(x, l), n, replace=TRUE, prob=c(1-new_p, new_p))
  sum(Y)
})

# What is the expected value over 1,000 policies?
mean(X)

# What is the probability of losing money?
sum(X<0)/B

# probability of losing more than one million dollars?
mean(X<-1000000)





















