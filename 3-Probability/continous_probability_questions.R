
# Set the seed to 16, then use rnorm to generate a normal distribution of 10000 tests
#  with a mean of 20.9 and standard deviation of 5.7. Save these values as act_scores.
# You'll be using this dataset throughout these four multi-part questions.
set.seed(16)
act_scores <- rnorm(10000, 20.9, 5.7)


######### QUESTION 1a/b/c/d/e #########
# What is the mean of act_scores?
mean(act_scores)

# What is the standard deviation of act_scores?
sd(act_scores)

# A perfect score is 36 or greater (the maximum reported score is 36).
# In act_scores, how many perfect scores are there out of 10,000 simulated tests?
sum(act_scores >= 36)

# In act_scores, what is the probability of an ACT score greater than 30?
sum(act_scores > 30)/length(act_scores)

# In act_scores, what is the probability of an ACT score less than or equal to 10?
sum(act_scores <= 10)/length(act_scores)


######### QUESTION 2 #########
# Set x equal to the sequence of integers 1 to 36. Use dnorm to determine the value
#  of the probability density function over x given a mean of 20.9 and standard
#  deviation of 5.7; save the result as f_x. Plot x against f_x.
x <- 1:36
f_x <- dnorm(x, 20.9, 5.7)
plot(x, f_x)


######### QUESTION 3a/b/c #########
# Convert act_scores to Z-scores. Recall from Data Visualization that to standardize
#  values (convert values into Z-scores, that is, values distributed with a mean of
#  0 and standard deviation of 1), you must subtract the mean and then divide by the
#  standard deviation. Use the mean and standard deviation of act_scores, not the
#  original values used to generate random test scores.
# What is the probability of a Z-score greater than 2 (2 standard deviations above
#  the mean)?
z_act_scores <- (act_scores-mean(act_scores))/sd(act_scores)
sum(z_act_scores > 2)/length(z_act_scores)

# What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?
2*sd(act_scores)+mean(act_scores)

# A Z-score of 2 corresponds roughly to the 97.5th percentile. Use qnorm to determine
#  the 97.5th percentile of normally distributed data with the mean and standard
#  deviation observed in act_scores.
# What is the 97.5th percentile of act_scores?
qnorm(.975, mean(act_scores), sd(act_scores))


######### QUESTION 4a/b/c/d #########
# Write a function that takes a value and produces the probability of an ACT score less
#  than or equal to that value (the CDF). Apply this function to the range 1 to 36.
# What is the minimum integer score such that the probability of that score or lower
#  is at least .95?
ceiling(qnorm(0.95, mean(act_scores), sd(act_scores)))

# Use qnorm to determine the expected 95th percentile, the value for which the
#  probability of receiving that score or lower is 0.95, given a mean score of
#  20.9 and standard deviation of 5.7. What is the 95th percentile of act_scores?
qnorm(0.95, 20.9, 5.7)

# As discussed in the Data Visualization course, we can use quantile to determine
#  sample quantiles from the data.
# Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), the 1st
#  through 99th percentiles of the act_scores data. Save these as sample_quantiles.
# In what percentile is a score of 26? Note that a score between the 98th and 99th
#  percentile should be considered the 98th percentile, for example, and that quantile
#  numbers are used as names for the vector sample_quantiles.
sample_quantiles <- seq(0.01, 0.99, 0.01)
quantile(act_scores, sample_quantiles)

# Make a corresponding set of theoretical quantiles using qnorm over the interval
#  p <- seq(0.01, 0.99, 0.01) with mean 20.9 and standard deviation 5.7. Save these
#  as theoretical_quantiles. Make a QQ-plot graphing sample_quantiles on the y-axis
#  versus theoretical_quantiles on the x-axis.
theoretical_quantiles <- qnorm(sample_quantiles, 20.9, 5.7)
qqplot(y=sample_quantiles, x=theoretical_quantiles)



