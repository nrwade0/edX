# inmport research funding data from dslabs
library(tidyverse)
library(dslabs)
data("research_funding_rates")
research_funding_rates


######################### QUESTION 1 a ###########################
# Construct a table of gender (men/women) by award status (awarded/not)
#  using the total numbers across all disciplines.
awards_totals <- research_funding_rates %>%
  summarize(total_men = sum(applications_men), 
            selected_men = sum(awards_men),
            total_women = sum(applications_women),
            selected_women = sum(awards_women))  


######################### QUESTION 2 a ###########################
# Use the table from Question 1 to compute the percentages of men
#  awarded versus women awarded.
awards_totals$selected_men / awards_totals$total_men *100
awards_totals$selected_women / awards_totals$total_women *100


######################### QUESTION 3 a ###########################
# Run a chi-squared test on the two-by-two table to determine whether the
#  difference in the two success rates is significant. (You can use tidy
#  to turn the output of chisq.test into a data frame as well.)
# What is the p-value of the difference in funding rate?

# define variables to help with computation
no_men <- 1635-290
no_women <- 1188-177
yes_men <- awards_totals$selected_men
yes_women <- awards_totals$selected_women

# rate of success
rate <- (yes_men + yes_women)/(yes_men + no_men +yes_women + no_women)

# two-by-two table tested in chi-square test
two_by_two <- data.frame(awarded = c("no", "yes"), 
                         men = c(no_men, awards_totals$selected_men),
                         women = c(no_women, awards_totals$selected_women))

# perform chi-square test
two_by_two %>% select(-awarded) %>% chisq.test()


######################### QUESTION 4 a/b/c ###########################
# There may be an association between gender and funding. But can we infer causation here?
#  Is gender bias causing this observed difference? The response to the original paper claims
#  that what we see here is similar to the UC Berkeley admissions example. Specifically they 
#  state that this "could be a prime example of Simpson’s paradox; if a higher percentage of
#  women apply for grants in more competitive scientific disciplines, then an analysis across
#  all disciplines could incorrectly show 'evidence' of gender inequality."

# To settle this dispute, use dataset 'dat' to check if this is a case of Simpson's paradox,
#  plot the success rates versus disciplines, which have been ordered by overall success, with
#  colors to denote the genders and size to denote the number of applications.
dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat

dat %>% ggplot(aes(x=discipline, y=success, col=gender, size=applications)) +
  geom_point()

