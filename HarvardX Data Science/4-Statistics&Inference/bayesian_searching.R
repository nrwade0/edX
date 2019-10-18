# Bayes theorem: Pr(𝐴∣𝐵)=Pr(𝐵∣𝐴)Pr(𝐴)/Pr(𝐵)
# Pr(plane in A) = 0.2 
# Pr(plane in B) = 0.6
# Pr(plane in C) = 0.15
# Pr(plane in D) = 0.05
# Pr(plane not found in area | plane in area) = 0.1
# Pr(plane not found in area | plane in different area) = 1
Pr_A = 0.2
Pr_B = 0.6
Pr_C = 0.15
Pr_D = 0.05
Pr_found_inarea = 0.9
Pr_notfound_inarea = 0.1
Pr_notfound_differentarea = 1

options(digits = 3)


######### Question 1 #########
# On day 1, the team will search area B. The probability that the plane is in area B
#  is  Pr(plane in B) = 0.6 .
# What is the probability the plane is not in B?
1-Pr_B

# What is the probability the plane is in B but is not found?
Pr_B*Pr_notfound_inarea

# What is  Pr(plane not found in B) , the probability the plane is not found in B on
#  day 1?
(1-Pr_B) + Pr_B*Pr_notfound_inarea
# probability the plane is not in B at all + the probability the plane is in B but
#  is overlooked.


######### Question 2 #########
# Suppose the plane will not be found on day 1. What is the equation for
#  Pr(plane in B | plane not found in B) , the posterior probability that the plane
#  is in area B given that it is not found in area B on day 1?
# Pr(plane in B | plane not found in B) =
#  Pr(plane not found in B | plane in B) * Pr(plane in B) / Pr(plane not found in B)



######### Question 3a/b/c #########
# Use Bayes' Theorem to calculate the posterior probabilities of finding the plane
#  in each of the 4 grid locations. Recall that area B will be searched on day 1.
# Using Bayes' theorem is annoying... Essentially it is 'normalizing' or 'updating'
#  the probabilities as new information is given
# 
# What is the posterior probability that the plane is in area B given that it is
#  not found on day 1?
new_Pr_B <- Pr_B * Pr_notfound_inarea
updated_total_prob <- (Pr_A + new_Pr_B + Pr_C + Pr_D)
new_Pr_B / updated_total_prob   # divide all in order to update probabilities
# 
# What is the posterior probability that the plane is in area C given that it is
#  not found on day 1?
Pr_C / updated_total_prob

# Which area has the highest posterior probability and should be searched on day 2?
Pr_A / updated_total_prob
new_Pr_B / updated_total_prob
Pr_C / updated_total_prob
Pr_D / updated_total_prob



######### Question 4a/b/c #########
# Before the search begins, you have been asked to report the probability that
#  you find the plane within two days.

# What is the probability of finding the plane on the first day?
four_a <- Pr_B * Pr_found_inarea

# What is the probability that the plane is not found on the first day but is
#  found on the second day?
four_b <- Pr_A / updated_total_prob * Pr_found_inarea * updated_total_prob

# What is the probability that the plane is found within 2 days?
four_a + four_b












