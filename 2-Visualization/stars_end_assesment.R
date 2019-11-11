library(tidyverse)
library(dslabs)
library(lattice)
library(ggrepel)
data(stars)
options(digits = 3)   # report 3 significant digits


# What is the mean magnitude?
mean(stars$magnitude)
# What is the standard deviation of magnitude?
sd(stars$magnitude)


# Make a density plot of the magnitude.
#  How many peaks are there in the data?
stars %>% ggplot(aes(x=magnitude)) +
  geom_density()


# Examine the distribution of star temperature.
#  Which of these statements best characterizes the temperature distribution?
stripplot(stars$temp)


# Make a scatter plot of the data with temperature on the x-axis and
#  magnitude on the y-axis and examine the relationship between the variables.
#  Recall that lower magnitude means a more luminous (brighter) star.
# Most stars follow a _______________ trend. These are called main sequence stars.
stars %>% ggplot(aes(x=temp, y=magnitude)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_continuous("log10")

# The least lumninous star in the sample with a surface temperature over 5000K is _________.
stars %>% filter(temp>5000, magnitude>0) %>%
  ggplot(aes(x=temp, y=magnitude, label=star)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_continuous("log10") +
  geom_label_repel()

# The two stars with lowest temperature and highest luminosity are known as supergiants.
#  The two supergiants in this dataset are ____________.
stars %>% filter(temp<5000, magnitude<0) %>%
  ggplot(aes(x=temp, y=magnitude, label=star)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_continuous("log10") +
  geom_label_repel()


# Remove the text labels and color the points by star type. This classification describes
#  the properties of the star's spectrum, the amount of light produced at various wavelengths.
stars %>%
  ggplot(aes(x=temp, y=magnitude, color=type)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_continuous("log10")







