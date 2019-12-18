library(caret)
library(dslabs)
library(tidyverse)

############################# QUESTION 1 ###############################
# We want to explore the tissue_gene_expression predictors by plotting
#  them.
data("tissue_gene_expression")
dim(tissue_gene_expression$x)

# We want to get an idea of which observations are close to each other,
#  but, as you can see from the dimensions, the predictors are
#  500-dimensional, making plotting difficult. Plot the first two
#  principal components with color representing tissue type.

# Which tissue is in a cluster by itself?
tissue_gene_expression$x[,1:2] %>% data.frame %>%
  ggplot(aes(MAML1, LHPP, color=tissue_gene_expression$y)) +
  geom_point()


############################# QUESTION 2 ###############################
# For each observation, compute the average across all predictors, and
#  then plot this against the first PC with color representing tissue.
#  Report the correlation.