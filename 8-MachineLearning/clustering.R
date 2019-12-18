library(tidyverse)
library(dslabs)
data("tissue_gene_expression")


############################## QUESTION 1 ###############################
# Remove the row means and compute the distance between each observation.
#  Store the result in d.
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))


############################## QUESTION 2 ###############################
# Make a hierarchical clustering plot and add the tissue types as labels.
# You will observe multiple branches. Which tissue type is in the branch 
#  Farthest to the left?
clusters <- hclust(d)
plot(clusters)


############################## QUESTION 3 ###############################
# Run a k-means clustering on the data with k=7. Make a table comparing
#  the identified clusters to the actual tissue types. Run the algorithm
#  several times to see how the answer changes. What do you observe for
#  the clustering of the liver tissue
cl <- kmeans(tissue_gene_expression$x, centers = 7)
table(cl$cluster, tissue_gene_expression$y)


############################## QUESTION 4 ###############################
library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)






