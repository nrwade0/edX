library(tidyverse)

# Construct a dataset that represents grade scores for 100 students in 24
#  different subjects. The overall average has been removed so this data
#  represents the percentage point each student received above or below
#  the average test score. So a 0 represents an average grade (C), a 25
#  is a high grade (A+), and a -25 represents a low grade (F).
set.seed(1987, sample.kind="Rounding")
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

############################## QUESTION 1 ###############################
# Visualize the test scores
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)


############################## QUESTION 2 ###############################
# Examine the correlation between the test scores directly.
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)


############################## QUESTION 3 ###############################
# Compute the SVD of y
s <- svd(y)
names(s)

# check SVD works
y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

# Compute the sum of squares of the columns of Y and store them in ss_y.
#  Then compute the sum of squares of columns of the transformed YV and
#  store them in ss_yv. Confirm that sum(ss_y) is equal to sum(ss_yv)
sum(colSums(y^2))


############################## QUESTION 4 ###############################
#  Plot ss_y  and ss_yv against the column number
ss_y <- colSums(y^2)
ss_yv <- apply((y%*%s$v)^2, 2, sum)
ggplot() +
  geom_point(aes(x=1:24, y=ss_y), color="blue") +
  geom_point(aes(x=1:24, y=ss_yv), color="red")


############################## QUESTION 5 ###############################
# plotting the square root of ss_yv versus the diagonal entries of D.
data.frame(x = sqrt(ss_yv), y = s$d) %>%
  ggplot(aes(x,y)) +
  geom_point()




