library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(date <= "2018-05-01")


########################### QUESTION 1 ##########################
# Use the loess function to obtain a smooth estimate of the expected number of deaths
#  as a function of date. Plot this resulting smooth function. Make the span about
#  two months long and use degree=1.
fit <- loess(deaths ~ date, degree=1, span = 0.04934, data=dat)

dat %>% ggplot(aes(date, deaths)) +
  geom_point() + 
  geom_smooth(color="red", span = 0.04934, method.args = list(degree=1))

# correct answer for plot:
span <- 60 / as.numeric(diff(range(dat$date)))
fit <- dat %>%
  mutate(x = as.numeric(date)) %>%
  loess(deaths ~ x, data = ., span = span, degree = 1)

dat %>% mutate(smooth = predict(fit, as.numeric(date))) %>%
  ggplot() +
  geom_point(aes(date, deaths)) +
  geom_line(aes(date, smooth), lwd = 2, col = 2)


########################### QUESTION 2 ##########################
# Work with the same data as in Q1 to plot smooth estimates against day of
#  the year, all on the same plot, but with different colors for each year.
dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)


########################### QUESTION 3 ##########################
# Suppose we want to predict 2s and 7s in the mnist_27 dataset with just
#  the second covariate. Can we do this? On first inspection it appears
#  the data does not have much predictive power. In fact, if we fit a
#  regular logistic regression the coefficient for x_2 is not significant!
# This can be seen using this code:
library(dslabs)
library(tidyverse)
library(broom)
data("mnist_27")
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()

# Plotting a scatterplot here is not useful since y is binary:
qplot(x_2, y, data = mnist_27$train)

# Fit a loess line to the data above and plot the results. What do you observe?
mnist_27$train %>%
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess")




######### Working with matrices assesment
library(tidyverse)
library(dslabs)
mnist <- read_mnist()

sum(mnist$train$images > 50 & mnist$train$images < 205)/(60000*784)





