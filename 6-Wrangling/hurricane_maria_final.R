library(tidyverse)
library(pdftools)
options(digits = 3)    # report 3 significant digits

# We will use your new data wrangling skills to extract actual daily
#  mortality data from Puerto Rico and investigate whether the Hurricane
#  María had an immediate effect on daily mortality compared to unaffected
#  days in September 2015-2017.

# open mortality file from dslabs directory and save pdf info into 'text'
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system2("open", args = fn)
text <- pdf_text(fn)

# split the ninth page (september) by new lines commands and save to 'x'
x <- str_split(text[[9]], "\n")
typeof(x)
length(x)

# save first lines of x in s
s <- x[[1]]
typeof(s)
length(s)

# trim s of whitespace on both sides
s <- str_trim(s, side="both")
s[[1]]

# find the index of the header for the pdf table, looking for "2015"
header_index <- str_which(s, pattern="2015") # found in lines 2 and 24
header_index <- 2

# save the header string vector into 'header' and split into words through whitespace
header <- s[[header_index]]
header <- str_split(string = header, pattern = "\\s+", simplify = FALSE)
header

# find the row at the end of the summary table, looking for "Total"
tail_index <- str_which(s, pattern="Total")

# determine 'out', a number vector, as the indices of rows to be removed from 's'
n <- str_count(string = s, pattern = "\\d+")
out <- 1:header_index # remove above the header
out <- append(out, which(n == 1)) # only has one number in these rows
out <- append(out, tail_index:length(s)) # remove summary table
# remove the rows
s <- s[-out]
length(s)

# remove all whitespace between tabled values
s <- str_remove_all(s, "[^\\d\\s]")

# convert s into a data matrix with just the day and death count data
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
# change class of s to numeric, m
m <- mapply(s, FUN=as.numeric)
# convert into a matrix of doubles, ss
ss <- matrix(data=m, ncol=5, nrow=30)
# change column names of double matrix ss
colnames(ss) <- c("day", "s2015", "s2016", "s2017", "s2018")
# finall convert 's' into a useable data frame, 'sdf'
sdf <- as.data.frame(ss)

# calculate the mean of deaths in 2015 column
mean(sdf$s2015)
# calculate the mean of deaths in 2016 column
mean(sdf$s2016)
# calculate the mean of deaths in 2017 column before hurricane (sept 1-19)
mean(sdf[1:19, 4])
# calculate the mean of deaths in 2017 column after hurricane (sept 20-30)
mean(sdf[20:30, 4])

# tidy data into dataframe to plot
sdf <- sdf %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
sdf

# plot datafram as line plot, year-color coordinated
ggplot(data=sdf, aes(x=day, y=deaths, group=year, color=year)) +
  geom_line()+
  geom_point()










