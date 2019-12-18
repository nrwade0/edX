# Import raw Brexit referendum polling data from Wikipedia:
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)

# ---

# remove first row
polls <- polls[-1,]

# rename column names
names(polls) <- c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")

# only keep rows with9 a percent sign in remain column
pattern <- "\\d+(?:\\.\\d+)?%"

length(grep(pattern, polls$remain))

# ---

# fix end dates
temp <- str_extract_all(polls$dates, "\\d+\\s[a-zA-Z]{3,5}")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
end_date

# ---
# ---

library(dslabs)
library(lubridate)
options(digits = 3)

pattern <- "\\d{4}-04-\\d{2}"

length(grep(pattern, brexit_polls$startdate))


# ---

round_date(brexit_polls$enddate, unit="week")

table(weekdays(brexit_polls$enddate))

# ---

data(movielens)
table(floor_date(as_datetime(movielens$timestamp), unit="year"))

table(hour(as_datetime(movielens$timestamp)))


# ---

library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)

table(str_detect(gutenberg_metadata$title, "Pride and Prejudice"))

gutenberg_works(title == "Pride and Prejudice")

words <- gutenberg_download(gutenberg_id = "1342") %>%
  unnest_tokens(word,text) %>%
  filter(!word %in% stop_words$word & !str_detect(word, "\\d+"))
nrow(words)

words %>% count(word, sort=TRUE) %>% filter(n>100)

afinn <- get_sentiments("afinn")
afinn_sentiments <- words %>% inner_join(afinn, by = "word")
dim(afinn_sentiments)

# proportion of words with a positive value?
mean(afinn_sentiments$value > 0)

# how many have word values are 4?
afinn_sentiments %>% filter(value==4)

