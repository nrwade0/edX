library(rvest)

url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

nodes <- html_nodes(h, "table")

# ---

html_table(nodes[[1]])
html_table(nodes[[2]])
html_table(nodes[[3]])
html_table(nodes[[4]])

# ---

length(nodes)

html_table(nodes[[19]])
html_table(nodes[[20]])
html_table(nodes[[21]])

# ---

tab_1 <- html_table(nodes[[10]])
tab_1[1] <- NULL
tab_1 <- tab_1[-1,]
names(tab_1)[1] <- "Team"
names(tab_1)[2] <- "Payroll"
names(tab_1)[3] <- "Average"

tab_2 <- html_table(nodes[[19]])
tab_2 <- tab_2[-1,]
names(tab_2)[1] <- "Team"
names(tab_2)[2] <- "Payroll"
names(tab_2)[3] <- "Average"

full_join(tab_1, tab_2, by = "Team")

# ---

url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)

nodes <- html_nodes(h, "table")

length(nodes)

html_table(nodes[[1]], fill = TRUE)
html_table(nodes[[2]], fill = TRUE)
html_table(nodes[[3]], fill = TRUE)
w <- html_table(nodes[[4]], fill = TRUE)
w <- html_table(nodes[[5]], fill = TRUE)
w <- html_table(nodes[[6]], fill = TRUE)
html_table(nodes[[7]], fill = TRUE)










