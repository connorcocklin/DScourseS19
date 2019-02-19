library(rvest)
library(tidyverse)
url <- "http://insider.espn.com/nba/hollinger/statistics"
page <- read_html(url)
table <- html_table(page, fill = TRUE)
table
write.csv(table, file = "PER_Rankings.csv")