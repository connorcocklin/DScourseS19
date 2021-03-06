library(rvest)
library(tidyverse)
library(devtools)
devtools::install_github("abresler/forbesListR")
library(forbesListR)
nba_2017_values <- get_year_forbes_list_data(list = "NBA Valuations", year = 2016)
write.csv(nba_2017_values, file = "NBA_2017_Valuations.csv")
