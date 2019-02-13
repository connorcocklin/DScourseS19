library(tidyverse)
library(downloader)

download("http://api.fantasy.nfl.com/v1/players/stats?statType=seasonStats&season=2010&week=1&format=json", 
"nflstats.json",mode = "w")

cat nflstats.jon

mydf <- fromJSON('nflstats.json')

class(mydf)

class(mydf$players)

head(mydf)