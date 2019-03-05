library(rvest)
library(tidyverse)
url <- "http://insider.espn.com/nba/hollinger/statistics"
page <- read_html(url)
table <- html_table(page, fill = TRUE)
table
write.csv(table, file = "PER_Rankings.csv")
library(readr)
PER_Rankings <- read_csv("PER_Rankings.csv")
PER_Rankings <- PER_Rankings[-1,-1]
PER_Rankings
names(PER_Rankings) <- c("RK","Player", "GP", "MPG", "TS%", 
                         "AST", "TO", "USG", "ORR", "DRR",
                         "REBR", "PER", "VA", "EWA")
View(PER_Rankings)
PER_Rankings[-1, ]
PER_Rankings <- PER_Rankings[-1, ]
View(PER_Rankings)
view(PER_Rankings[-33, ])
PER_Rankings <- PER_Rankings[-33, ]
view(PER_Rankings)
PER_Rankings <- PER_Rankings[-11, ]
view(PER_Rankings)
PER_Rankings <- PER_Rankings[-21, ]
view(PER_Rankings)
PER_Rankings <- PER_Rankings[-41, ]
view(PER_Rankings)

library(wordcloud2)
wcdata <- PER_Rankings[ ,c("Player","PER")]
write.csv(wcdata, file = "wcdata.csv")
wcdata1 <- read_csv("wcdata1.csv")
wordcloud2(data = wcdata1, color = "random-light", size = .2, shape = 'cardioid', rotateRatio = .5)

library(corrplot)
library(RColorBrewer)
view(PER_Rankings)
write.csv(PER_Rankings, file = "corNBAdata.csv")
corNBAdata <- read_csv("corNBAdata.csv")
view(corNBAdata)
cordata <- corNBAdata[ ,c("GP","MPG","TS%","AST","TO",
                          "USG", "ORR", "DRR", "REBR", "PER",
                          "VA", "EWA")]
NBAcor <- cor(cordata)
corrplot(NBAcor, type = "upper", order = "hclust",
         col = brewer.pal(n=8, name = "RdYlBu"))


view(corNBAdata)
library(ggpubr)
ggscatter(corNBAdata, x = "PER", y = "EWA", add = "reg.line",
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "Player Efficiency Rating",
          ylab = "Estimated Wins Added")
