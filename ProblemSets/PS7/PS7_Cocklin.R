install.packages("stargazer")
install.packages("mice")

library(stargazer)
library(mice)
library(tidyverse)

wages <- read.csv("wages.csv", header = TRUE, sep = ",")

wages1 <- wages %>% drop_na(hgc,tenure)
view(wages1)
stargazer(wages1)

listwise <- as.data.frame(na.omit(wages1))
View(listwise)

listwise.reg <- lm(logwage ~ hgc + college + tenure + I(tenure^2) +age +married, wages1)
stargazer(listwise.reg)

wages.mean<-wages1

wages.mean$logwage[is.na(wages.mean$logwage)]<- mean(wages.mean$logwage,na.rm = TRUE)
View(wages.mean)
meanreg<- lm(logwage ~ hgc + college + tenure + I(tenure^2) +age +married,wages.mean)
summary(meanreg)

wages1$logwagepredimp <- wages1$logwage
predlogwage <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = wages1)
wages1$predictions <- NA
wages1$predictions[!is.na(wages1$hgc) & !is.na(wages1$tenure)] <- predict(
  predlogwage, wages1
)

wages1$logwagepredimp[is.na(wages1$logwage)] <- wages1$predictions[is.na(wages1$logwage)]
pred_val_reg.imp <- lm(logwagepredimp ~ hgc + college + tenure + I(tenure^2) + age + married ,data = wages1)

print(summary(pred_val_reg.imp))

stargazer(listwise.reg, meanreg, pred_val_reg.imp)



library(mice)

wages_M <- wages %>% drop_na(hgc, tenure)
wages_M.imp = mice(wages_M, seed = 12345)
fit = with(wages_M.imp, lm(logwage ~ hgc + college + tenure + I(tenure^2) +age +married))

summary(pool(fit))

