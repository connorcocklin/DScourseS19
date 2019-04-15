

set.seed(100)

income <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
#   age: continuous.
#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#   fnlwgt: continuous.
#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#   education-num: continuous.
#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#   sex: Female, Male.
#   capital-gain: continuous.
#   capital-loss: continuous.
#   hours-per-week: continuous.
#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.

######################
# Clean up the data
######################
# Drop unnecessary columns
income$native.country <- NULL
income$fnlwgt         <- NULL
# Make sure continuous variables are coded as such
income$age            <- as.numeric(income$age)
income$hours          <- as.numeric(income$hours)
income$education.num  <- as.numeric(income$education.num)
income$capital.gain   <- as.numeric(income$capital.gain)
income$capital.loss   <- as.numeric(income$capital.loss)
# Combine levels of categorical variables that currently have too many levels
levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))

# Break up the data:
n <- nrow(income)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
income.train <- income[train,]
income.test  <- income[test, ]


library(mlr)
library(tidyverse)
library(magrittr)
library(glmnet)
library(rpart)
library(nnet)
library(kknn)
library(e1071)

income$high.earner <- as.character(income$high.earner)
ClassifTask <- makeClassifTask(id = "taskname", data = income.train, target = "high.earner")
resampleStrat <- makeResampleDesc(method = "CV", iters = 3)
tuneMethod <- makeTuneControlRandom(maxit = 10L)

treesLearner <- makeLearner(cl = "classif.rpart", predict.type = "response")
logrLearner <- makeLearner(cl = "classif.glmnet", predict.type = "response")
NNLearner <- makeLearner(cl = "classif.nnet", predict.type = "response")
NaiveLearner <- makeLearner(cl = "classif.naiveBayes", predict.type = "response")
kNNLearner <- makeLearner(cl = "classif.kknn", predict.type = "response")
SVMLearner <- makeLearner(cl = "classif.svm", predict.type = "response")

PredAlg <- (high.earner ~ age + workclass + education + education.num
+ marital.status + occupation + relationship + race + sex + capital.gain +
  capital.loss + hours)


#Tree                                   
TreeALG <- rpart(high.earner ~ age + workclass + education + education.num
                 + marital.status + occupation + relationship + race + sex + capital.gain +
                   capital.loss + hours, data = income.train)
summary(TreeALG)
printcp(TreeALG)
plotcp(TreeALG)

#logreg
library(glmnet)

# Search over penalty parameter lambda and force elastic net parameter to be 1 (LASSO)
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=3),makeNumericParam("alpha",lower=0,upper=1))

# Do the tuning
tunedModel <- tuneParams(learner = logrLearner,
                         task = ClassifTask,
                         resampling = resampleStrat,       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)

LOGREGAlg <- setHyperPars(learner=logrLearner, par.vals = tunedModel$x)

# Verify performance on cross validated sample sets
resample(LOGREGAlg,logrLearner,resampleStrat)

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

#Nueral Network
NNALG <- nnet(high.earner ~ age + workclass + education + education.num
              + marital.status + occupation + relationship + race + sex + capital.gain +
                capital.loss + hours, data = income.train, size = 7)
library(NeuralNetTools)
plotnet(NNALG)

#Naive Bayes
NBALG <- naiveBayes(high.earner ~ age + workclass + education + education.num
                    + marital.status + occupation + relationship + race + sex + capital.gain +
                      capital.loss + hours, data = income.train)

#kNN
kNNAlg <- kknn(high.earner ~ age + workclass + education + education.num
               + marital.status + occupation + relationship + race + sex + capital.gain +
                 capital.loss + hours, train = income.train,test = income.test)

SVMALG <- svm(high.earner ~ age + workclass + education + education.num
              + marital.status + occupation + relationship + race + sex + capital.gain +
                capital.loss + hours, data = income.train)

#training the models
TreeModel <- tune.rpart(high.earner ~ age + workclass + education + education.num
                        + marital.status + occupation + relationship + race + sex + capital.gain +
                          capital.loss + hours, data = income.train)
TreeModel$performances





