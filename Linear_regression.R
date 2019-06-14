#load data
bankMarkingData <- read.csv("bank-additional-full.csv", header = TRUE, sep = ";" , stringsAsFactors = TRUE)

#converting in numeric
bankMarkingData$age <- as.numeric(bankMarkingData$age)
bankMarkingData$job <- as.numeric(bankMarkingData$job)
bankMarkingData$marital <- as.numeric(bankMarkingData$marital)
bankMarkingData$education <- as.numeric(bankMarkingData$education)
bankMarkingData$default <- as.numeric(bankMarkingData$default)
bankMarkingData$housing <- as.numeric(bankMarkingData$housing)
bankMarkingData$loan <- as.numeric(bankMarkingData$loan)
bankMarkingData$contact<- as.numeric(bankMarkingData$contact)
bankMarkingData$month <- as.numeric(bankMarkingData$month)
bankMarkingData$day_of_week <- as.numeric(bankMarkingData$day_of_week)
bankMarkingData$duration <- as.numeric(bankMarkingData$duration)
bankMarkingData$campaign <- as.numeric(bankMarkingData$campaign)
bankMarkingData$pdays <- as.numeric(bankMarkingData$pdays)
bankMarkingData$previous <- as.numeric(bankMarkingData$previous)
bankMarkingData$poutcome <- as.numeric(bankMarkingData$poutcome)
bankMarkingData$y <- as.numeric(bankMarkingData$y)

library(psych)

#corelation
# cor(bankMarkingData$y, bankMarkingData$age)
# cor(bankMarkingData$y, bankMarkingData$job)
# cor(bankMarkingData$y, bankMarkingData$marital)
# cor(bankMarkingData$y, bankMarkingData$education)
# cor(bankMarkingData$y, bankMarkingData$default)
# cor(bankMarkingData$y, bankMarkingData$housing)
# cor(bankMarkingData$y, bankMarkingData$loan)
# cor(bankMarkingData$y, bankMarkingData$contact)
# cor(bankMarkingData$y, bankMarkingData$month)
# cor(bankMarkingData$y, bankMarkingData$day_of_week)
# cor(bankMarkingData$y, bankMarkingData$duration)
# cor(bankMarkingData$y, bankMarkingData$campaign)
# cor(bankMarkingData$y, bankMarkingData$pdays)
# cor(bankMarkingData$y, bankMarkingData$previous)
# cor(bankMarkingData$y, bankMarkingData$poutcome)
# cor(bankMarkingData$y, bankMarkingData$emp.var.rate)
# cor(bankMarkingData$y, bankMarkingData$cons.price.idx)
# cor(bankMarkingData$y, bankMarkingData$cons.conf.idx)
# cor(bankMarkingData$y, bankMarkingData$euribor3m)
# cor(bankMarkingData$y, bankMarkingData$nr.employed)

cor(bankMarkingData[c("age", "campaign","previous","emp.var.rate", 
                      "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed")])

#scatterplot matrix

pairs.panels(bankMarkingData[c("age", "campaign","previous","emp.var.rate", 
                               "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed")])


##########Simple Linear############


lm1<- lm(emp.var.rate ~ euribor3m, bankMarkingData)
summary(lm1)



lm2<- lm(emp.var.rate ~ nr.employed, bankMarkingData)
summary(lm2)


lm3 <- lm(euribor3m ~ nr.employed, bankMarkingData)
summary(lm3)


##########Multi Linear Regression######

mlm_1 <- lm(nr.employed~ emp.var.rate + euribor3m, bankMarkingData)
summary(mlm_1)

#Residual Standard Error (RSE), or sigma
sigma(mlm_1)/mean(bankMarkingData$nr.employed)



