opit<- c(1, 3, 4, 6, 7, 7, 8, 8, 10, 12, 17)
summary(opit)
boxplot(opit)$out

opit1<- c(1, 2, 2, 3, 3, 4, 5, 5, 10)
summary(opit1)

bankMarkingData <- read.csv("bank-additional-full.csv", header = TRUE, sep = ";" , stringsAsFactors = TRUE)

summary(bankMarkingData)





#histogram


bankMarkingData$marital <- as.numeric(bankMarkingData$marital)

marital <- bankMarkingData$marital
m <- mean(marital)
std <- sd(marital)
hist(marital, prob = T, col = "grey" , main="curve over histogram")
curve(dnorm(x, mean = m, sd= std), add = TRUE, col = "red", lwd=2, yaxt="n" )

qqnorm(bankMarkingData$marital); qqline(bankMarkingData$marital)

education = bankMarkingData$education
m<-mean(education)
std<-sqrt(var(education))
hist(education, density=20, breaks=20, prob=TRUE, 
    
     main="normal curve over histogram")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")









length(bankMarkingData$education)
sd(bankMarkingData$education)
bankMarkingData$education <- as.numeric(bankMarkingData$education)
bankMarkingData$education= rnorm(41188, 4.747, 2.136482)
plot(bankMarkingData$education)
lines(bankMarkingData$education)

#Q-Q Plots
qqnorm(bankMarkingData$education); qqline(bankMarkingData$education)

qqnorm(bankMarkingData$age); qqline(bankMarkingData$age)

bankMarkingData_sub <- subset(bankMarkingData, age < 70)
qqnorm(bankMarkingData_sub$age); qqline(bankMarkingData_sub$age)



bankMarkingData$marital <- as.numeric(bankMarkingData$marital)
qqnorm(bankMarkingData$marital); qqline(bankMarkingData$marital)

summary(bankMarkingData$marital)

qqnorm(bankMarkingData$is_divorced); qqline(bankMarkingData$is_divorced)
qqnorm(bankMarkingData$is_married); qqline(bankMarkingData$is_married)
qqnorm(bankMarkingData$is_single); qqline(bankMarkingData$is_single)
qqnorm(bankMarkingData$is_unknown); qqline(bankMarkingData$is_unknown)



bankMarkingData$is_divorced <- ifelse(bankMarkingData$marital == "divorced", 1, 0)
bankMarkingData$is_single <- ifelse( bankMarkingData$marital == "single", 2, 0)
bankMarkingData$is_married <- ifelse( bankMarkingData$marital == "married", 3, 0)
bankMarkingData$is_unknown <- ifelse(bankMarkingData$marital == "unknown", 4, 0)

table(bankMarkingData$is_single)
table(bankMarkingData$is_married)
table(bankMarkingData$is_divorced)
table(bankMarkingData$is_unknown)

str(bankMarkingData$marital)
summary(bankMarkingData$marital)

hist(bankMarkingData$age,col=terrain.colors(10))

summary(bankMarkingData$default)










