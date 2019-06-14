#load data
bankMarkingData <- read.csv("bank-additional-full.csv", header = TRUE, sep = ";" , stringsAsFactors = TRUE)

#check the data type 
class(bankMarkingData)

#structure of the dataset
str(bankMarkingData)

#dimension of the dataset
dim(bankMarkingData)

#col names
colnames(bankMarkingData)

#examine the content of the data by showing the first  6 and last 6 rows
head(bankMarkingData)
tail(bankMarkingData)

#summary of data
summary(bankMarkingData)



library(dplyr)

#target persentage 
summary(bankMarkingData$y)

table_target <- table(bankMarkingData$y)
target_values<- cbind(table_target, prop.table(table_target)*100)
colnames(target_values) <- c('Count','Percentage')
 target_percentage <- round(target_values, digits = 1)
  
target_percentage
 

#identify the missing values
sum(is.na(bankMarkingData))
sapply(bankMarkingData, function(x) sum(is.na(bankMarkingData)))


#deeper checking of missing values
if(length(which(is.na(bankMarkingData)==T))){
  print("Missig values are found")
  
}else{
  print("No missing values are found")
  
}

#histogram normal curve distribution


#job
job <- bankMarkingData$job

  m <- mean(job)
std <- sd(job)
hist(job, prob = T, col = "grey" , main= "normal curve over histogram")
curve(dnorm(x, mean = m, sd= std), add = TRUE, col = "red", lwd=2 , yaxt= "n")

hist(bankMarkingData$job)
#marital
library(dplyr)
#  nrow(bankMarkingData[bankMarkingData$marital =="unknown", ])
#  
# bankMarkingData <- bankMarkingData %>%  filter(marital != "unknown") 

summary(bankMarkingData$marital)



marital <- bankMarkingData$marital
m <- mean(marital)
std <- sd(marital)
hist(marital, prob = T, col = "grey" , main="normal curve over histogram")
curve(dnorm(x, mean = m, sd= std), add = TRUE, col = "red", lwd=2, yaxt="n" )


#outliers 

age_outliers <- boxplot(bankMarkingData$age, main="Age",
   xlab="Age", horizontal=TRUE,
        col="#4682B433")$out
print(age_outliers)

#hist(bankMarkingData$age,col=terrain.colors(10))




#Nr.employed - Quarterly indicator of number of employees(nr.employed)
outliers_nr<- boxplot(bankMarkingData$nr.employed,main="Quarterly indicator of number of employees (nr.employed)",
                      xlab="Quarterly indicator of number of employees", horizontal=TRUE,
                      col="#4682B433" )$out
print(outliers_nr)

#Euribor3m -  Daily indicator of euribor 3 month rate 
outliers_euribor3m <- boxplot(bankMarkingData$euribor3m,main="Daily indicator of euribor 3 month rate (euribor3m)",
                              xlab="Daily indicator of euribor 3 month rate", horizontal=TRUE,
                              col="#4682B433")$out

print(outliers_euribor3m)

summary(bankMarkingData$euribor3m)

#Emp.var.rate - Quarterly indicator of employment variation rate 

outliers_emp.var.rate<- boxplot(bankMarkingData$emp.var.rate, main = "Quarterly indicator of employment variation rate(Emp.var.rate)",
                                xlab = "Quarterly indicator of employment variation rate", horizontal=TRUE,
                                col="#4682B433")$out

print(outliers_emp.var.rate)

summary(bankMarkingData$emp.var.rate)

#Cons.price.idx - Monthly indicator of consumer price index 
summary(bankMarkingData$cons.price.idx)

outliers_cons.price.idx <- boxplot(bankMarkingData$cons.price.idx, main = "Monthly indicator of consumer price index (cons.price.idx)",
                                   xlab = "Monthly indicator of consumer price index", horizontal=TRUE,
                                   col="#4682B433")$out
print(outliers_cons.price.idx)


#Cons.conf.idx - Monthly indicator of consumer confidence price index 
outliers_cons.conf.idx <- boxplot(bankMarkingData$cons.conf.idx, main = "Monthly indicator of consumer confidence price index  (Cons.conf.idx)",
                                  xlab = "Monthly indicator of consumer confidence price index", horizontal=TRUE,
                                  col="#4682B433")$out
print(outliers_cons.conf.idx)

summary(bankMarkingData$cons.conf.idx)

#Previous - Number of the contact before this campaign with this client 
outliers_previous <- boxplot(bankMarkingData$previous, main = "Number of the contact before this campaign with this client   (Previous)",
                             xlab = "Number of the contact before this campaign with this client", horizontal=TRUE,
                             col="#4682B433")$out
print(outliers_previous)

summary(bankMarkingData$previous)
table(bankMarkingData$previous)


#Campaign - Number of contacts with the client during this campaign including last contact 
outliers_campaign <- boxplot(bankMarkingData$campaign, main = "Number of contacts with the client during this campaign including last contact  (Campaign)",
                             xlab = "Number of contacts with the client during this campaign including last contact ", horizontal=TRUE,
                             col="#4682B433")$out
print(outliers_campaign)
summary(bankMarkingData$campaign)
table(bankMarkingData$campaign)



boxplot(bankMarkingData$pdays, main="pdays",
        yaxt="n", xlab="pdays", horizontal=TRUE,
        col=terrain.colors(2))
outliers_pdays<- boxplot(bankMarkingData$pdays)$out
print(outliers_pdays)



outlier_loan <- boxplot(bankMarkingData$loan, main = "loan",
                               xlab = "loan", horizontal=TRUE,
                               col="#4682B433")$out
print(outlier_loan)
summary(bankMarkingData$loan)
table(bankMarkingData$loan)

#job
outliers_job <- boxplot(bankMarkingData$job, main = "job",
                                  xlab = "job", horizontal=TRUE,
                                  col="#4682B433")$out




# IQR rule
#age
summary(bankMarkingData$age)
quantile(bankMarkingData$age)
Q1 <- quantile(bankMarkingData$age, 0.25)
Q3 <- quantile(bankMarkingData$age, 0.75)
cat("Q1:",Q1, "Q3: ",Q3)

IQR <- IQR(bankMarkingData$age)
cat("IQR: ", IQR)

q1_below <- Q1 - (1.5 * IQR) 
q1_over  <- Q3 +(1.5 * IQR)

inner_fence <- c(q1_below , q1_over)
cat("Inner hence below: ", q1_below , "Inner hence over: ", q1_over)

q3_below <- Q1 - (3 * IQR) 
q3_over  <- Q3 + (3 * IQR)
outer_fence <- c(q3_below , q3_over)
cat("Outer hence below: ", q3_below , "Outer hence over: ", q3_over)

#job
summary(bankMarkingData$job)
quantile(bankMarkingData$job)
Q1 <- quantile(bankMarkingData$job, 0.25)
Q3 <- quantile(bankMarkingData$job, 0.75)
cat("Q1:",Q1, "Q3: ",Q3)
IQR <- IQR(bankMarkingData$job)
cat("IQR: ", IQR)
q1_below <- Q1 - (1.5 * IQR) 
q1_over  <- Q3 +(1.5 * IQR)
inner_fence <- c(q1_below , q1_over)
cat("Inner hence below: ", q1_below , "Inner hence over: ", q1_over)

q3_below <- Q1 - (3 * IQR) 
q3_over  <- Q3 + (3 * IQR)
outer_fence <- c(q3_below , q3_over)
cat("Outer hence below: ", q3_below , "Outer hence over: ", q3_over)

#marital
summary(bankMarkingData$marital)
quantile(bankMarkingData$marital)
Q1 <- quantile(bankMarkingData$marital, 0.25)
Q3 <- quantile(bankMarkingData$marital, 0.75)
cat("Q1:",Q1, "Q3: ",Q3)

IQR <- IQR(bankMarkingData$marital)
cat("IQR: ", IQR)

q1_below <- Q1 - (1.5 * IQR) 
q1_over  <- Q3 +(1.5 * IQR)
inner_fence <- c(q1_below , q1_over)
cat("Inner hence below: ", q1_below , "Inner hence over: ", q1_over)

q3_below <- Q1 - (3 * IQR) 
q3_over  <- Q3 + (3 * IQR)
outer_fence <- c(q3_below , q3_over)
cat("Outer hence below: ", q3_below , "Outer hence over: ", q3_over)

#education
summary(bankMarkingData$education)
quantile(bankMarkingData$education)
Q1 <- quantile(bankMarkingData$education, 0.25)
Q3 <- quantile(bankMarkingData$education, 0.75)
cat("Q1:",Q1, "Q3: ",Q3)

IQR <- IQR(bankMarkingData$education)
cat("IQR: ", IQR)

q1_below <- Q1 - (1.5 * IQR) 
q1_over  <- Q3 +(1.5 * IQR)
inner_fence <- c(q1_below , q1_over)
cat("Inner hence below: ", q1_below , "Inner hence over: ", q1_over)

q3_below <- Q1 - (3 * IQR) 
q3_over  <- Q3 + (3 * IQR)
outer_fence <- c(q3_below , q3_over)
cat("Outer hence below: ", q3_below , "Outer hence over: ", q3_over)


#default
summary(bankMarkingData$default)
quantile(bankMarkingData$default)
Q1 <- quantile(bankMarkingData$default, 0.25)
Q3 <- quantile(bankMarkingData$default, 0.75)
cat("Q1:",Q1, "Q3: ",Q3)

IQR <- IQR(bankMarkingData$default)
cat("IQR: ", IQR)

q1_below <- Q1 - (1.5 * IQR) 
q1_over  <- Q3 +(1.5 * IQR)
inner_fence <- c(q1_below , q1_over)
cat("Inner hence below: ", q1_below , "Inner hence over: ", q1_over)

q3_below <- Q1 - (3 * IQR) 
q3_over  <- Q3 + (3 * IQR)
outer_fence <- c(q3_below , q3_over)
cat("Outer hence below: ", q3_below , "Outer hence over: ", q3_over)

#housing
summary(bankMarkingData$housing)
quantile(bankMarkingData$housing)
Q1 <- quantile(bankMarkingData$housing, 0.25)
Q3 <- quantile(bankMarkingData$housing, 0.75)
cat("Q1:",Q1, "Q3: ",Q3)

IQR <- IQR(bankMarkingData$housing)
cat("IQR: ", IQR)

q1_below <- Q1 - (1.5 * IQR) 
q1_over  <- Q3 +(1.5 * IQR)
inner_fence <- c(q1_below , q1_over)
cat("Inner hence below: ", q1_below , "Inner hence over: ", q1_over)

q3_below <- Q1 - (3 * IQR) 
q3_over  <- Q3 + (3 * IQR)
outer_fence <- c(q3_below , q3_over)
cat("Outer hence below: ", q3_below , "Outer hence over: ", q3_over)


#Loan
summary(bankMarkingData$loan)
quantile(bankMarkingData$loan)
Q1 <- quantile(bankMarkingData$loan, 0.25)
Q3 <- quantile(bankMarkingData$loan, 0.75)
cat("Q1:",Q1, "Q3: ",Q3)

IQR <- IQR(bankMarkingData$loan)
cat("IQR: ", IQR)

q1_below <- Q1 - (1.5 * IQR) 
q1_over  <- Q3 +(1.5 * IQR)
inner_fence <- c(q1_below , q1_over)
cat("Inner hence below: ", q1_below , "Inner hence over: ", q1_over)

q3_below <- Q1 - (3 * IQR) 
q3_over  <- Q3 + (3 * IQR)
outer_fence <- c(q3_below , q3_over)
cat("Outer hence below: ", q3_below , "Outer hence over: ", q3_over)


#contact
summary(bankMarkingData$contact)
quantile(bankMarkingData$contact)
Q1 <- quantile(bankMarkingData$contact, 0.25)
Q3 <- quantile(bankMarkingData$contact, 0.75)
cat("Q1:",Q1, "Q3: ",Q3)

IQR <- IQR(bankMarkingData$contact)
cat("IQR: ", IQR)

q1_below <- Q1 - (1.5 * IQR) 
q1_over  <- Q3 +(1.5 * IQR)
inner_fence <- c(q1_below , q1_over)
cat("Inner hence below: ", q1_below , "Inner hence over: ", q1_over)

q3_below <- Q1 - (3 * IQR) 
q3_over  <- Q3 + (3 * IQR)
outer_fence <- c(q3_below , q3_over)
cat("Outer hence below: ", q3_below , "Outer hence over: ", q3_over)

#month
summary(bankMarkingData$month)
quantile(bankMarkingData$month)
Q1 <- quantile(bankMarkingData$month, 0.25)
Q3 <- quantile(bankMarkingData$month, 0.75)
cat("Q1:",Q1, "Q3: ",Q3)

IQR <- IQR(bankMarkingData$month)
cat("IQR: ", IQR)

q1_below <- Q1 - (1.5 * IQR) 
q1_over  <- Q3 +(1.5 * IQR)
inner_fence <- c(q1_below , q1_over)
cat("Inner hence below: ", q1_below , "Inner hence over: ", q1_over)

q3_below <- Q1 - (3 * IQR) 
q3_over  <- Q3 + (3 * IQR)
outer_fence <- c(q3_below , q3_over)
cat("Outer hence below: ", q3_below , "Outer hence over: ", q3_over)

#day_of_week
summary(bankMarkingData$day_of_week)
quantile(bankMarkingData$day_of_week)
Q1 <- quantile(bankMarkingData$day_of_week, 0.25)
Q3 <- quantile(bankMarkingData$day_of_week, 0.75)
cat("Q1:",Q1, "Q3: ",Q3)

IQR <- IQR(bankMarkingData$day_of_week)
cat("IQR: ", IQR)

q1_below <- Q1 - (1.5 * IQR) 
q1_over  <- Q3 +(1.5 * IQR)
inner_fence <- c(q1_below , q1_over)
cat("Inner hence below: ", q1_below , "Inner hence over: ", q1_over)

q3_below <- Q1 - (3 * IQR) 
q3_over  <- Q3 + (3 * IQR)
outer_fence <- c(q3_below , q3_over)
cat("Outer hence below: ", q3_below , "Outer hence over: ", q3_over)

#Duration
summary(bankMarkingData$duration)
quantile(bankMarkingData$duration)
Q1 <- quantile(bankMarkingData$duration, 0.25)
Q3 <- quantile(bankMarkingData$duration, 0.75)
cat("Q1:",Q1, "Q3: ",Q3)

IQR <- IQR(bankMarkingData$duration)
cat("IQR: ", IQR)

q1_below <- Q1 - (1.5 * IQR) 
q1_over  <- Q3 +(1.5 * IQR)
inner_fence <- c(q1_below , q1_over)
cat("Inner hence below: ", q1_below , "Inner hence over: ", q1_over)

q3_below <- Q1 - (3 * IQR) 
q3_over  <- Q3 + (3 * IQR)
outer_fence <- c(q3_below , q3_over)
cat("Outer hence below: ", q3_below , "Outer hence over: ", q3_over)


#campaign
summary(bankMarkingData$campaign)
quantile(bankMarkingData$campaign)
Q1 <- quantile(bankMarkingData$campaign, 0.25)
Q3 <- quantile(bankMarkingData$campaign, 0.75)
cat("Q1:",Q1, "Q3: ",Q3)

IQR <- IQR(bankMarkingData$campaign)
cat("IQR: ", IQR)

q1_below <- Q1 - (1.5 * IQR) 
q1_over  <- Q3 +(1.5 * IQR)
inner_fence <- c(q1_below , q1_over)
cat("Inner hence below: ", q1_below , "Inner hence over: ", q1_over)

q3_below <- Q1 - (3 * IQR) 
q3_over  <- Q3 + (3 * IQR)
outer_fence <- c(q3_below , q3_over)
cat("Outer hence below: ", q3_below , "Outer hence over: ", q3_over)


#pdays
summary(bankMarkingData$pdays)
quantile(bankMarkingData$pdays)
Q1 <- quantile(bankMarkingData$pdays, 0.25)
Q3 <- quantile(bankMarkingData$pdays, 0.75)
cat("Q1:",Q1, "Q3: ",Q3)

IQR <- IQR(bankMarkingData$pdays)
cat("IQR: ", IQR)

q1_below <- Q1 - (1.5 * IQR) 
q1_over  <- Q3 +(1.5 * IQR)
inner_fence <- c(q1_below , q1_over)
cat("Inner hence below: ", q1_below , "Inner hence over: ", q1_over)

q3_below <- Q1 - (3 * IQR) 
q3_over  <- Q3 + (3 * IQR)
outer_fence <- c(q3_below , q3_over)
cat("Outer hence below: ", q3_below , "Outer hence over: ", q3_over)

#previous
summary(bankMarkingData$previous)
quantile(bankMarkingData$previous)
Q1 <- quantile(bankMarkingData$previous, 0.25)
Q3 <- quantile(bankMarkingData$previous, 0.75)
cat("Q1:",Q1, "Q3: ",Q3)

IQR <- IQR(bankMarkingData$previous)
cat("IQR: ", IQR)

q1_below <- Q1 - (1.5 * IQR) 
q1_over  <- Q3 +(1.5 * IQR)
inner_fence <- c(q1_below , q1_over)
cat("Inner hence below: ", q1_below , "Inner hence over: ", q1_over)

q3_below <- Q1 - (3 * IQR) 
q3_over  <- Q3 + (3 * IQR)
outer_fence <- c(q3_below , q3_over)
cat("Outer hence below: ", q3_below , "Outer hence over: ", q3_over)


#postcome
summary(bankMarkingData$poutcome)
quantile(bankMarkingData$poutcome)
Q1 <- quantile(bankMarkingData$poutcome, 0.25)
Q3 <- quantile(bankMarkingData$poutcome, 0.75)
cat("Q1:",Q1, "Q3: ",Q3)

IQR <- IQR(bankMarkingData$poutcome)
cat("IQR: ", IQR)

q1_below <- Q1 - (1.5 * IQR) 
q1_over  <- Q3 +(1.5 * IQR)
inner_fence <- c(q1_below , q1_over)
cat("Inner hence below: ", q1_below , "Inner hence over: ", q1_over)

q3_below <- Q1 - (3 * IQR) 
q3_over  <- Q3 + (3 * IQR)
outer_fence <- c(q3_below , q3_over)
cat("Outer hence below: ", q3_below , "Outer hence over: ", q3_over)


#Emp.var.rate
summary(bankMarkingData$emp.var.rate)
quantile(bankMarkingData$emp.var.rate)
Q1 <- quantile(bankMarkingData$emp.var.rate, 0.25)
Q3 <- quantile(bankMarkingData$emp.var.rate, 0.75)
cat("Q1:",Q1, "Q3: ",Q3)

IQR <- IQR(bankMarkingData$emp.var.rate)
cat("IQR: ", IQR)

q1_below <- Q1 - (1.5 * IQR) 
q1_over  <- Q3 +(1.5 * IQR)
inner_fence <- c(q1_below , q1_over)
cat("Inner hence below: ", q1_below , "Inner hence over: ", q1_over)

q3_below <- Q1 - (3 * IQR) 
q3_over  <- Q3 + (3 * IQR)
outer_fence <- c(q3_below , q3_over)
cat("Outer hence below: ", q3_below , "Outer hence over: ", q3_over)


#cons.price.idx
summary(bankMarkingData$cons.price.idx)
quantile(bankMarkingData$cons.price.idx)
Q1 <- quantile(bankMarkingData$cons.price.idx, 0.25)
Q3 <- quantile(bankMarkingData$cons.price.idx, 0.75)
cat("Q1:",Q1, "Q3: ",Q3)

IQR <- IQR(bankMarkingData$cons.price.idx)
cat("IQR: ", IQR)

q1_below <- Q1 - (1.5 * IQR) 
q1_over  <- Q3 +(1.5 * IQR)
inner_fence <- c(q1_below , q1_over)
cat("Inner hence below: ", q1_below , "Inner hence over: ", q1_over)

q3_below <- Q1 - (3 * IQR) 
q3_over  <- Q3 + (3 * IQR)
outer_fence <- c(q3_below , q3_over)
cat("Outer hence below: ", q3_below , "Outer hence over: ", q3_over)

#cons.conf.idx
summary(bankMarkingData$cons.conf.idx)
quantile(bankMarkingData$cons.conf.idx)
Q1 <- quantile(bankMarkingData$cons.conf.idx, 0.25)
Q3 <- quantile(bankMarkingData$cons.conf.idx, 0.75)
cat("Q1:",Q1, "Q3: ",Q3)

IQR <- IQR(bankMarkingData$cons.conf.idx)
cat("IQR: ", IQR)

q1_below <- Q1 - (1.5 * IQR) 
q1_over  <- Q3 +(1.5 * IQR)
inner_fence <- c(q1_below , q1_over)
cat("Inner hence below: ", q1_below , "Inner hence over: ", q1_over)

q3_below <- Q1 - (3 * IQR) 
q3_over  <- Q3 + (3 * IQR)
outer_fence <- c(q3_below , q3_over)
cat("Outer hence below: ", q3_below , "Outer hence over: ", q3_over)


#euribor3m
summary(bankMarkingData$euribor3m)
quantile(bankMarkingData$euribor3m)
Q1 <- quantile(bankMarkingData$euribor3m, 0.25)
Q3 <- quantile(bankMarkingData$euribor3m, 0.75)
cat("Q1:",Q1, "Q3: ",Q3)

IQR <- IQR(bankMarkingData$euribor3m)
cat("IQR: ", IQR)

q1_below <- Q1 - (1.5 * IQR) 
q1_over  <- Q3 +(1.5 * IQR)
inner_fence <- c(q1_below , q1_over)
cat("Inner hence below: ", q1_below , "Inner hence over: ", q1_over)

q3_below <- Q1 - (3 * IQR) 
q3_over  <- Q3 + (3 * IQR)
outer_fence <- c(q3_below , q3_over)
cat("Outer hence below: ", q3_below , "Outer hence over: ", q3_over)

#nr.employed
summary(bankMarkingData$nr.employed)
quantile(bankMarkingData$nr.employed)
Q1 <- quantile(bankMarkingData$nr.employed, 0.25)
Q3 <- quantile(bankMarkingData$nr.employed, 0.75)
cat("Q1:",Q1, "Q3: ",Q3)

IQR <- IQR(bankMarkingData$nr.employed)
cat("IQR: ", IQR)

q1_below <- Q1 - (1.5 * IQR) 
q1_over  <- Q3 +(1.5 * IQR)
inner_fence <- c(q1_below , q1_over)
cat("Inner hence below: ", q1_below , "Inner hence over: ", q1_over)

q3_below <- Q1 - (3 * IQR) 
q3_over  <- Q3 + (3 * IQR)
outer_fence <- c(q3_below , q3_over)
cat("Outer hence below: ", q3_below , "Outer hence over: ", q3_over)

summary(bankMarkingData)

#SD
# calculate summary statistics
#age 
summary(bankMarkingData$age)

data_mean <- mean(bankMarkingData$age)
data_sd <- sd(bankMarkingData$age)
SD3 <- data_sd * 3
lower <- data_mean - SD3
upper <- data_mean + SD3

# identify outliers
cat( "outliers below: ",lower, "outliers above: ", upper )

print(bankMarkingData$age < lower | bankMarkingData$age > upper)

#campaign
summary(bankMarkingData$campaign)

data_mean <- mean(bankMarkingData$campaign)
data_sd <- sd(bankMarkingData$campaign)
SD3 <- data_sd * 3
lower <- data_mean - SD3
upper <- data_mean + SD3

# identify outliers
cat( "outliers below: ",lower, "outliers above: ", upper )

#previous

summary(bankMarkingData$previous)

data_mean <- mean(bankMarkingData$previous)
data_sd <- sd(bankMarkingData$previous)
SD3 <- data_sd * 3
lower <- data_mean - SD3
upper <- data_mean + SD3

# identify outliers
cat( "outliers below: ",lower, "outliers above: ", upper )

#Poutcome
summary(bankMarkingData$poutcome)

data_mean <- mean(bankMarkingData$poutcome)
data_sd <- sd(bankMarkingData$poutcome)
SD3 <- data_sd * 3
lower <- data_mean - SD3
upper <- data_mean + SD3

# identify outliers
cat( "outliers below: ",lower, "outliers above: ", upper )

#emp.var.rate
summary(bankMarkingData$emp.var.rate)

data_mean <- mean(bankMarkingData$emp.var.rate)
data_sd <- sd(bankMarkingData$emp.var.rate)
SD3 <- data_sd * 3
lower <- data_mean - SD3
upper <- data_mean + SD3

# identify outliers
cat( "outliers below: ",lower, "outliers above: ", upper )


#cons.conf.idx
summary(bankMarkingData$cons.conf.idx)

data_mean <- mean(bankMarkingData$cons.conf.idx)
data_sd <- sd(bankMarkingData$cons.conf.idx)
SD3 <- data_sd * 3
lower <- data_mean - SD3
upper <- data_mean + SD3

# identify outliers
cat( "outliers below: ",lower, "outliers above: ", upper )


#cons.price.idx
summary(bankMarkingData$cons.price.idx)

data_mean <- mean(bankMarkingData$cons.price.idx)
data_sd <- sd(bankMarkingData$cons.price.idx)
SD3 <- data_sd * 3
lower <- data_mean - SD3
upper <- data_mean + SD3

# identify outliers
cat( "outliers below: ",lower, "outliers above: ", upper )


#euribor3m
summary(bankMarkingData$euribor3m)

data_mean <- mean(bankMarkingData$euribor3m)
data_sd <- sd(bankMarkingData$euribor3m)
SD3 <- data_sd * 3
lower <- data_mean - SD3
upper <- data_mean + SD3

# identify outliers
cat( "outliers below: ",lower, "outliers above: ", upper )

#nr.employed
summary(bankMarkingData$nr.employed)

data_mean <- mean(bankMarkingData$nr.employed)
data_sd <- sd(bankMarkingData$nr.employed)
SD3 <- data_sd * 3
lower <- data_mean - SD3
upper <- data_mean + SD3
# identify outliers
cat( "outliers below: ",lower, "outliers above: ", upper )

#marital

summary(bankMarkingData$marital)

data_mean <- mean(bankMarkingData$marital)
data_sd <- sd(bankMarkingData$marital)
SD3 <- data_sd * 3
lower <- data_mean - SD3
upper <- data_mean + SD3

# identify outliers
cat( "outliers below: ",lower, "outliers above: ", upper )


#job

summary(bankMarkingData$job)
data_mean <- mean(bankMarkingData$job)
data_sd <- sd(bankMarkingData$job)
SD3 <- data_sd * 3
lower <- data_mean - SD3
upper <- data_mean + SD3

# identify outliers
cat( "outliers below: ",lower, "outliers above: ", upper )


#education

summary(bankMarkingData$education)
data_mean <- mean(bankMarkingData$education)
data_sd <- sd(bankMarkingData$education)
SD3 <- data_sd * 3
lower <- data_mean - SD3
upper <- data_mean + SD3

# identify outliers
cat( "outliers below: ",lower, "outliers above: ", upper )


#default

summary(bankMarkingData$default)
data_mean <- mean(bankMarkingData$default)
data_sd <- sd(bankMarkingData$default)
SD3 <- data_sd * 3
lower <- data_mean - SD3
upper <- data_mean + SD3

# identify outliers
cat( "outliers below: ",lower, "outliers above: ", upper )

#housing

summary(bankMarkingData$housing)
data_mean <- mean(bankMarkingData$housing)
data_sd <- sd(bankMarkingData$housing)
SD3 <- data_sd * 3
lower <- data_mean - SD3
upper <- data_mean + SD3

# identify outliers
cat( "outliers below: ",lower, "outliers above: ", upper )

#loan

summary(bankMarkingData$loan)
data_mean <- mean(bankMarkingData$loan)
data_sd <- sd(bankMarkingData$loan)
SD3 <- data_sd * 3
lower <- data_mean - SD3
upper <- data_mean + SD3

# identify outliers
cat( "outliers below: ",lower, "outliers above: ", upper )


#contact

summary(bankMarkingData$contact)
data_mean <- mean(bankMarkingData$contact)
data_sd <- sd(bankMarkingData$contact)
SD3 <- data_sd * 3
lower <- data_mean - SD3
upper <- data_mean + SD3

# identify outliers
cat( "outliers below: ",lower, "outliers above: ", upper )

#month
summary(bankMarkingData$month)
data_mean <- mean(bankMarkingData$month)
data_sd <- sd(bankMarkingData$month)
SD3 <- data_sd * 3
lower <- data_mean - SD3
upper <- data_mean + SD3

# identify outliers
cat( "outliers below: ",lower, "outliers above: ", upper )

#Day of week
summary(bankMarkingData$day_of_week)
data_mean <- mean(bankMarkingData$day_of_week)
data_sd <- sd(bankMarkingData$day_of_week)
SD3 <- data_sd * 3
lower <- data_mean - SD3
upper <- data_mean + SD3

# identify outliers
cat( "outliers below: ",lower, "outliers above: ", upper )

#duration

summary(bankMarkingData$duration)
data_mean <- mean(bankMarkingData$duration)
data_sd <- sd(bankMarkingData$duration)
SD3 <- data_sd * 3
lower <- data_mean - SD3
upper <- data_mean + SD3

# identify outliers
cat( "outliers below: ",lower, "outliers above: ", upper )


#pdays

summary(bankMarkingData$pdays)
data_mean <- mean(bankMarkingData$pdays)
data_sd <- sd(bankMarkingData$pdays)
SD3 <- data_sd * 3
lower <- data_mean - SD3
upper <- data_mean + SD3

# identify outliers
cat( "outliers below: ",lower, "outliers above: ", upper )


#Q-Q Plots
qqnorm(bankMarkingData$education); qqline(bankMarkingData$education)

qqnorm(bankMarkingData$age); qqline(bankMarkingData$age)

bankMarkingData_sub <- subset(bankMarkingData, age < 70)
qqnorm(bankMarkingData_sub$age); qqline(bankMarkingData_sub$age)

qqnorm(bankMarkingData$campaign); qqline(bankMarkingData$campaign)

qqnorm(bankMarkingData$cons.conf.idx); qqline(bankMarkingData$cons.conf.idx)

#remove outliers
#age
bankMarkingData_sub <- subset(bankMarkingData, age < 68)
boxplot(bankMarkingData_sub$age, main="Age",
        yaxt="n", xlab="Age", horizontal=TRUE,
        col=terrain.colors(2))

summary(bankMarkingData_sub$age)

#cons.conf.idx
bankMarkingData_sub<- subset(bankMarkingData, cons.conf.idx < -26.95 )
boxplot(bankMarkingData_sub$cons.price.idx, main = "Monthly indicator of consumer confidence price index  (Cons.conf.idx)",
        xlab = "Monthly indicator of consumer confidence price index", horizontal=TRUE,
        col="#4682B433")

summary(bankMarkingData_sub$cons.conf.idx)


bankMarkingData_sub<- subset(bankMarkingData,cons.conf.idx < -26.9)
boxplot(bankMarkingData_sub$cons.price.idx, main = "Monthly indicator of consumer confidence price index  (Cons.conf.idx)",
        xlab = "Monthly indicator of consumer confidence price index", horizontal=TRUE,
        col="#4682B433")
summary(bankMarkingData_sub)

#previous

#campaign

#loan




summary(bankMarkingData_sub$cons.conf.idx)
summary(bankMarkingData$cons.conf.idx)
table(bankMarkingData$cons.conf.idx)
table(bankMarkingData_sub$cons.conf.idx)


bankMarkingData_sub<- subset(bankMarkingData,previous > 0)
boxplot(bankMarkingData_sub$previous, main = "previous (previous)",
        xlab = "previous", horizontal=TRUE,
        col="#4682B433")
summary(bankMarkingData_sub$previous)
table(bankMarkingData_sub$previous)


bankMarkingData_sub<- subset(bankMarkingData,campaign < 7)
boxplot(bankMarkingData_sub$campaign, main = "campaign (campaign)",
        xlab = "previous", horizontal=TRUE,
        col="#4682B433")

summary(bankMarkingData_sub$campaign)


summary(bankMarkingData_sub)











#visualization of variables

library(ggplot2)

#visualization between job and target

ggplot(data = bankMarkingData,aes(x=job))+geom_bar(mapping =aes(fill=y))+
      theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
      theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black")) 

#visualization between age and target
      
ggplot(data = bankMarkingData,aes(x=age))+geom_bar(mapping =aes(fill=y))+
  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black")) 

#visualization between marital and target

ggplot(data = bankMarkingData,aes(x=marital))+geom_bar(mapping =aes(fill=y))+
  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))

#visualization between education and target

ggplot(data = bankMarkingData,aes(x=education))+geom_bar(mapping =aes(fill=y))+
  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))

#visualization between job and contact

ggplot(bankMarkingData,aes(job))+geom_bar(aes(fill=contact))+
  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))

#visualization between default and target

ggplot(bankMarkingData,aes(default))+geom_bar(aes(fill=y))+
  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))


#ggplot visualization of default

summary(bankMarkingData$default)

ggplot(bankMarkingData, aes(default)) + geom_bar() +
  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))

#ggplot visualization of personal loan

summary(bankMarkingData$loan)

ggplot(bankMarkingData, aes(loan)) + geom_bar() +  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))

#visualization between loan and target

ggplot(bankMarkingData,aes(loan))+geom_bar(aes(fill=y))+
  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))


#ggplot visualization of housing loan

summary(bankMarkingData$housing)

ggplot(bankMarkingData, aes(housing)) + geom_bar() +  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))


#visualization between housing loan and target

ggplot(bankMarkingData,aes(housing))+geom_bar(aes(fill=y))+
  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))

#visualization between pevious and target

summary(bankMarkingData$previous)
table(bankMarkingData$previous)

ggplot(bankMarkingData,aes(previous))+geom_bar(aes(fill=y)) + 
  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))

#visualization between poutcome and target

ggplot(bankMarkingData,aes(poutcome))+geom_bar(aes(fill=y)) + 
  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))

table(bankMarkingData$poutcome, bankMarkingData$y)

ggplot(bankMarkingData, aes(poutcome)) + geom_bar() +  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))

#visualization between campaing and target

table(bankMarkingData$campaign, bankMarkingData$y)
summary(bankMarkingData$campaign)
tail(bankMarkingData$campaign,500)

table(bankMarkingData$campaign)



ggplot(bankMarkingData, aes(campaign)) + geom_bar() +  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))

ggplot(bankMarkingData,aes(campaign))+geom_bar(aes(fill=y)) + 
  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))

#visualization between month and target

ggplot(bankMarkingData, aes(month)) + geom_bar() +  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))

ggplot(bankMarkingData,aes(month))+geom_bar(aes(fill=y)) + 
  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))

#visualization between day and target

table(bankMarkingData$day_of_week)
table(bankMarkingData$day_of_week,bankMarkingData$y)


ggplot(bankMarkingData, aes(day_of_week)) + geom_bar() +  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))

ggplot(bankMarkingData,aes(day_of_week))+geom_bar(aes(fill=y)) + 
  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))

#visualization between duration and target

summary(bankMarkingData$duration)
table(bankMarkingData$duration , bankMarkingData$y)

ggplot(bankMarkingData, aes(duration)) + geom_bar(aes(fill=y)) +  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))

ggplot(bankMarkingData, aes(duration)) + geom_bar() +  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))

#visualization of Number of days from last contact with client from the previous campaign
ggplot(bankMarkingData, aes(pdays)) + geom_histogram(binwidth = 50) +  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(bankMarkingData, aes(pdays)) + geom_bar() +  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))

hist(bankMarkingData$pdays)


#visualization of Employment variation rate (quarterly)

summary(bankMarkingData$emp.var.rate)

ggplot(bankMarkingData, aes(emp.var.rate)) + geom_histogram(binwidth = 1) +  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#visualization of consumer price index 
summary(bankMarkingData$cons.price.idx)

ggplot(bankMarkingData, aes(cons.price.idx)) + geom_histogram(binwidth = 1) +  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#visualization of Consumer confidence index

summary(bankMarkingData$cons.conf.idx)
 
ggplot(bankMarkingData, aes(cons.conf.idx)) + geom_histogram(binwidth = 5) +  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#visualization of Daily indicator of euribor 3 month rate 

summary(bankMarkingData$euribor3m)

ggplot(bankMarkingData, aes(euribor3m)) + geom_histogram(binwidth = 0.01) + theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#visualization of Number of employees

summary(bankMarkingData$nr.employed)

ggplot(bankMarkingData, aes(nr.employed)) + geom_histogram(binwidth = 60)+ theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))








boxplot(bankMarkingData$default)

plot( bankMarkingData$housing, bankMarkingData$y,
      xlab="Housing", ylab="Become Customer?", col=c("red","green"))



#ggplot(bankMarkingData$age, aes(x = "age", y = refund_value)) +
  #geom_boxplot()



#overview of the class of each variable if converting is need it
sapply(bankMarkingData, class)

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

sapply(bankMarkingData, class)





summary(bankMarkingData$marital)
table(bankMarkingData$marital)
table(bankMarkingData$contact)



summary(bankMarkingData$education)


boxplot(bankMarkingData$education)$out

table(bankMarkingData$job)
table(bankMarkingData$default)
table(bankMarkingData$previous)
table(bankMarkingData$age)
table(bankMarkingData$marital)


#sum of missing values

sum(is.na(bankMarkingData))
levels(bankMarkingData$job)
summary(bankMarkingData$marital)

#slipt data into training and test set

set.seed(12345)
size_data <- nrow(bankMarkingData) * 0.7
bank_data <- sample(1:nrow(bankMarkingData), size = size_data)
bank_data_training <- bankMarkingData[bank_data,]
bank_data_testing <- bankMarkingData[-bank_data,]

summary(bank_data_training)
summary(bank_data_testing)

dim(bankMarkingData)
dim(bank_data_training)
dim(bank_data_testing)

#to check  imbalancing or not
table(bank_data_training$y)
table(bank_data_testing$y)


#decision tree
#CART(regression and classification part)
library(rpart)
library(rpart.plot)

#Classification trees

#Fully grown trees
#build the model
model_class <- rpart(y~., data = bank_data_training, method = "class" )

summary(model_class)
#plot visualization of the tree
rpart.plot(model_class)

#predict
rpart_predict_class <- predict(model_class,bank_data_testing,type="class")
#mean(rpart_predict_class==bank_data_testing$y)

#confusion matrix
library(gmodels)


confusion_matrix <- prop.table(table(pred=rpart_predict_class,true=bank_data_testing$y))
confusion_matrix

# confusionMatrix <- table(pred=rpart_predict_class,true=bank_data_testing$y)
# confusionMatrix

confusionMatrix <- CrossTable(bank_data_testing$y, rpart_predict_class,
                              prop.chisq = F, prop.c = F, prop.r = F,
                              dnn = c('actual', 'predicted'))

accuracy<- confusion_matrix[1,1] + confusion_matrix[2,2] 
 accuracy


# bank_data_training.rpart <- rpart(y ~ ., data= bank_data_training, method = "anova")
# 
# rpart.plot(bank_data_training.rpart)
# predictions <- predict(bank_data_training.rpart, bankMarkingData, type = "class")
# confusion.matrix <- prop.table(table(predictions, bankMarkingData$y))
# 
# 
# accuracy <- confusion.matrix[1,1] + confusion.matrix[2,2] 
# accuracy




fancyRpartPlot(rpart_predict_class)



#simple linear regression

# simpleLm <- lm(y~age, data = bankMarkingData)
# summary(simpleLm)
# 
# 
# simple_linear <- lm( y~ age, bankMarkingData)
# summary(simple_linear)
# 
# cor(bankMarkingData$age, bankMarkingData$y)
# 
# confint(simple_linear)
# sigma(simple_linear)*100/mean(bankMarkingData$y)
# 
# 
# predit_output <- predict(simple_linear, bankMarkingData)
# cor(bankMarkingData$age, bankMarkingData$y)
# attach(bank_data_training)
# 
# model1 <- lm(y~., data = bank_data_training)
# summary(model1)
# predit_output_1 <- predict(model1, bank_data_testing)
# summary(predit_output_1)
# mean(100*(bank_data_testing$y - predit_output_1)^2)

######################################

m <- lm(y~., data = bankMarkingData)
summary(m)

