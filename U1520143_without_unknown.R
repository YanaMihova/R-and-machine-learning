#load data
bankMarkingData <- read.csv("bank-additional-full.csv", header = TRUE, sep = ";" , stringsAsFactors = TRUE)

summary(bankMarkingData)

#visualization
library(ggplot2)

ggplot(data = bankMarkingData,aes(x=marital))+geom_bar(mapping =aes(fill=y))+
  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))

#visualization between job and target

ggplot(data = bankMarkingData,aes(x=job))+geom_bar(mapping =aes(fill=y))+
  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black")) 

#visualization between default and target

ggplot(bankMarkingData,aes(default))+geom_bar(aes(fill=y))+
  theme(legend.text = element_text(color = "black", face = "bold", size = 15)) + 
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black"))


#visualization between education and target

ggplot(data = bankMarkingData,aes(x=education))+geom_bar(mapping =aes(fill=y))+
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


#remove unknown
library(dplyr)
#marital
  nrow(bankMarkingData[bankMarkingData$marital =="unknown", ])
  
 bankMarkingData <- bankMarkingData %>%  filter(marital != "unknown") 
 
#job
 nrow(bankMarkingData[bankMarkingData$job =="unknown", ])
 
 bankMarkingData <- bankMarkingData %>%  filter(job != "unknown") 
 
#default
 nrow(bankMarkingData[bankMarkingData$default =="yes", ])
 
 bankMarkingData <- bankMarkingData %>%  filter(default != "yes") 
 
#education
 nrow(bankMarkingData[bankMarkingData$education =="unknown", ])
 
 bankMarkingData <- bankMarkingData %>%  filter(education != "unknown") 
 
 nrow(bankMarkingData[bankMarkingData$education =="illiterate", ])
 
 bankMarkingData <- bankMarkingData %>%  filter(education != "illiterate") 
 
 
 #loan
 nrow(bankMarkingData[bankMarkingData$loan =="unknown", ])
 
 bankMarkingData <- bankMarkingData %>%  filter(loan != "unknown") 
 
#housing
 nrow(bankMarkingData[bankMarkingData$housing =="unknown", ])
 
 bankMarkingData <- bankMarkingData %>%  filter(housing != "unknown") 
 
 
 
summary(bankMarkingData)
summary(bankMarkingData$job)
  summary(bankMarkingData$marital)
summary(bankMarkingData$default)
summary(bankMarkingData$education)
summary(bankMarkingData$housing)


#remove outliers
#age
bankMarkingData_sub<- subset(bankMarkingData, age < 69)
boxplot(bankMarkingData_sub$age, main="Age",
        yaxt="n", xlab="Age", horizontal=TRUE,
        col=terrain.colors(2))

summary(bankMarkingData_sub$age)

#cons.conf.idx
#cons.conf.idx
bankMarkingData_sub<- subset(bankMarkingData, cons.conf.idx > -26.95 )
boxplot(bankMarkingData_sub$cons.price.idx, main = "Monthly indicator of consumer confidence price index  (Cons.conf.idx)",
        xlab = "Monthly indicator of consumer confidence price index", horizontal=TRUE,
        col="#4682B433")

summary(bankMarkingData_sub$cons.conf.idx)


#cons.conf.idx
bankMarkingData_sub<- subset(bankMarkingData, cons.conf.idx < -26.9 )
boxplot(bankMarkingData_sub$cons.price.idx, main = "Monthly indicator of consumer confidence price index  (Cons.conf.idx)",
        xlab = "Monthly indicator of consumer confidence price index", horizontal=TRUE,
        col="#4682B433")

summary(bankMarkingData_sub$cons.conf.idx)

#previous
outliers_previous <- boxplot(bankMarkingData$previous, main = "Number of the contact before this campaign with this client   (Previous)",
                             xlab = "Number of the contact before this campaign with this client", horizontal=TRUE,
                             col="#4682B433")$out
print(outliers_previous)

bankMarkingData_sub<- subset(bankMarkingData,previous > 0)
boxplot(bankMarkingData_sub$previous, main = "previous (previous)",
        xlab = "previous", horizontal=TRUE,
        col="#4682B433")
summary(bankMarkingData_sub$previous)
table(bankMarkingData_sub$previous)

#campaign
outliers_campaign <- boxplot(bankMarkingData$campaign, main = "Number of contacts with the client during this campaign including last contact  (Campaign)",
                             xlab = "Number of contacts with the client during this campaign including last contact ", horizontal=TRUE,
                             col="#4682B433")$out
print(outliers_campaign)

bankMarkingData_sub2<- subset(bankMarkingData, campaign < 7 )
boxplot(bankMarkingData_sub$campaign, main = "campaing ",
        xlab = "campaing", horizontal=TRUE,
        col="#4682B433")

summary(bankMarkingData_sub$campaign)
table(bankMarkingData$campaign)

#loan
#remove unknown
nrow(bankMarkingData[bankMarkingData$loan =="unknown", ])

bankMarkingData <- bankMarkingData %>%  filter(loan != "unknown")

#check outlier for loan
outlier_loan <- boxplot(bankMarkingData$loan, main = "loan",
                        xlab = "loan", horizontal=TRUE,
                        col="#4682B433")$out
print(outlier_loan)


#default
outliers_default <- boxplot(bankMarkingData$default, main = "default",
                             xlab = "default", horizontal=TRUE,
                             col="#4682B433")$out
print(outliers_default)


bankMarkingData_sub<- subset(bankMarkingData, default < 2  )
boxplot(bankMarkingData_sub$cons.price.idx, main = "default ",
        xlab = "default", horizontal=TRUE,
        col="#4682B433")


#poutcome
outliers_poutcome <- boxplot(bankMarkingData$poutcome, main = "poutcome",
                            xlab = "poutcome", horizontal=TRUE,
                            col="#4682B433")$out
print(outliers_poutcome)


summary(bankMarkingData_sub)
summary(bankMarkingData)

 


bank_new <- data.frame(bankMarkingData_sub1, bankMarkingData_sub2)

summary(bank_new)




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
bankMarkingData$y<- as.numeric(bankMarkingData$y)


# bankMarkingData_sub$age <- cut(bankMarkingData_sub$age, c(1,20,40,60,100))
# bankMarkingData_sub$is_divorced <- ifelse( bankMarkingData_sub$marital == "divorced", 1, 0)
# bankMarkingData_sub$is_single <- ifelse( bankMarkingData_sub$marital == "single", 2, 0)
# bankMarkingData_sub$is_married <- ifelse( bankMarkingData_sub$marital == "married", 3, 0)
# bankMarkingData_sub$marital <- NULL
# bankMarkingData_sub$is_admin <- ifelse(bankMarkingData_sub$job == "admin")
# bankMarkingData_sub$job <- NULL
# str(bankMarkingData_sub)


#histogram normal curve distribution
#marital

marital <- bankMarkingData$marital
m <- mean(marital)
std <- sd(marital)
hist(marital, prob = T, col = "grey" , main="curve over histogram")
curve(dnorm(x, mean = m, sd= std), add = TRUE, col = "red", lwd=2, yaxt="n" )
pnorm(3.7, mean = 2.17, sd = 0.61 ) - pnorm(-3.7,mean=2.17,sd=0.61)

qqnorm(bankMarkingData$marital); qqline(bankMarkingData$marital)


#job
job <- bankMarkingData$job
summary(bankMarkingData$job)
m <- mean(job)
std <- sd(job)
hist(job, prob = T,col = "grey" , main=" curve over histogram")
curve(dnorm(x, mean = m, sd= std), add = TRUE, col = "red", lwd=2, yaxt="n")
pnorm(12, mean = 4.72, sd = 3.59 ) - pnorm(-12,mean=4.72,sd=3.59)

qqnorm(bankMarkingData$job); qqline(bankMarkingData$job)


#default
default <- bankMarkingData$default

m <- mean(default)
std <- sd(default)
hist(default, prob = T, col = "grey" , main=" curve over histogram")
curve(dnorm(x, mean = m, sd= std), add = TRUE, col = "red", lwd=2, yaxt="n")

qqnorm(bankMarkingData$default); qqline(bankMarkingData$default)

#education
education <- bankMarkingData$education

m <- mean(education)
std <- sd(education)
hist(education, prob = T, col = "grey" , main=" curve over histogram")
curve(dnorm(x, mean = m, sd= std), add = TRUE, col = "red", lwd=2, yaxt="n")

qqnorm(bankMarkingData$education); qqline(bankMarkingData$education)

#loan

loan <- bankMarkingData$loan

m <- mean(loan)
std <- sd(loan)
hist(loan, prob = T, col = "grey" , main=" curve over histogram")
curve(dnorm(x, mean = m, sd= std), add = TRUE, col = "red", lwd=2, yaxt="n")


qqnorm(bankMarkingData$loan); qqline(bankMarkingData$loan)

#housing

housing <- bankMarkingData$housing

m <- mean(housing)
std <- sd(housing)
hist(housing, prob = T, col = "grey" , main=" curve over histogram")
curve(dnorm(x, mean = m, sd= std), add = TRUE, col = "red", lwd=2, yaxt="n")

qqnorm(bankMarkingData$housing); qqline(bankMarkingData$housing)

#outlier 
outlier_loan <- boxplot(bankMarkingData$loan, main = "loan",
                        xlab = "loan", horizontal=TRUE,
                        col="#4682B433")$out
print(outlier_loan)



#job
outliers_job <- boxplot(bankMarkingData$job, main = "job",
                        xlab = "job", horizontal=TRUE,
                        col="#4682B433")$out
print(outliers_job)



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

###########################################################
#decision tree
#CART(regression and classification trees)
library(dplyr)
library(rpart)
library(rpart.plot)
library(rattle)

#Classification trees

#Fully grown trees
#build the model
model_class <- rpart(y~., data = bank_data_training , method = "class" )

summary(model_class)
print(model_class)
#plot visualization of the tree
#rpart.plot(model_class)
fancyRpartPlot(model_class)

#predict, test and evaluate
rpart_predict_class <- predict(model_class,bank_data_testing,type="class")
mean(rpart_predict_class==bank_data_testing$y)


#confusion matrix
library(gmodels)


confusion_matrix <- prop.table(table(pred=rpart_predict_class,true=bank_data_testing$y))
confusion_matrix

# confusionMatrix <- table(pred=rpart_predict_class,true=bank_data_testing$y)
# confusionMatrix

confusionMatrix <- CrossTable(bank_data_testing$y, rpart_predict_class,
                              prop.chisq = F, prop.c = F, prop.r = F,
                              dnn = c('actual', 'predicted'))

accuracy<-  round(100 * (confusion_matrix[1,1] + confusion_matrix[2,2]), digits = 2 )  
cat('Accuracy percentange in CART (classification trees): ', accuracy,'%')

#confusionMatrix(as.factor(bank_data_testing$y), as.factor(rpart_predict_class))

# #cost-complexity pruning
# 
# printcp(model_class)
# 
# # get index of CP with lowest xerror
# # opt <- which.min(model_class$cptable[,"xerror"])
# # #get its value
# # cp <- model_class$cptable[opt, "CP"]
# # 
# # #prune tree
# # pruned_model <- prune(model_class,cp)
# pruned_model<- train(y~., data = bank_data_training, method = "rpart",
#                      trControl = trainControl("cv", number = 10),
#                      tuneLength = 10)
# summary(pruned_model)
# pruned_model$bestTune
# #plot tree
# rpart.plot(pruned_model)
# 
# #predictions using test set of the pruned tree
# # rpart_pruned_predict <- predict(pruned_model, bank_data_testing,type="class")
# rpart_predict_class <- pruned_model %>% predict(bank_data_testing)
# mean(rpart_pruned_predict==bank_data_testing$y)

################
#C5.0
#install.packages("C50")
library(C50)
#build the model
C50_model <- C5.0(bank_data_training[-21],bank_data_training$y)
print(C50_model)
summary(C50_model)

#plot visualization of the tree
plot(C50_model)

#predict, test, evaluate
C50_predict <- predict(C50_model, bank_data_testing)
summary(C50_predict)

#accuracy results
mean(C50_predict==bank_data_testing$y)

confusion_matrix_1 <- prop.table(table(pred=C50_predict,true=bank_data_testing$y))
confusion_matrix_1
accuracy<-  round(100 * (confusion_matrix_1[1,1] + confusion_matrix_1[2,2]), digits = 2 )  
cat('Accuracy percentange in C5.0 (classification trees): ', accuracy,'%')

# confusionMatrix <- table(pred=rpart_predict_class,true=bank_data_testing$y)
# confusionMatrix

confusionMatrix1 <- CrossTable(bank_data_testing$y, C50_predict,
                              prop.chisq = F, prop.c = F, prop.r = F,
                              dnn = c('actual', 'predicted'))


#improving the model predictions
#three iterations of boosting procedure

c5_boost_tree <- C5.0(x= bank_data_training[ -21], y = bank_data_training$y, trials = 3)

#summary
summary(c5_boost_tree)

#predict boost model
c5_boost_tree_predict = predict(c5_boost_tree, bank_data_testing)

#accuracy
mean(c5_boost_tree_predict==bank_data_testing$y)

CrossTable(bank_data_testing$y,
           c5_boost_tree_predict,
           prop.chisq = FALSE,
           prop.c     = FALSE,
           prop.r     = FALSE,
           dnn = c('actual ', 'predicted '))


#rule based model

rule_tree_mode <- C5.0(x= bank_data_training[ -21], y = bank_data_training$y, rules = TRUE)
summary(rule_three_mode)

#predict rule mode tree
rule_mode_predict <- predict(rule_tree_mode, newdata = bank_data_testing[-21], type ="prob")
#mean(rule_mode_predict==bank_data_testing$y)
#Cost-Sensitive Models
cost_matrix <- matrix(c(0,2,1,0), nrow = 2)
rownames(cost_matrix) <- colnames(cost_matrix) <- c("no", "yes")
cost_matrix

cost_sens_mode <- C5.0(x= bank_data_training[-21], y = bank_data_training$y,
                       costs = cost_matrix)

summary(cost_matrix)

table(predict(cost_sens_mode, bank_data_testing[-21]))
table(predict(C50_model, bank_data_testing[-21]))




######
# install.packages("caret")
# library(caret)
# 
# install.packages("randomForest")
# library(randomForest)
# model_random_forest <- randomForest(y ~ ., data=bank_data_training)
# model_random_forest
# 
# #importance of each predictor
# importance(model_random_forest)
# #predict random forest test
# predicted <- predict(model_random_forest, bank_data_testing)
# table(predicted)
# confusionMatrix(predicted, bank_data_testing$y)
# 
# mean(predicted==bank_data_testing$y)

###########################3

#Decission tree classifier using caret package
#install.packages("caret")
library(caret)
library(rpart.plot)

#Training the Decision Tree classifier with criterion as information gain
trcontrol <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(12345)
dt_infoGain<- train(y ~., data = bank_data_training, method = "rpart",
                    parms = list(split = "information"),
                    trControl=trcontrol,
                    tuneLength = 10 )
dt_infoGain

#visualizate 
prp(dt_infoGain$finalModel, box.palette = "Reds", tweak = 1.2)

#predict


dt_infoGain_testing <- predict(dt_infoGain, newdata = bank_data_testing)
confusionMatrix(dt_infoGain_testing, bank_data_testing$y)

#Training the Decision Tree classifier with criterion as gini index
dt_gini<- train(y ~., data = bank_data_training, method = "rpart",
                    parms = list(split = "gini"),
                    trControl=trcontrol,
                    tuneLength = 10 )
dt_gini

prp(dt_gini$finalModel, box.palette = "Reds", tweak = 1.2)

#predict
dt_gini_testing <- predict(dt_gini, newdata = bank_data_testing)
confusionMatrix(dt_gini_testing, bank_data_testing$y)


###############################################
# #Decission Tree CHAID
# #install.packages("CHAID", repos="http://R-Forge.R-project.org")
# library(partykit)
# library(CHAID)
# 
# 
# # bankMarkingData %>% 
# #    select_if(is.factor)%>% ncol
# # 
# # bankMarkingData %>% 
# #   select_if(is.numeric) %>% ncol
# # 
# # bankMarkingData %>%
# #   select_if(function(col)
# #     length(unique(col)) <= 10 & is.integer(col)) %>%
# #   head
# #   
#        bankMarkingData %>%
#                       mutate(
#                              age = factor(age),
#                              job = factor(job),
#                              marital = factor(marital),
#                              education = factor(education),
#                              default = factor(default),
#                              housing =  factor(housing),
#                              loan   = factor(loan),
#                              contact = factor(contact),
#                              month  = factor(month),
#                              day_of_week = factor(day_of_week),
#                              duration = factor(duration),
#                              campaign = factor(campaign),
#                              pdays  = factor(pdays),
#                              previous = factor(previous),
#                              poutcome = factor(poutcome),
# 
#                              emp.var.rate  = factor(emp.var.rate),
#                              cons.price.idx = factor(cons.price.idx),
#                              cons.conf.idx = factor(cons.conf.idx),
#                              euribor3m  = factor(euribor3m),
#                              nr.employed = factor(nr.employed),
#                              y = factor(y)
#   ) %>%
#   str
# 
#        set.seed(12345)
#        size_data <- nrow(bankMarkingData) * 0.7
#        bank_data <- sample(1:nrow(bankMarkingData), size = size_data)
#        bank_data_training <- bankMarkingData[bank_data,]
#        bank_data_testing <- bankMarkingData[-bank_data,]
#        
#install.packages("processanimateR")
# install.packages("caret") 
# library(caret)
# library(processanimateR)

# bankMarkingData %>% select_if(is.factor)
# 
# chaid_model <- chaid(y~., data = bankMarkingData,
#                      control = chaid_control(minprob = 0.001,
#                                              minsplit = 500,minbucket = 200))
#            plot(chaid_model, 
#                 uniform = T, 
#                 compress = T, 
#                 margin = 0.2, 
#                 branch = 0.3)
#      # Label on Decision Tree
#      text(chaid_model, 
#           use.n = T, 
#           digits = 3, 
#           cex = 0.6)
#      summary(chaid_model)

# BANK <- bankMarkingData[sample(1:nrow(bankMarkingData), 1000),]
# ctrl <- chaid_control(minsplit = 20, minbucket = 5, minprob = 0)
# #f <- reformulate(setdiff(colnames(bankMarkingData), "y"), response="y")
# chaidb <- chaid(y ~ marital, data = BANK, control = ctrl)
# print(chaidb)

####################################################3
#Linear Regression
#Simple linear Regression

#visualizate
plot( bankMarkingData$age)


#corelation
cor(bankMarkingData$y, bankMarkingData$age)
cor(bankMarkingData$y, bankMarkingData$job)
cor(bankMarkingData$y, bankMarkingData$marital)
cor(bankMarkingData$y, bankMarkingData$education)
cor(bankMarkingData$y, bankMarkingData$default)
cor(bankMarkingData$y, bankMarkingData$housing)
cor(bankMarkingData$y, bankMarkingData$loan)
cor(bankMarkingData$y, bankMarkingData$contact)
cor(bankMarkingData$y, bankMarkingData$month)
cor(bankMarkingData$y, bankMarkingData$day_of_week)
cor(bankMarkingData$y, bankMarkingData$duration)
cor(bankMarkingData$y, bankMarkingData$campaign)
cor(bankMarkingData$y, bankMarkingData$pdays)
cor(bankMarkingData$y, bankMarkingData$previous)
cor(bankMarkingData$y, bankMarkingData$poutcome)
cor(bankMarkingData$y, bankMarkingData$emp.var.rate)
cor(bankMarkingData$y, bankMarkingData$cons.price.idx)
cor(bankMarkingData$y, bankMarkingData$cons.conf.idx)
cor(bankMarkingData$y, bankMarkingData$euribor3m)
cor(bankMarkingData$y, bankMarkingData$nr.employed)

#scatter.smooth(x= bankMarkingData$duration, y = bankMarkingData$y)
simpleLm <- lm(y~age, data = bankMarkingData)
summary(simpleLm)
plot(simpleLm)

du <- lm(y~ duration, data =  bankMarkingData)
summary(du)

marital_lm <- lm(y~ marital, data = bankMarkingData)

summary(marital_lm)






