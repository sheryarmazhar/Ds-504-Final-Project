---
  title: "Ds-504 Final Project "
author: "Sheryar Mazhar"
date: "Nov 08, 2018"
output:
  word_document: default
pdf_document: default
---
  
  
rm(list=ls())

# Load Library
install.packages("ROSE")
library(ROSE)
install.packages("caret")
library(caret)
library(dplyr)
install.packages("gmodels")
library(gmodels)       # accuracy Calc 
install.packages("pROC")
library(pROC)  
library(class)        # kNN
install.packages("ROCR")
library(ROCR)

getwd()
setwd("C:/Users/smazhar/Downloads")
mydata<-read.csv("C:/Users/smazhar/Downloads/lifelinecustdata.csv")

# For Exploring data
str(mydata)

############################# 1. Descriptive Stats #####################################
# user written function for creating descriptive statistics
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}

vars <- c( "SeriousDlqin2yrs" , "RevolvingUtilizationOfUnsecuredLines" ,  "age",   
           "NumberOfTime30.59DaysPastDueNotWorse" , "DebtRatio", "MonthlyIncome" , "NumberOfOpenCreditLinesAndLoans" ,
           "NumberOfTimes90DaysLate" , "NumberRealEstateLoansOrLines" , "NumberOfTime60.89DaysPastDueNotWorse" , 
           "NumberOfDependents")

diag_stats<-t(data.frame(apply(mydata[vars], 2, mystats)))

# Writing Summary stats to external file
write.csv(diag_stats, file = "diag_stats.csv")

#Outliers
mydata$age[mydata$age>87]<- 87
mydata$RevolvingUtilizationOfUnsecuredLines[mydata$RevolvingUtilizationOfUnsecuredLines>1]<- 1
mydata$NumberOfTime30.59DaysPastDueNotWorse[mydata$NumberOfTime30.59DaysPastDueNotWorse>12.9993772]<- 13
mydata$DebtRatio[mydata$DebtRatio>6466.4650758]<- 6466.4650758
mydata$NumberOfOpenCreditLinesAndLoans[mydata$NumberOfOpenCreditLinesAndLoans>23.890613]<- 24
mydata$NumberOfTimes90DaysLate[mydata$NumberOfTimes90DaysLate>12.7738847]<- 13
mydata$NumberRealEstateLoansOrLines[mydata$NumberRealEstateLoansOrLines>4.407553]<- 4
mydata$NumberOfTime60.89DaysPastDueNotWorse[mydata$NumberOfTime60.89DaysPastDueNotWorse>12.7059249]<- 13
mydata$MonthlyIncome[mydata$MonthlyIncome>45311.57]<- 45311.57
mydata$NumberOfDependents[mydata$NumberOfDependents>4]<- 4

summary(mydata)

#missing values
mydata$MonthlyIncome[is.na(mydata$MonthlyIncome ==  TRUE)] <- 6460
mydata$NumberOfDependents[is.na(mydata$NumberOfDependents ==  TRUE)] <- 1
mydata$age[mydata$age==0]<- 52

cor_matrix<-data.frame(cor(mydata))
write.csv(cor_matrix, file = "cor_matrix.csv")

plot(cor_matrix)



############################# 2. SAMPLING #####################################

## Creating Training & Validation Data sets(70:30)
set.seed(125)
smp_size <- floor(0.70 * nrow(mydata))
train_ind <- sample(seq_len(nrow(mydata)), size = smp_size)
training <- mydata[train_ind, ]
testing <- mydata[-train_ind, ]


#See how much balanced the training data is!
table(training$SeriousDlqin2yrs)    #by Counts

prop.table(table(mydata$SeriousDlqin2yrs))  #by %
plot(Totaldata)
prop.table(table(training$SeriousDlqin2yrs))  #by %
prop.table(table(testing$SeriousDlqin2yrs))  #by %

############################# 2B. SUB-SET of DATA #####################################
#kNN usually takes a long time for computation in case input data is large!
#Hemce, Considering only a subset of the data for Demo!

trainset <- training[1:2500,]
testset <- testing[1:1500,]

#Convert 2 Factors
trainset$SeriousDlqin2yrs <- factor(trainset$SeriousDlqin2yrs) 
testset$SeriousDlqin2yrs <- factor(testset$SeriousDlqin2yrs) 

############################ 3. LOGISTIC REGRESSION ####################################
#A base model for Comparison
glm_fit <- glm(SeriousDlqin2yrs~.,        
               data = trainset, 
               family = binomial(logit))
summary(glm_fit)


pred.fc <- predict.glm(glm_fit, newdata = testset, type="response")
accuracy.meas(testset$SeriousDlqin2yrs, pred.fc)  
roc.curve(testset$SeriousDlqin2yrs, pred.fc, plotit = T)

#Accracy thru Confusion Matrix
z=table(testset$SeriousDlqin2yrs,pred.fc > 0.5)
(acc.fc<-(z[1,1]+z[2,2])/sum(z))   

############################# 4. kNN MODEL ###################################################
#DECIDE OPTIMAL K WITH CARET

set.seed(500)

ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5)

knnFit <- train(SeriousDlqin2yrs ~ ., data = trainset, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 5)

#Output of kNN fit
knnFit
plot(knnFit)

#Final Prediction
knnPredict <- predict(knnFit,newdata = testset)

#Accuracy Check
table(knnPredict,testset$SeriousDlqin2yrs)
mean(knnPredict==testset$SeriousDlqin2yrs)
confusionMatrix(knnPredict, testset$SeriousDlqin2yrs)


############################# AUC & ROC  ########################################
#Model Fit
knnPredict <- predict(knnFit,newdata = testset)
accuracy.meas(testset$SeriousDlqin2yrs, knnPredict)  
roc.curve(testset$SeriousDlqin2yrs, pred.fc, plotit = T)







