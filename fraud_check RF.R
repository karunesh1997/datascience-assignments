Fraud_check <- read.csv("C:/Users/Ratnesh/Downloads/Fraud_check.csv")
View(Fraud_check)

str(Fraud_check)
range(Fraud_check$Taxable.Income)
Fraud_check$Taxable.Income = cut(Fraud_check$Taxable.Income, br=c(10000,30000,99619),lavels=c("Risky","Good"))

summary(Fraud_check)

table(Fraud_check$Taxable.Income)

## data prtition
set.seed(1234)
ind<- sample(2,nrow(Fraud_check),replace = T,prob = c(0.80,0.20))
train<-Fraud_check[ind==1,]
test<- Fraud_check[ind==2,]

#building the model on traning data
library(randomForest)
set.seed(222)
rf<-randomForest(Taxable.Income~.,data = train)
print(rf)# no of tree = 500
# error rate = 23.12
# acc = 77%

attributes(rf)
library(caret)
pd1<- predict(rf,train)
head(pd1)#it show correct
# compare with real data
confusionMatrix(pd1,train$Taxable.Income)# acc=90%

## check the accuracy
pd2<-predict(rf,test)
confusionMatrix(pd2,test$Taxable.Income)# acc= 78%

plot(rf)

############## Bagging ###########################

library(C50)
## build the model
fittree<- C5.0(train$Taxable.Income~.,data=train)
## predicting
pred<- predict.C5.0(fittree,test)
table<- table(test$Taxable.Income,pred)

## accuracy
acc<-c(Fraud_check,sum(diag(table))/sum(table))
acc# 79%

######################## Boosting######################
install.packages("gbm")
library(gbm)
boost<-C5.0(train$Taxable.Income~.,data = train,trails=10)

# generate the model summary
summary(boost)
# predict for the test data set

pred<-predict.C5.0(boost,test)
table<-table(test$Taxable.Income,pred)

## accuracy
sum(diag(table))/sum(table)##79%
