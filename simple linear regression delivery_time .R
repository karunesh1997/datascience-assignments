View(delivery_time)
data<- delivery_time

attach(data)
mean(data$Delivery.Time)
mean(data$Sorting.Time)
str(data)
plot(data)

hist(data$Delivery.Time)
hist(data$Sorting.Time)
boxplot(data)


#define variable
y<-cbind(delivery_time)
x<-cbind(Sorting.Time)

#correlation among variables
cor(y,x)

#plotting data on scatter digram
plot(data$Delivery.Time,data$Sorting.Time)
#we can see here a slightly positive linear regression

#simple linear regression
simpre<-lm(Sorting.Time~.,data = data)
summary(simpre)

confint(simpre,level = 0.95)
anova(simpre)

#plotting the regression line
abline(simpre)

#predicted values for dependent variables
y1hat<-fitted(simpre)
summary(y1hat)
plot(y1hat~Sorting.Time)

#regression residuals
e1hat<-resid(simpre)
summary(e1hat)
plot(e1hat~Sorting.Time)


#ggplot for adding regression line for data
library(ggplot2)
ggplot(data=delivery_time,aes(x=Delivery.Time,y=Sorting.Time))+geom_point(colour='blue')+
  geom_line(colour='red',data = delivery_time,aes(x=Delivery.Time,y=Sorting.Time))

predict(simpre,list(Sorting.Time=6))
