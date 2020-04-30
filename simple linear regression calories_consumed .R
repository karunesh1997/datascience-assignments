View(calories_consumed)
mydata<-calories_consumed
attach(mydata)

mean(mydata$Weight.gained..grams.)
mean(mydata$Calories.Consumed)

plot(mydata)
hist(mydata$Weight.gained..grams.)
hist(mydata$Calories.Consumed)

boxplot(mydata)

#define variables
y<- cbind(Weight.gained..grams.)
x<- cbind(Calories.Consumed)


#correlation among variables
cor(y,x)

#plotting data on scatter digram
plot(mydata$Weight.gained..grams.,mydata$Calories.Consumed)
# here we can see some positive relation between these variables

#simple linear regression
simp<-lm(Calories.Consumed~.,data = mydata)
summary(simp)
confint(simp,level = 0.95)


#plotting the regression line
abline(simp)


#predected values for dependent variables
y1hat<-fitted(simp)
summary(y1hat)
plot(y1hat~Calories.Consumed)


#regression residual
e1hat<-resid(simp)
summary(e1hat)
plot(e1hat~Calories.Consumed)


#ggplot adding regression line for data
library(ggplot2)
ggplot(data = calories_consumed,aes(x=Calories.Consumed,y=Weight.gained..grams.))+geom_point(colour='red')+geom_line(colour='blue',data = calories_consumed,aes(x=Calories.Consumed,y=Weight.gained..grams.))

####prediction####
res<- predict(simp,list(Weight.gained..grams.=c(200,150,200,400,150,180)))
res

