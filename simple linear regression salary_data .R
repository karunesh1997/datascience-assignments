View(Salary_Data)
mydata<- Salary_Data
attach(mydata)

plot(mydata)
mean(mydata$Salary)
str(mydata$Salary)
hist(mydata$Salary)
boxplot(mydata$Salary)


#define variables
y<-YearsExperience
x<-Salary

#correlation among variables
cor(y,x)

#plotting data on a scatter digram
plot(mydata$YearsExperience,mydata$Salary)
#we can see the slightly positive relation between these variables

#simple linear regression
simp<-lm(Salary~.,data = mydata)
summary(simp)
confint(simp,level = 0.95)
anova(simp)


#plotting the regression line
abline(simp)

#predicted values for dependent variables
y1hat<-fitted(simp)
summary(y1hat)
plot(y1hat~Salary)

#regression residual
e1hat<-resid(simp)
summary(e1hat)
plot(e1hat~Salary)


#ggplot adding regression linefor data
library(ggplot2)
ggplot(data = Salary_Data,aes(x=Salary,y=YearsExperience))+geom_point(colour='blue')+geom_line(colour='red',data = Salary_Data,aes(x=Salary,y=YearsExperience))
