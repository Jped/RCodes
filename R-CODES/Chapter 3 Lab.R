# Chapter 3 Lab: Linear Regression

library(MASS)
install.packages("ISLR") ## Only need to install once for your own computer
library(ISLR)

# Simple Linear Regression

fix(Boston)
names(Boston)

lm.fit=lm(medv~lstat) ##Error in eval(expr, envir, enclos) : object 'medv' not found
lm.fit=lm(medv~lstat,data=Boston)
lm.fit=lm(Boston$medv~Boston$lstat)

attach(Boston)
lm.fit=lm(medv~lstat)
lm.fit

summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)

predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="prediction")

plot(lstat,medv)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")

plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)

par(mfrow=c(2,2))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# Multiple Linear Regression

lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)
lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)
summary(lm.fit)$r.sq
summary(lm.fit)$sigma


install.packages("car")
library(car)
vif(lm.fit) #variance inflation factors

lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)
lm.fit1=update(lm.fit, ~.-age)


# Interaction Terms

summary(lm(medv~lstat*age,data=Boston))

# Non-linear Transformations of the Predictors

lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)
lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)
summary(lm(medv~log(rm),data=Boston))

# Qualitative Predictors

fix(Carseats)
names(Carseats)
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)

attach(Carseats)
contrasts(ShelveLoc)

# Writing Functions

LoadLibraries #Error: object 'LoadLibraries' not found
LoadLibraries() #Error: could not find function "LoadLibraries"

LoadLibraries=function(){
 library(ISLR)
 library(MASS)
 print("The libraries have been loaded.")
 }
LoadLibraries
LoadLibraries()


###############################################
## All R-codes used in the Chapter 3 class PPT
###############################################

Advertising=read.csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv", header=TRUE); 
newdata=Advertising[,-1]
fix(newdata)
names(newdata)
pairs(newdata)


#################
## to get Table 3.1
lm.fit=lm(Sales~TV,data=Advertising) 
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit) 


#################
## To get Table 3.4 ##
lm.fit=lm(Sales~TV+Radio+Newspaper,data=Advertising)
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)


#################
## To get Table 3.4 ##
lm.fit1=lm(Sales~Newspaper,data=Advertising)
summary(lm.fit1)

lm.fit2=lm(Sales~Newspaper+TV,data=Advertising)
summary(lm.fit2)

lm.fit3=lm(Sales~Newspaper+TV+Radio,data=Advertising)
summary(lm.fit3)

lm.fit4=lm(Sales~TV+Radio,data=Advertising)
summary(lm.fit4)


#################
par(mfrow=c(2,2))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))


#################
Credit=read.csv("http://www-bcf.usc.edu/~gareth/ISL/Credit.csv", header=TRUE); 
head(Credit); newdata=Credit [,-1]
fix(newdata); names(newdata)
pairs(newdata[,c(1, 2, 4, 5, 6, 7)])

lm.fit=lm(Balance~Gender,data=Credit)
summary(lm.fit); contrasts(Credit$Gender)

lm.fit=lm(Balance~Ethnicity,data=Credit)
summary(lm.fit)

Advertising=read.csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv", header=TRUE); 
lm.fit=lm(Sales~TV*Radio,data=Advertising)
summary(lm.fit)

Credit=read.csv("http://www-bcf.usc.edu/~gareth/ISL/Credit.csv", header=TRUE); 
head(Credit); newdata=Credit [,-1]
fix(newdata); names(newdata)
pairs(newdata[,c(1, 2, 4, 5, 6, 7)])

Credit=read.csv("http://www-bcf.usc.edu/~gareth/ISL/Credit.csv", header=TRUE); 
lm.fit=lm(Balance~Gender,data=Credit)
summary(lm.fit); contrasts(Credit$Gender)

Credit=read.csv("http://www-bcf.usc.edu/~gareth/ISL/Credit.csv", header=TRUE); 
lm.fit=lm(Balance~Ethnicity,data=Credit)
summary(lm.fit)

Advertising=read.csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv", header=TRUE); 
lm.fit=lm(Sales~TV*Radio,data=Advertising)
summary(lm.fit)


