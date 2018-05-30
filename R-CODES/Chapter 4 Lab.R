# Chapter 4 Lab: Logistic Regression, LDA, QDA, and KNN

# The Stock Market Data

library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
View(Smarket)

cor(Smarket) #Error in cor(Smarket) : 'x' must be numeric
cor(Smarket[,-9])

attach(Smarket)
plot(Volume)
dev.off()
######################################################
# Logistic Regression
######################################################
## The glm() function fits generalized linear models, 
## a class of models that includes logistic regression. 
## The syntax generalized of the glm() function is similar to 
## that of lm(), except that we must pass in linear model
## the argument family=binomial in order to tell R to run a logistic regression
## rather than some other type of generalized linear model.

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fit)
coef(glm.fit)

summary(glm.fit)$coef
summary(glm.fit)$coef[,4]

#######################################################################
## The predict() function can be used to predict the probability that the
## market will go up, given values of the predictors. 
## The type="response" option tells R to output probabilities of the form P(Y = 1|X), 
## as opposed to other information such as the logit.
#######################################################################

## Approach1: Train on the data, then test on the original data ##
glm.probs=predict(glm.fit,type="response")   
glm.probs[1:10]
contrasts(Direction)

glm.pred=rep("Down",1250);
glm.pred

glm.pred[glm.probs>.5]="Up"
glm.pred

table(glm.pred,Direction) ## to get the confusion matrix

## To get the prediction accuracy
## Method 1 ## 
(507+145)/1250; ## This is the accuracy of correct prediction

## Method 2 ##
## Or you can use the following codes #
TAB=table(glm.pred,Direction);
accuracy=sum(diag(TAB))/sum(TAB); accuracy

## Method 3 ##
## OR use ##
mean(glm.pred==Direction) 

#######################################################################
## Approach2: Train on the data with Year<2005, then test on the data with Year=2005 ##
## This is a more realistic approach ##
#######################################################################
table(Year) ## to display var of Year

## Simulation study to some commands for the rest of programming ##
x=1:3
y=c(FALSE, TRUE, FALSE)
x[y]
x[!y]
###### End of simulation ##########

train=(Year<2005); table(train)  ## create a new index variable
Smarket.2005=Smarket[!train,]; Smarket.2005

dim(Smarket.2005)
Direction.2005=Direction[!train]; Direction.2005

## Use the subset=train with 6 predictors ##
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")

glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005) ## confusion table

mean(glm.pred==Direction.2005) ## Prediction Accuracy= 0.4801587
mean(glm.pred!=Direction.2005) ## Prediction Error= 0.5198413


## Use the subset=train with 2 predictors ##
glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")

glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
##           Direction.2005
## glm.pred  Down  Up
## Down      35  35
## Up        76 106
mean(glm.pred==Direction.2005) ##overall accuracy=0.5595238

### Take a deeper look at the outputs ###
contrasts(factor(glm.pred))  ## "Up" for 1, "down" for 0.
sum(Direction.2005=="Up") ## 141
sum(Direction.2005=="Down") ## 111

#Sensitivity=Pr(Predicted="Up" | truth="Up") = True Positive        
106/(35+106) #0.751773

#Specificity=Pr(Predicted="Down" | truth="Down") = 1-False Positive   
35/(35+76)  #0.3153153

## Take another deeper look ##
106/(106+76)  #0.5824176 ==> Pr(truth="Up" | Predicted="Up")
## the confusion matrix shows that on days when logistic regression predicts
## an increase in the market, it has a 58% accuracy rate.
## This suggests a possible trading strategy of buying on days when the model predicts 
## an increasing market, and avoiding trades on days when a decrease is predicted.

predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")


##################################
# Linear Discriminant Analysis (in MASS library)
## lda(), qda() functions are identical to that of lm(), and glm() except family option
##################################
## To find the priors for training set ##
mean(Direction[train]=="Up")  #0.508016
mean(Direction[train]=="Down") #0.491984

library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit)
# Coefficients of linear discriminants (-0.6420190, -0.5135293)
# The plot() function produces plots of the linear discriminants, 
# obtained by computing -0.642 × Lag1 - 0.514 × Lag2 for each of the training observations.

###################################
names(lda.fit)
## To reproduce the plots from plot(lda.fit)
OUT=NULL;
Smarket.train=Smarket[train,]
for(i in 1:length(Direction[train]))
{
  S1=sum(lda.fit$scaling[,1]* (Smarket.train[i,2:3]))
  OUT=c(OUT, S1)
} 
OUT
par(mfrow=c(2,1))
hist(OUT[Smarket.train$Direction=="Down"], col="lightblue", nclass=16, main="Group Up")
hist(OUT[Smarket.train$Direction=="Up"], col="lightblue", nclass=18, main="Group Down")
###################################


lda.pred=predict(lda.fit, Smarket.2005) ##for only test set
names(lda.pred)
lda.class=lda.pred$class; lda.class ## for LDA prediction classes
lda.pred$posterior ## for LDA prediction posterior for classes
lda.pred$x # linear discriminant
#class: the Maximum a Posteriori Probability (MAP) classification (a factor)
#posterior: posterior probabilities for the classes
#x: the scores of test cases on up to discriminant variables


###################################
## To reproduce the X-value from prediction output
OUT=NULL;
for(i in 1:252)
{
  S1=sum(lda.fit$scaling[,1]* (Smarket.2005[i,2:3]))
  OUT=c(OUT, S1)
} 
OUT
cbind(OUT, lda.pred$x) ##minor difference due to roundings
###################################


# LDA and logistic regression predictions are almost identical #
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005) #0.5595238

## Note that the posterior prob output by the model corresponding
## to the prob that the market will decrease (against our intuition). 
sum(lda.pred$posterior[,1]>=.5) ## 70 for Down with 50% threshold 
sum(lda.pred$posterior[,1]<.5) ## 182 for Up

lda.pred$posterior[1:20,1] ##down with posterior prob>0.5##
lda.class[1:20]

sum(lda.pred$posterior[,1]>.9) ##Use 90% threshold for Down


##################################
# Quadratic Discriminant Analysis (in MASS library)
##################################
qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005) #0.5992063


##################################
# K-Nearest Neighbors in a single step
##################################
#install.packages("class")
library(class)
train.X=cbind(Lag1,Lag2)[train,] ##set up training_X set
test.X=cbind(Lag1,Lag2)[!train,] ##set up testing_X set
train.Direction=Direction[train] ##set up training_y 
test.Direction=Direction[!train]

#We set a random seed before we apply knn() because
#if several observations are tied as nearest neighbors, then R will randomly
#break the tie. Therefore, a seed must be set in order to ensure reproducibility
#of results.
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=3)
knn.pred

table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)
(83+43)/252


set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=2)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005) #0.531746

##################################
# An Application to Caravan Insurance Data
##################################
dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822

standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

test=1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]

train.Y=Purchase[-test]
test.Y=Purchase[test]

set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred,test.Y)
9/(68+9)

knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
5/26

knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
4/15

glm.fit=glm(Purchase~.,data=Caravan,family=binomial,subset=-test)
glm.probs=predict(glm.fit,Caravan[test,],type="response")
glm.pred=rep("No",1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)
glm.pred=rep("No",1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred,test.Y)
11/(22+11)


###############################################
## All R-codes used in the Chapter 4 class PPT
###############################################
library(MASS); library(ISLR)
data(OJ)
head(OJ)

library(MASS); library(ISLR)
data(Default)
head(Default)

library(MASS); library(ISLR)
data(Default)
head(Default)

attach(Default)
plot(balance[default=="Yes"], income[default=="Yes"], pch="+", col="darkorange")
points(balance[default=="No"], income[default=="No"], pch=21, col="lightblue")

par(mfrow=c(1,2))
plot(default, balance, col=c("lightblue", "red"), xlab="Default", ylab="Balance")
plot(default, income, col=c("lightblue", "red"), xlab="Default", ylab="Income")

library(MASS); library(ISLR)
attach(Default); head(Default)
# Logistic Regression
glm.fit=glm(default~balance,family=binomial)
summary(glm.fit)
summary(glm.fit)$coef

with(glm.fit, null.deviance - deviance)
with(glm.fit, df.null - df.residual)
with(glm.fit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(glm.fit)





