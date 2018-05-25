## Jonathan Pedoeem
## Project 3
## UNCW REU 2018

#Load in data

fn <- file.choose();
library("stringr")
morph2=read.csv(file=fn,header=F); 
grabLetter <- function(x){
  gender<- str_extract(x, "[M,F]")
  if(gender == "M"){
    return(1);
  }
  else if (gender == "F"){
    return(0);
  } 
}
grabAge <- function(x){
  start <- str_locate(x, "[M,F]")[1];
  end <- str_locate(x, ".JPG")[1];
  return(as.numeric(substr(x,start+1, end-1)));
}
morph2$gender <- lapply(morph2[,1],grabLetter);
morph2$age    <- lapply(morph2[,1],grabAge);

morph2= morph2[,-1]
morph2$gender = factor(morph2$gender)
morph2$age    = as.integer(morph2$age)
##part 1: Regression
##Linear Regression
lm.fit = lm(as.integer(morph2$age)~.,data=morph2[,1:20])
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)

#most significant is V3
lm.fit = lm(as.integer(morph2$age)~V3,data=morph2)
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
plot(as.integer(morph2$age)~morph2$V3, main="Using V3 to Predict Age", ylab="Age", xlab="V3 Val")
abline(lm.fit,col="red")

##Quadratic Regression
lm.quadfit = lm(as.integer(morph2$age)~poly(V3,2),data=morph2)
summary(lm.quadfit)
names(lm.quadfit)
coef(lm.quadfit)
confint(lm.quadfit)
xx = seq(0,255,length=100)
lines(xx, predict(lm.quadfit,data.frame(V3=xx)),col="blue")
##Polynomial Regression (degree 3)
lm.cubefit = lm(as.integer(morph2$age)~poly(V3,3),data=morph2)
summary(lm.cubefit)
names(lm.cubefit)
coef(lm.cubefit)
confint(lm.cubefit)
lines(xx, predict(lm.cubefit,data.frame(V3=xx)),col="green")

##Part 2: Classification
#split up into test and train
train = morph2[1:800,]
test  = morph2[801:1000,]
#also take the time for each of the different methods.


##Logistic Regression
#Train on all of the data.
logistic.fit = glm(morph2$gender~., data=morph2[,1:175], family=binomial)
summary(logistic.fit)
logisitc.prob = predict(logistic.fit, type="response")
logistic.pred   = rep(0,1000)
logistic.pred[logisitc.prob>0.5]=1
#confusion matrix
table(logistic.pred,morph2$gender)
#the overall prediction accuracy
mean(logistic.pred==morph2$gender)
#sensitivity
807/(36+807)
#specificity 
117/(117+40)
#Now do the same with the 80/20 train test split.
logistic.fit2 = glm(train$gender~., data=train[,1:125], family=binomial)
summary(logistic.fit2)
logisitc.prob2 = predict(logistic.fit2,test, type="response")
logistic.pred2   = rep(0,200)
logistic.pred2[logisitc.prob2>0.5]=1
#confusion matrix
table(logistic.pred2,test$gender)
#the overall prediction accuracy
mean(logistic.pred==test$gender)
#sensitivity
142/(142+29)
#specificity 
20/29


##LDA

lda.fit=lda(morph2$gender~., data=morph2[,1:175])
lda.pred=predict(lda.fit)
lda.class=lda.pred$class
table(lda.class,morph2$gender)
#overall prediction accuracy
mean(lda.class==morph2$gender)
#sensitivity
821/(821+22)
#speceificity
108/(108+49)

#now with the 80 20 split.
lda.fit2=lda(train$gender~., data=train[,1:175])
lda.pred2=predict(lda.fit2,test)
lda.class2=lda.pred2$class
table(lda.class2,test$gender)
#overall prediction accuracy
mean(lda.class2==test$gender)
#sensitivity
145/(145+26)
#speceficity
9/29

##QDA

qda.fit=qda(morph2$gender~., data=morph2[,1:150])
qda.pred=predict(qda.fit)
qda.class=qda.pred$class
table(qda.class,morph2$gender)
#overall prediction accuracy
mean(qda.class==morph2$gender)
#sensitivity 1
#speceficity 1

#now for the 80 20 split
qda.fit2=qda(train$gender~., data=train[,1:100])
qda.pred2=predict(qda.fit2,test)
qda.class2=qda.pred2$class
table(qda.class2,test$gender)
#overall prediction accuracy
mean(qda.class2==test$gender)
#sensitivity
1
#speceficitiy
0

##KNN
library(class)


set.seed(1)
knn.pred=knn(train[,1:100],test[,1:100],train$gender,k=3)


table(knn.pred,test$gender)
mean(knn.pred==test$gender)
#sensitivity
159/(159+12)
#speceficity
19/29
