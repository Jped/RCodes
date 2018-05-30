## Jonathan Pedoeem
## Project 4
## UNCW REU 2018
require(caret)
library(MASS)
require(stringr)
fn <- file.choose();
morph2=read.csv(file=fn,header=F); 
grabLetter <- function(x){
  gender<- str_extract(x, "[M,F]")
  if (is.na(gender)){
    
  }else if(gender == "M"){
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
morph2$gender = factor(as.numeric(morph2$gender))
# Step 1 Apply 5-fold cross validation for all four classifiers of Logistic Regression, 
#LDA, QDA, and K-NN. Find the overall prediction accuracy for each classifier
n_fold = 5;
set.seed(12345)
folds <- createFolds(morph2$gender, k = n_fold, list = TRUE, returnTrain = FALSE)


OUT=matrix(0,nrow=4,ncol=n_fold);
TRUTH = NULL; OUTPUT=NULL;
for (k in 1:n_fold) 
{
  test.ID <- folds[[k]]
  train_set <- morph2[-test.ID, ]
  test_set <- morph2[test.ID, ]
  #Logistic Regression
  logistic.fit2 = glm(train_set$gender~., data=train_set[1:150], family=binomial)
  logisitc.prob2 = predict(logistic.fit2,test_set, type="response")
  logistic.pred2   = rep(0,length(test_set$gender))
  logistic.pred2[logisitc.prob2>0.5]=1
  table(logistic.pred2,test_set$gender)
  # #the overall prediction accuracy
  OUT[1,k] = mean(logistic.pred2==test_set$gender)
  #LDA
  lda.fit=lda(train_set$gender~., data=train_set[,1:175])
  lda.pred=predict(lda.fit,test_set)
  lda.class=lda.pred$class
  table(lda.class,test_set$gender)
  #overall prediction accuracy
  OUT[2,k] = mean(lda.class==test_set$gender)
  #QDA
  qda.fit=qda(train_set$gender~., data=train_set[,1:120])
  qda.pred=predict(qda.fit,test_set)
  qda.class=qda.pred$class
  table(qda.class,test_set$gender)
  #overall prediction accuracy
  OUT[3,k] = mean(qda.class==test_set$gender)
}
print(OUT)
mean(OUT[1,])
sd(OUT[1,])
mean(OUT[2,])
sd(OUT[2,])
mean(OUT[3,])
sd(OUT[3,])






#K-NN
library(class)
set.seed(1)
#Optimize for K too.
OUTKNN=matrix(0,nrow=7,ncol=n_fold);
for(kp in 1:7){
  for (k in 1:n_fold) 
  {
    test.ID <- folds[[k]]
    train_set <- morph2[-test.ID, ]
    test_set <- morph2[test.ID, ]
    knn.pred=knn(train_set[,1:100],test_set[,1:100],train_set$gender,k=kp)
    OUTKNN[kp,k] = mean(knn.pred==test_set$gender)
  }
}
mean(OUTKNN[1,])
sd(OUTKNN[1,])
mean(OUTKNN[2,])
sd(OUTKNN[2,])
mean(OUTKNN[3,])
sd(OUTKNN[3,])
mean(OUTKNN[4,])
sd(OUTKNN[4,])
mean(OUTKNN[5,])
sd(OUTKNN[5,])
mean(OUTKNN[6,])
sd(OUTKNN[6,])
mean(OUTKNN[7,])
sd(OUTKNN[7,])
#K=3 is the best for KNN.
OUT[4,] = OUTKNN[3,]
rownames(OUT) = c("Logistic Regression", "LDA", "QDA", "KNN")
boxplot(t(OUT),col="orange", main="5-Fold Validation Comparison", ylab="Accuracy")

# legend("bottomleft", c("Logistic Regression", "LDA", "QDA", "KNN"),)
#Now do the same thing for Leave One Out.
LOOOUT=matrix(0,nrow=4,ncol=n_fold);
for (k in 1:1000) 
{
  test.ID <- folds[[k]]
  train_set <- morph2[-test.ID, ]
  test_set <- morph2[test.ID, ]
  #Logistic Regression
  logistic.fit2 = glm(train_set$gender~., data=train_set[1:150], family=binomial)
  logisitc.prob2 = predict(logistic.fit2,test_set, type="response")
  logistic.pred2   = rep(0,length(test_set$gender))
  logistic.pred2[logisitc.prob2>0.5]=1
  table(logistic.pred2,test_set$gender)
  #the overall prediction accuracy
  LOOOUT[1,k] = mean(logistic.pred2==test_set$gender)
  #LDA
  lda.fit=lda(train_set$gender~., data=train_set[,1:175])
  lda.pred=predict(lda.fit,test_set)
  lda.class=lda.pred$class
  table(lda.class,test_set$gender)
  #overall prediction accuracy
  LOOOUT[2,k] = mean(lda.class==test_set$gender)
  #QDA
  qda.fit=qda(train_set$gender~., data=train_set[,1:120])
  qda.pred=predict(qda.fit,test_set)
  qda.class=qda.pred$class
  table(qda.class,test_set$gender)
  #overall prediction accuracy
  LOOOUT[3,k] = mean(qda.class==test_set$gender)
}


LOOOUTKNN=matrix(0,nrow=7,ncol=n_fold);
for(kp in 1:7){
  for (k in 1:n_fold) 
  {
    test.ID <- folds[[k]]
    train_set <- morph2[-test.ID, ]
    test_set <- morph2[test.ID, ]
    knn.pred=knn(train_set[,1:100],test_set[,1:100],train_set$gender,k=kp)
    LOOOUTKNN[kp,k] = mean(knn.pred==test_set$gender)
  }
}
#K=3 is the best.

LOOOUT[4,] = LOOOUTKNN[3,]
rownames(LOOOUT) = c("Logistic Regression", "LDA", "QDA", "KNN")
boxplot(t(LOOOUT),col="orange", ylab="Accuracy", main="Leave One Out Cross Validation")

mean(LOOOUT[1,])
sd(LOOOUT[1,])
mean(LOOOUT[2,])
sd(LOOOUT[2,])
mean(LOOOUT[3,])
sd(LOOOUT[3,])
mean(LOOOUT[4,])
sd(LOOOUT[4,])