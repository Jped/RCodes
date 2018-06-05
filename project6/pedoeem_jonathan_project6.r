## Jonathan Pedoeem
## Project 6
## UNCW REU 2018
setwd("C:/Users/pedoe/Documents/code/REU/project6")
fn <- "./clean_partial.csv";
morph2=read.csv(file=fn,header=T); 
morph2= morph2[,-1]



morph2PCA = prcomp(morph2[,1:2568], retx=TRUE,center=T, scale=FALSE)


morph2pc  = as.matrix(morph2[,1:2568])%*%morph2PCA$rotation


cumsum(morph2PCA$sdev^2)/sum(morph2PCA$sdev^2)


#pick out the first 150 dimension.
morph2pcsub = morph2pc[,1:150]
library(MASS)
n=dim(morph2)[1]; m=dim(morph2)[2]; print(c(m,n))
set.seed(12345)
n_fold = 5;
## repeat 1:n_fold until we can the full size of n
folds <- sample(rep(1:n_fold, length.out = n))
OUT = NULL
for (k in 1:n_fold) 
{
  test.ID   = which(folds==k)
  train_set = morph2pcsub[-test.ID,]
  test_set  = morph2pcsub[test.ID,]
  train.y   = morph2$gender[-test.ID]
  test.y    = morph2$gender[test.ID]
  lda.fit=lda(train.y~., as.data.frame(train_set))
  lda.pred=predict(lda.fit,as.data.frame(test_set))
  lda.class=lda.pred$class
  table(lda.class,test.y)
  #overall prediction accuracy
  OUT = c(OUT, mean(lda.class==test.y)) 
}


#random forest for Gender
library(randomForest)
OUT2 = NULL
for (k in 1:n_fold) 
{
  test.ID   = which(folds==k)
  train_set = morph2pcsub[-test.ID,]
  test_set  = morph2pcsub[test.ID,]
  train.y   = morph2$gender[-test.ID]
  test.y    = morph2$gender[test.ID]
  RF.fit=randomForest(as.factor(train.y)~.,data=train_set, mtry=12,importance=TRUE, ntree=500) ##mtry=p^0.5
  RF.pred   = predict(RF.fit,test_set)
  OUT2= c(OUT2, mean(RF.pred==test.y))
  
}




#linear regression and K-NN for age. 

OUT3 = NULL
for (k in 1:n_fold) 
{
  test.ID   = which(folds==k)
  train_set = morph2pcsub[-test.ID,]
  test_set  = morph2pcsub[test.ID,]
  train.y   = morph2$age[-test.ID]
  test.y    = morph2$age[test.ID]
  lm.fit = lm(train.y~.,data=as.data.frame(train_set))
  summary(lm.fit)
  lm.pred=predict(lm.fit,as.data.frame(test_set))
  OUT3= c(OUT3, mean(abs(lm.pred-test.y)))
}
mean(OUT3)

#non-pca Linear regression.
OUT3NP = NULL
for (k in 1:n_fold) 
{
  test.ID   = which(folds==k)
  train_set = morph2[-test.ID,1:150]
  test_set  = morph2[test.ID,1:150]
  train.y   = morph2$age[-test.ID]
  test.y    = morph2$age[test.ID]
  lm.fit = lm(train.y~.,data=as.data.frame(train_set))
  summary(lm.fit)
  lm.pred=predict(lm.fit,as.data.frame(test_set))
  OUT3NP= c(OUT3NP, mean(abs(lm.pred-test.y)))
}
mean(OUT3NP)


#KNN
library(class)
OUT4 = NULL
for (k in 1:n_fold) 
{
  test.ID   = which(folds==k)
  train_set = morph2pcsub[-test.ID,]
  test_set  = morph2pcsub[test.ID,]
  train.y   = morph2$age[-test.ID]
  test.y    = morph2$age[test.ID]
  knn.pred=knn(train_set,test_set,train.y,k=3)
  OUT4= c(OUT4, mean(abs(as.integer(knn.pred)-test.y)))
}
mean(OUT4)

OUT4NP = NULL
for (k in 1:n_fold) 
{
  test.ID   = which(folds==k)
  train_set = morph2[-test.ID,1:150]
  test_set  = morph2[test.ID,1:150]
  train.y   = morph2$age[-test.ID]
  test.y    = morph2$age[test.ID]
  lm.fit = lm(train.y~.,data=as.data.frame(train_set))
  knn.pred=knn(train_set,test_set,train.y,k=3)
  OUT4NP= c(OUT4NP, mean(abs(as.integer(knn.pred)-test.y)))
}
mean(OUT4NP)
