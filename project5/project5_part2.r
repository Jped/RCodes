## Jonathan Pedoeem
## Project 5 part 2
## UNCW REU 2018

#Use Dataset from project 3 for age estimation
#using: Deicison Tree,Bagging,randomForest, Boosting


library(stringr)
library(tree)
library(randomForest)
library(gbm)
fn <- file.choose();
morph2=read.csv(file=fn,header=T);


n=dim(morph2)[1]; m=dim(morph2)[2]; print(c(m,n))
set.seed(12345)
n_fold = 5;
rep(1:n_fold, length.out = n) ## repeat 1:n_fold until we can the full size of n
folds <- sample(rep(1:n_fold, length.out = n)) ##without replacement
#five fold.
OUT = NULL
for (k in 1:n_fold) 
{
  test.ID <- which(folds == k)
  train_set <- morph2[-test.ID, ]
  test_set <- morph2[test.ID, ]
  ## Decision Tree
  tree.age=tree(train_set$age~.,train_set[,1:150])
  tree.pred = predict(tree.age, test_set)
  plot(tree.age)
  text(tree.age,pretty=0)
  OUT= c(OUT, mean((tree.pred-test_set$age)^2))
}

print(OUT)


##Bagging
OUTBag = NULL
for (k in 1:n_fold) 
{
  test.ID <- which(folds == k)
  train_set <- morph2[-test.ID, ]
  test_set <- morph2[test.ID, ]
  bag.gender=randomForest(train_set$age~.,data=train_set[,1:150], mtry=150, ntree=500, importance=TRUE)##mtry=p
  bag.pred = predict(bag.gender,test_set)
  OUTBag = c(OUTBag, mean((bag.pred-test_set$age)^2))
}

print(OUTBag)

#Random Forest. m = 12
RF.fit    =randomForest(morph2$age~.,data=morph2[,1:150], mtry=150,importance=TRUE, ntree=500) ##mtry=p^0.5
RF.pred   = predict(RF.fit)
mean((RF.pred-morph2$age)^2)


##Boosting ???? 


LOOCVTree = read.csv(file="C:/Users/pedoe/Documents/code/REU/project5/genderclass/tree.csv",header=T)
mean(as.numeric(LOOCVTree[1,]))
sd(as.numeric(LOOCVTree[1,]))

LOOCVBagging= read.csv(file="C:/Users/pedoe/Documents/code/REU/project5/genderclass/bagging.csv",header=T)
mean(as.numeric(LOOCVBagging[1,]))
sd(as.numeric(LOOCVBagging[1,]))

LOOCVBost= read.csv(file="C:/Users/pedoe/Documents/code/REU/project5/genderclass/bagging.csv",header=T)
mean(as.numeric(LOOCVBost[1,]))
sd(as.numeric(LOOCVBost[1,]))