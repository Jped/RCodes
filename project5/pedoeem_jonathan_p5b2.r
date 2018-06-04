library(MASS)
library(stringr)
library(tree)
library(randomForest)
library(gbm)
setwd("/pylon5/ac5fphp/jped/project5/")
#fn ="./MorphII_BIF_s7-37_g0.1_max_partial.csv";
# fn <- file.choose();
fn = "./clean_partial.csv"
morph2=read.csv(file=fn,header=T);

morph2$gender = factor(as.numeric(morph2$gender))
morph2$age = as.numeric(morph2$age)
n=dim(morph2)[1]; m=dim(morph2)[2]; print(c(m,n))
set.seed(12345)
n_fold = 5;
## repeat 1:n_fold until we can the full size of n
folds <- sample(rep(1:n_fold, length.out = n)) ##without replacement



LOOOUT2=matrix(0,nrow=1,ncol=1000);
for (k in 1:1000) 
{
  print(k);
  test.ID <- k
  train_set <- morph2[-test.ID, ]
  test_set <- morph2[test.ID, ]
  #Bagging
  bag.gender=randomForest(train_set$gender~.,data=train_set[,1:150], mtry=150, ntree=500, importance=TRUE)##mtry=p
  bag.pred = predict(bag.gender,test_set)
  LOOOUT2[1,k]= mean(bag.pred==test_set$gender)
}
write.csv(LOOOUT2,file="bagging.csv")
