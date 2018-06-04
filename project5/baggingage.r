library(stringr)
library(tree)
library(randomForest)
library(gbm)

fn = "./clean_partial.csv"
morph2=read.csv(file=fn,header=T);


n=dim(morph2)[1]; m=dim(morph2)[2]; print(c(m,n))
set.seed(12345)
n_fold = 5;
rep(1:n_fold, length.out = n) ## repeat 1:n_fold until we can the full size of n
folds <- sample(rep(1:n_fold, length.out = n)) ##without replacement
#five fold.



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

write.csv(OUTBag,file="baggingage.csv")
