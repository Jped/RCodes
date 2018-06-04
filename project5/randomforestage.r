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

OUTFor = NULL
for (k in 1:n_fold) 
{
  test.ID <- which(folds == k)
  train_set <- morph2[-test.ID, ]
  test_set <- morph2[test.ID, ]
  #Random Forest. m = 12
  RF.fit    =randomForest(train_set$age~.,data=train_set[,1:150], mtry=50,importance=TRUE, ntree=500) ##mtry=p^0.5
  RF.pred   = predict(RF.fit, test_set)
  OUTFor    = c(OUTFor, mean(abs(RF.pred-test_set$age)))
}
print(OUTFor)
print(mean(OUTFor))
print(sd(OUTFor))
write.csv(OUTFor,file="randomforestAge.csv")
