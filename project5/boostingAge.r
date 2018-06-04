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
  ## Boosting
  boost.age=gbm(train_set$age~.,data=train_set[,1:150],distribution="gaussian",n.trees=5000,interaction.depth=4)
  boost.pred= predict(boost.age, newdata=test_set, n.trees=5000)
  OUT= c(OUT, mean((boost.pred-test_set$age)^2))
}

write.csv(OUT,file="boostingAge.csv")