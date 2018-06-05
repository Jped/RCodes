## Jonathan Pedoeem
## Project 7
## UNCW REU 2018
setwd("C:/Users/pedoe/Documents/code/REU/project7")
fn <- "./clean_partial.csv";

morph2=read.csv(file=fn,header=T); 
morph2= morph2[,-1]
n=dim(morph2)[1]; m=dim(morph2)[2]; print(c(m,n))
set.seed(12345)
n_fold = 5;
folds <- sample(rep(1:n_fold, length.out = n))

#SVM 
library(e1071)
#5-FOLD CROSS VALIDATION ON GENDER CLASSIFICATION USING LINEAR

# morph2sub = morph2[,1:150]
# test.ID   = which(folds==1)
# train_set = morph2sub[-test.ID,]
# test_set  = morph2sub[test.ID,]
# train.y   = morph2sub$gender[-test.ID]
# test.y    = morph2sub$gender[test.ID]
# svm.gender=svm(as.factor(train.y)~.,train_set, kernel="linear", cost=c, scale=FALSE)
# yhat=predict(svm.gender, newdata=test_set)
# print(mean(yhat == train.y))

test.ID   = which(folds==1)
train_set = morph2[-test.ID,1:150]
test_set  = morph2[test.ID,1:150]
train.y   = morph2$gender[-test.ID]
test.y    = morph2$gender[test.ID]
tune.costs <- function(c){
  svm.gender=svm(as.factor(train.y)~.,train_set, kernel="linear", cost=c, scale=FALSE)
  yhat=predict(svm.gender, newdata=test_set)
  return(mean(yhat == train.y))
}


results  = lapply(c(0.001, 0.01, 0.1, 1,5,10,100), tune.costs)

## Top Cost is 0.001

OUTsvm = NULL
for (k in 1:n_fold) 
{
  test.ID   = which(folds==k)
  train_set = morph2[-test.ID,1:150]
  test_set  = morph2[test.ID,1:150]
  train.y   = morph2$gender[-test.ID]
  test.y    = morph2$gender[test.ID]
  svm.gender=svm(as.factor(train.y)~.,train_set, kernel="linear", cost=.001, scale=FALSE)
  yhat=predict(svm.gender, newdata=test_set)
  OUTsvm = c(OUTsvm, mean(yhat == train.y))  
}
mean(OUTsvm)

#Now Run with PCA....
morph2PCA = prcomp(morph2[,1:2568], retx=TRUE,center=T, scale=FALSE)
morph2pc  = as.matrix(morph2[,1:2568])%*%morph2PCA$rotation
#pick out the first 150 dimension.
morph2pcsub = morph2pc[,1:150]


#tune

test.ID2   = which(folds==1)
train_set2 = morph2pcsub[-test.ID2,]
test_set2  = morph2pcsub[test.ID2,]
train.y2   = morph2$gender[-test.ID2]
test.y2    = morph2$gender[test.ID2]
tune.costsPCA <- function(c){
  print(c)
  svm.gender2=svm(as.factor(train.y2)~.,train_set2, kernel="linear", cost=c, scale=FALSE)
  yhat=predict(svm.gender2, newdata=test_set2)
  return(mean(yhat == train.y2))
}

PCAresults = lapply(c(0.001, 0.01, 0.1, 1,5,10,100),tune.costsPCA)

#all have the same accuracy...

#run five folds
OUTsvmPCA = NULL
for (k in 1:n_fold) 
{
  test.ID2   = which(folds==1)
  train_set2 = morph2pcsub[-test.ID2,]
  test_set2  = morph2pcsub[test.ID2,]
  train.y2   = morph2$gender[-test.ID2]
  test.y2    = morph2$gender[test.ID2]
  svm.gender2=svm(as.factor(train.y2)~.,train_set2, kernel="linear", cost=1, scale=FALSE)
  yhat=predict(svm.gender2, newdata=test_set2)
  OUTsvmPCA = c(OUTsvmPCA, mean(yhat == train.y2))  
}
mean(OUTsvmPCA)




#Now Run With different KERNEL

#Radial
gammas = c(2^-4, 2^-2, 2, 4, 8, 16, 32)
costs  = c(0.001, 0.01, 0.1, 1,5,10,100)
tune.costs.radial <- function(g){
  max_c = 0;
  max_val=0;
  for (c in costs){
    svm.gender=svm(as.factor(train.y)~.,train_set, kernel="radial", gamma=g, cost=c, scale=FALSE)
    yhat=predict(svm.gender, newdata=test_set)
    val = (mean(yhat == train.y))
    if (val>max_val){
      max_val = val;
      max_c   = c;
      }
  }
  print(g)
  print(max_c)
  return(max_val)
}
radialResults = lapply(gammas,tune.costs.radial)

# now do 5 fold cv.
OUTsvmRadial = NULL
for (k in 1:n_fold) 
{
  test.ID   = which(folds==k)
  train_set = morph2[-test.ID,1:150]
  test_set  = morph2[test.ID,1:150]
  train.y   = morph2$gender[-test.ID]
  test.y    = morph2$gender[test.ID]
  svm.gender=svm(as.factor(train.y)~.,train_set, kernel="radial", gamma=0.24, cost=1, scale=FALSE)
  yhat=predict(svm.gender, newdata=test_set)
  OUTsvmRadial = c(OUTsvmRadial, mean(yhat == train.y))  
}
mean(OUTsvmRadial)


#now try with PCA.
test.ID2   = which(folds==1)
train_set2 = morph2pcsub[-test.ID2,]
test_set2  = morph2pcsub[test.ID2,]
train.y2   = morph2$gender[-test.ID2]
test.y2    = morph2$gender[test.ID2]
tune.costs.radial.PCA <- function(g){
  max_c = 0;
  max_val=0;
  for (c in costs){
    svm.gender=svm(as.factor(train.y2)~.,train_set2, kernel="radial", gamma=g, cost=c, scale=FALSE)
    yhat=predict(svm.gender, newdata=test_set2)
    val = (mean(yhat == train.y2))
    if (val>max_val){
      max_val = val;
      max_c   = c;
    }
  }
  print(g)
  print(max_c)
  return(max_val)
}
PCAradialResults = lapply(gammas,tune.costs.radial.PCA)

#all the same and same result as normal radial...


OUTsvmRadialpca = NULL
for (k in 1:n_fold) 
{
  test.ID2   = which(folds==1)
  train_set2 = morph2pcsub[-test.ID2,]
  test_set2  = morph2pcsub[test.ID2,]
  train.y2   = morph2$gender[-test.ID2]
  test.y2    = morph2$gender[test.ID2]
  svm.gender=svm(as.factor(train.y2)~.,train_set2, kernel="radial", gamma=2, cost=1, scale=FALSE)
  yhat=predict(svm.gender, newdata=test_set2)
  OUTsvmRadialpca = c(OUTsvmRadialpca, mean(yhat == train.y))  
}
mean(OUTsvmRadialpca)

totalResults = cbind(OUTsvm, OUTsvmPCA, OUTsvmRadial, OUTsvmRadialpca)
colnames(totalResults) = c("Linear SVM", "Linear SVM w/PCA", "Radial SVM", "Radial SVM w/PCA")

boxplot(totalResults, main="Comparison of SVM Methods on Classifying Gender", ylab="Accuracy", xlab="Method", col=c("blue","blue", "orange","orange"))
