## Jonathan Pedoeem
## Project 5
## UNCW REU 2018
library(MASS)
library(stringr)
library(tree)
library(randomForest)
library(gbm)

fn <- file.choose();
morph2=read.csv(file=fn,header=T);

# morph2=read.csv(file=fn,header=F); 
# grabLetter <- function(x){
#   gender<- str_extract(x, "[M,F]")
#   if (is.na(gender)){
#     
#   }else if(gender == "M"){
#     return(1);
#   }
#   else if (gender == "F"){
#     return(0);
#   } 
# }
# grabAge <- function(x){
#   start <- str_locate(x, "[M,F]")[1];
#   end <- str_locate(x, ".JPG")[1];
#   return(as.numeric(substr(x,start+1, end-1)));
# }
# morph2$gender <- lapply(morph2[,1],grabLetter);
# morph2$age    <- lapply(morph2[,1],grabAge);
# 

morph2= morph2[,-1]
morph2$gender = factor(as.numeric(morph2$gender))
morph2$age = as.numeric(morph2$age)
n=dim(morph2)[1]; m=dim(morph2)[2]; print(c(m,n))
set.seed(12345)
n_fold = 5;
rep(1:n_fold, length.out = n) ## repeat 1:n_fold until we can the full size of n
folds <- sample(rep(1:n_fold, length.out = n)) ##without replacement
#five fold.
set.seed(12345)
OUT2=matrix(0,nrow=4,ncol=n_fold);
for (k in 1:n_fold) 
{
  test.ID <- which(folds == k)
  train_set <- morph2[-test.ID, ]
  test_set <- morph2[test.ID, ]
  #Decision Tree to classify Gender
  tree.gender=tree(train_set$gender~.,data=train_set[,1:150])
  tree.pred  =predict(tree.gender,test_set,type="class")
  #table(tree.pred,test_set$gender)
  OUT2[1,k]=mean(tree.pred==test_set$gender)
  #Bagging
  bag.gender=randomForest(train_set$gender~.,data=train_set[,1:150], mtry=150, ntree=500, importance=TRUE)##mtry=p
  bag.pred = predict(bag.gender,test_set)
  OUT2[2,k]= mean(bag.pred==test_set$gender)
  # #Random Forest. m = 12
  # RF.fit=randomForest(train_set$gender~.,data=train_set[,1:150], mtry=12,importance=TRUE, ntree=500) ##mtry=p^0.5
  # RF.pred   = predict(RF.fit,test_set)
  # OUT2[3,k] = mean(RF.pred==test_set$gender)
  #Boosting
  gbm_algorithm = gbm((as.numeric(train_set$gender)-1)~.,data=train_set[,1:150], distribution = "bernoulli", n.trees = 1000, shrinkage=0.01, interaction.depth = 2)
  gbm_predicted =predict(gbm_algorithm, test_set, n.trees =1000, type = 'response') #giving the probability by "response"
  pred=factor(ifelse(gbm_predicted<=0.5, 0, 1))
  OUT2[4,k]=mean(pred==(as.numeric(test_set$gender)-1))
}

mean(OUT2[1,])
sd(OUT2[1,])
mean(OUT2[2,])
sd(OUT2[2,])
mean(OUT2[3,])
sd(OUT2[3,])
mean(OUT2[4,])
sd(OUT2[4,])

rownames(OUT2) = c("Decision Tree", "Bagging", "Random Forest", "Boosting")
boxplot(t(OUT2),col="orange", main="5-Fold Validation Comparison", ylab="Accuracy")


LOOOUT2=matrix(0,nrow=4,ncol=1000);
for (k in 1:1000) 
{
  test.ID <- k
  train_set <- morph2[-test.ID, ]
  test_set <- morph2[test.ID, ]
  tree.gender=tree(train_set$gender~.,data=train_set[,1:150])
  tree.pred  =predict(tree.gender,test_set,type="class")
  #table(tree.pred,test_set$gender)
  LOOOUT2[1,k]=mean(tree.pred==test_set$gender)
}
mean(LOOOUT2[1,])
sd(LOOOUT2[1,])
