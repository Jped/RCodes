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
# 
# morph2= morph2[,-1]

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
  #Boosting
  gbm_algorithm = gbm((as.numeric(train_set$gender)-1)~.,data=train_set[,1:150], distribution = "bernoulli", n.trees = 1000, shrinkage=0.01, interaction.depth = 2)
  gbm_predicted =predict(gbm_algorithm, test_set, n.trees =1000, type = 'response') #giving the probability by "response"
  pred=factor(ifelse(gbm_predicted<=0.5, 0, 1))
  LOOOUT2[1,k]=mean(pred==(as.numeric(test_set$gender)-1))
}
write.csv(LOOOUT2,file="boosting.csv")

