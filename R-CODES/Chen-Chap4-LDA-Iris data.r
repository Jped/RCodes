library(MASS)
attach(iris)
View(iris)
names(iris)
table(Species)

##https://tgmstat.wordpress.com/2014/01/15/computing-and-visualizing-lda-in-r/

## To fit the LDA model for the WHOLE data
lda.fit=lda(Species~., data=iris)
## OR use
lda.fit = lda(formula = Species ~ ., 
         data = iris, 
         prior = c(1,1,1)/3)
#The DOT in the formula argument means that we 
#use all the remaining variables in data as covariates. 
#The prior argument sets the prior probabilities of class membership. 
#If unspecified, the class proportions for the training set are used. 
#If present, the probabilities should be specified in the order of the factor levels.

lda.fit
names(lda.fit)
lda.fit$prior
lda.fit$counts
lda.fit$means
lda.fit$scaling
lda.fit$svd
#As we can see above, a call to lda returns the prior probability of each class, 
#the counts for each class in the data, the class-specific means for each covariate, 
#the linear combination coefficients (scaling) for each linear discriminant 
#(remember that in this case with 3 classes we have at most two linear discriminants) 
#and the singular values (svd) that gives the ratio of the 
#between- and within-group standard deviations on the linear discriminant variables.

prop = lda.fit$svd^2/sum(lda.fit$svd^2); prop
#We can use the singular values to compute the amount of the 
#between-group variance that is explained by each linear discriminant. 
#In our example we see that the first linear discriminant explains 
#more than {99\%} of the between-group variance in the iris dataset.


plot(lda.fit)
## The plot() function produces plots of the linear
#discriminants, obtained by computing first 
#(0.8293776)*(Sepal.Length)+(1.5344731)*(Sepal.Width)+(-2.2012117)*(Petal.Length)+(-2.8104603)*(Petal.Width)
#and next
#(0.02410215)*(Sepal.Length)+(2.16452123)*(Sepal.Width)+(-0.93192121)*(Petal.Length)+(2.83918785)*(Petal.Width)
#for each of the training observations.

####################################################
## Let's do example by HAND
#> iris[1,]
#Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#1          5.1         3.5          1.4         0.2  setosa

names(lda.fit)
lda.fit$scaling[,1]

sum(lda.fit$scaling[,1]* (iris[1,1:4]))
sum(lda.fit$scaling[,2]* (iris[1,1:4]))

OUT=NULL;
for(i in 1:length(Species))
{
  S1=sum(lda.fit$scaling[,1]* (iris[i,1:4]))
  S2=sum(lda.fit$scaling[,2]* (iris[i,1:4]))
  OUT=rbind(OUT, c(S1, S2))
} 
OUT
###################################################

lda.pred=predict(lda.fit, iris)
names(lda.pred)

table(lda.pred$class, Species)

## To view the posterior and final classification results
View(cbind(lda.pred$posterior, lda.pred$class))
