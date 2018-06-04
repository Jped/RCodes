#Jonathan Pedoeem
#Project 1 
#REU UNCW 2018

fn <- file.choose();
library("stringr")
morph2=read.csv(file=fn,header=F); 
grabLetter <- function(x){
  gender<- str_extract(x, "[M,F]")
  if(gender == "M"){
    return(1);
  }
  else if (gender == "F"){
    return(0);
  } 
}
grabAge <- function(x){
  start <- str_locate(x, "[M,F]")[1];
  end <- str_locate(x, ".JPG")[1];
  return(as.numeric(substr(x,start+1, end-1)));
}
dim(morph2)
morph2$gender <- lapply(morph2[,1],grabLetter);
morph2$age    <- lapply(morph2[,1],grabAge);

sum(is.na(morph2$gender))
sum(is.na(morph2$age))
dim(morph2)

morph2= morph2[,-1]

# pairs(as.matrix(morph2[,1:2]))
## Here we make the pie chart of the Sex
pieGender = c(sum(morph2$gender==0), sum(morph2$gender==1));
piePercent= round(pieGender/sum(pieGender),2)
pie(pieGender, labels=piePercent, main="Breakup of Gender", col= rainbow(length(pieGender)));
legend("topright", c("Female","Male"), cex = 0.8,
       fill = rainbow(length(pieGender)))


## Here we make the histogram graph of the different ages.
ages <- as.numeric(morph2$age)
hist(ages, main="Frequency of Ages", xlab="Age", col="blue")
summary(ages)

## Here we have a histogram of the vector of the BIF 
BIFs<- as.vector(as.matrix(subset(morph2,select=-c(age,gender))))
hist(BIFs, main="Frequency of BIF Values", xlab="BIF Value", col="blue")
summary(BIFs)


boxplot(ages, main="Boxplot of Ages", ylab="Ages")
boxplot(BIFs, main="Boxplot of BIF Values", ylab="BIF Values")

boxplot(ages ~ as.numeric(morph2$gender), ylab="Age", xlab="Gender", main="Boxplot of Age vs. Gender")


BIFmales<- as.vector(as.matrix(subset(morph2,select=-c(age,gender),subset=morph2$gender==1)))
BIFfemales<- as.vector(as.matrix(subset(morph2,select=-c(age,gender),subset=morph2$gender==0)))
boxplot(BIFmales, main="Boxplot of BIF Values for Males", ylab="BIF Values");
boxplot(BIFfemales, main="Boxplot of BIF Values for Females", ylab="BIF Values");
