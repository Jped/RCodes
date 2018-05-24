#Jonathan Pedoeem
#Project 2
#REU UNCW 2018

library(dplyr)

#Step1
fn <- file.choose();
morph2NC         =read.csv(file=fn,header=T);
morph2NCMale     =subset(morph2NC,subset=morph2NC$gender=="M") 
morph2NCFemale   =subset(morph2NC,subset=morph2NC$gender=="F")
# morph2NCDist     =subset(morph2NC, subset=morph2NC$picture_num==0) 
morph2NCDID      =distinct(morph2NC, id_num, .keep_all=T)
setdiff(morph2NCDID$id_num, morph2NCDist$id_num)

distinctMales = distinct(morph2NCMale, id_num,.keep_all=T)
sum(distinctMales$race=="B")
sum(distinctMales$race=="W")
sum(distinctMales$race=="A")
sum(distinctMales$race=="H")
sum(distinctMales$race=="O")

distinctFemales = distinct(morph2NCFemale, id_num, .keep_all=T)
sum(distinctFemales$race=="B")
sum(distinctFemales$race=="W")
sum(distinctFemales$race=="A")
sum(distinctFemales$race=="H")
sum(distinctFemales$race=="O")


sum(morph2NCDID$race=="B")
sum(morph2NCDID$race=="W")
sum(morph2NCDID$race=="A")
sum(morph2NCDID$race=="H")
sum(morph2NCDID$race=="O")


fn <- file.choose();
morph2C       =read.csv(file=fn,header=T)
#step 2
morph2CD      = distinct(morph2C, id_num, .keep_all=T)
morph2CMale    =subset(morph2C,subset=morph2C$gender=="M") 
morph2CFemale  =subset(morph2C,subset=morph2C$gender=="F")

distinctMalesCleaned = distinct(morph2CMale, id_num,.keep_all=T)
sum(distinctMalesCleaned$race=="B")
sum(distinctMalesCleaned$race=="W")
sum(distinctMalesCleaned$race=="A")
sum(distinctMalesCleaned$race=="H")
sum(distinctMalesCleaned$race=="O")

distinctFemalesCleaned = distinct(morph2CFemale, id_num,.keep_all=T)
sum(distinctFemalesCleaned$race=="B")
sum(distinctFemalesCleaned$race=="W")
sum(distinctFemalesCleaned$race=="A")
sum(distinctFemalesCleaned$race=="H")
sum(distinctFemalesCleaned$race=="O")

sum(morph2CD$race=="B")
sum(morph2CD$race=="W")
sum(morph2CD$race=="A")
sum(morph2CD$race=="H")
sum(morph2CD$race=="O")

#step 3a
maleAggregate = aggregate(picture_num~id_num,morph2CMale,max)
sum(maleAggregate$picture_num ==1)
sum(maleAggregate$picture_num ==2)
sum(maleAggregate$picture_num ==3)
sum(maleAggregate$picture_num ==4)
sum(maleAggregate$picture_num ==0)
femaleAggregate= aggregate(picture_num~id_num,morph2CFemale,max)
sum(femaleAggregate$picture_num ==1)
sum(femaleAggregate$picture_num ==2)
sum(femaleAggregate$picture_num ==3)
sum(femaleAggregate$picture_num ==4)
sum(femaleAggregate$picture_num ==0)

morph2CDA = aggregate(picture_num~id_num,morph2C,max)
sum(morph2CDA$picture_num ==1)
sum(morph2CDA$picture_num ==2)
sum(morph2CDA$picture_num ==3)
sum(morph2CDA$picture_num ==4)
sum(morph2CDA$picture_num ==0)

#3b
maleAA = aggregate(age~id_num,morph2CMale,min)
sum(maleAA$age<20)
sum(maleAA$age>19 & maleAA$age<30)
sum(maleAA$age>29 & maleAA$age<40)
sum(maleAA$age>39 & maleAA$age<49)
sum(maleAA$age>48)
femaleAA= aggregate(age~id_num,morph2CFemale,min)
sum(femaleAA$age<20)
sum(femaleAA$age>19 & femaleAA$age<30)
sum(femaleAA$age>29 & femaleAA$age<40)
sum(femaleAA$age>39 & femaleAA$age<49)
sum(femaleAA$age>48)

morph2CAA = aggregate(age~id_num,morph2C,min)
sum(morph2CAA$age<20)
sum(morph2CAA$age>19 & morph2CAA$age<30)
sum(morph2CAA$age>29 & morph2CAA$age<40)
sum(morph2CAA$age>39 & morph2CAA$age<49)
sum(morph2CAA$age>48)


#step 4
#code to get dataframe, taken from project 1
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


#lets now clean up the name of the image in morph2 so we can use it for a merge
getID <- function(x){
  start <- str_locate(x, "_")[1]
  return(as.integer(substr(x,1,start-1)))
}
library(plyr)
morph2$id_num <- lapply(morph2[,1],getID)

#now we perform a merg from here to the cleancsv.
morph2Race = merge(morph2CD[,c("id_num","race")],morph2, by="id_num")

#lets get a pie chart of races.
raceCount  = count(morph2Race, "race")
pie(raceCount$freq, main="Breakup of Race", col=rainbow(5))
legend("topright", c("Asian", "Black", "Hispanic", "Other", "White"), cex = 0.8,
       fill = rainbow(5))

#also boxplot of races against age. 
ages <- as.numeric(morph2Race$age)
boxplot(ages~morph2Race$race,names=c("Asian", "Black", "Hispanic", "Other", "White"),ylab="Age",xlab="Race", main="Age vs. Race")
summary(as.numeric(subset(morph2Race, race=="B")$age))
summary(as.numeric(subset(morph2Race, race=="A")$age))
summary(as.numeric(subset(morph2Race, race=="H")$age))
summary(as.numeric(subset(morph2Race, race=="O")$age))
summary(as.numeric(subset(morph2Race, race=="W")$age))
  