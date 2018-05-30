Advertising=read.csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv", header=TRUE); 
newdata=Advertising[,-1]
fix(newdata)
View(newdata)
names(newdata)
pairs(newdata)
lm.fit=lm(sales~TV,data=Advertising) ## to get Table 3.1
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit) 
lm.fit=lm(sales~TV+radio+newspaper,data=Advertising)
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
lm.fit1=lm(sales~newspaper,data=Advertising)
summary(lm.fit1)

lm.fit2=lm(sales~newspaper+TV,data=Advertising)
summary(lm.fit2)

lm.fit3=lm(sales~newspaper+TV+radio,data=Advertising)
summary(lm.fit3)

lm.fit4=lm(sales~TV+radio,data=Advertising)
summary(lm.fit4)

Credit=read.csv("http://www-bcf.usc.edu/~gareth/ISL/Credit.csv", header=TRUE); 
lm.fit=lm(Balance~Gender,data=Credit)
summary(lm.fit); contrasts(Credit$Gender)


Credit=read.csv("http://www-bcf.usc.edu/~gareth/ISL/Credit.csv", header=TRUE); 
lm.fit=lm(Balance~Ethnicity,data=Credit)
summary(lm.fit)

contrasts(Credit$Ethnicity)

Advertising=read.csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv", header=TRUE); 
lm.fit=lm(sales~TV*radio,data=Advertising)
summary(lm.fit)

lm.fit=lm(sales~radio*TV,data=Advertising)
summary(lm.fit)

