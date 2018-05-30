library(MASS); library(ISLR)
data(Default)
head(Default)
attach(Default)

lda.fit= lda(default~student + balance)
lda.fit
plot(lda.fit)

lda.pred = predict(lda.fit, default)
names(lda.pred)
lda.class = lda.pred$class
table(lda.class, default)
mean(lda.class == default)vb

mean(default == "Yes")
