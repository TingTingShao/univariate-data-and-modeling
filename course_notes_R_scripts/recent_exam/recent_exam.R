#1.a
a<-pnorm(250, 220, 35)
c<-pnorm(200, 220, 35)
a-c
#1.b
qnorm(0.2, 220, 35)
#2
car<-car_purchase_1_
head(car)
modelA<-glm(Y~X1+X2+X1*X2, family = binomial(link = logit), data = car)
summary(model1)
modelB<-glm(Y~X1, family = binomial(link = logit), data = car)
summary(model2)
#2.a
library(pscl)
#H0: beta X2 and beta X*X2 equal to 0
#H1: beta X2 and beta X*X2 not equal to 0
pR2(modelA)
pR2(modelB)
anova(modelB, modelA, test="Chisq")
#do not reject H0, ModelA is the better model

#4.a
movie<-movies_sml_5_
head(movie)
names(movie)
action0.df<-subset(movie, movie$Action==0)
action1.df<-subset(movie, movie$Action==1)
#action0<-movie[movie$Action==0, "length"]
#action1<-movie[movie$Action==1, "length"]
#normality
length(action1$length)
length(action0$length)
#homogeneity
var.test(action0$length,action1$length) #variance is not equal
t.test(movie$length~movie$Action, mu=-17, var.equal=FALSE, alternative="less") #reject H0




