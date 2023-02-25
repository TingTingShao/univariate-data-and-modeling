#logistic regression
PP<-read_excel("/Users/shaotingting/Documents/univariate_data_and_modeling/data/data_for_univariate_data_and_modeling/political_party.xlsx")
head(PP)
names(PP)
#obtain model
glm.log1<-glm(Republican~pro_capital_punishment, data=PP, family=binomial(link=logit))
summary(glm.log1)
plot(glm.log1)
#obtain parameter estimates
##look at predicted values
fit<-fitted(glm.log1)
combine<-data.frame(cbind(PP$Republican, PP$pro_capital_punishment, fit))
head(combine)
plot(PP$pro_capital_punishment, PP$Republican, type="p")
points(PP$pro_capital_punishment, fit)
#3.6 interpretation of b1. odd(x+1)=exp(b1)*odd(x)
b1<-glm.log1$coefficients
exp(b1)
#3.7 simple logistic regression model with categorical explanatory variable
#3.7.1 binary predictor variable should be coded as 0 or 1
glm.log2<-glm(PP$Republican~PP$gender, family=binomial(link="logit"))
summary(glm.log2)
plot(PP$Republican, PP$gender, type="p")
points(PP$gender, fitted(glm.log2))
exp(glm.log2$coefficients)
#3.7.2 use of categorical predictor variable(not binary)
titanic <- read_excel("Documents/univariate_data_and_modeling/Data/data_for_univariate_data_and_modeling/titanic.xlsx")
head(titanic)
names(titanic)
titanic$class.f<-as.factor(titanic$Class_New)
head(titanic)
glm.log3<-glm(titanic$survived~titanic$class.f, family = binomial(link=logit))
summary(glm.log3)
##change reference group
titanic$class_Ref<-relevel(titanic$class.f, ref="4")
glm.log4<-glm(titanic$survived~titanic$class_Ref, family=binomial(link=logit))
summary(glm.log4)
exp(glm.log4$coefficients)
#3.8 Goodness of fit
##3.8.1 Hosmer-Lemeshow goodness of fit test
library(ResourceSelection)
hoslem<-hoslem.test(PP$Republican, fit)
hoslem #P value>0.05, do not reject H0 (the logistic regression model fits the data)
##3.8.2 wald test to test significance of regression coefficients
summary(glm.log1) #p value 0.034<0.05, pro_capital_punishment is a significant variable
##3.8.3 deviance
summary(glm.log1) 
##pseudo R2
library(pscl)
pR2(glm.log1) # McFadden 0.0126, small, small part of deviance caused by pro_capital_punishment
#classification of observations
##classification table
table(PP$Republican, fit>0.5)
#ROC curve
library(ROCR)
glm.log1<-glm(Republican~pro_capital_punishment, data=PP, family=binomial(link=logit))
predict<-fitted(glm.log1)
pred<-prediction(predict, PP$Republican)
perf<-performance(pred, measure = "tpr", x.measure="fpr")
plot(perf, main="sensitivity vs false positive rate", colorize=TRUE, 
     colorkey.relwidth=0.5, lwd=4.5)
##area under ROC curve
perf_auc<-performance(pred, measure="auc")
perf_auc@y.values



