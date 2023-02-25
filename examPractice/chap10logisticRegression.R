#1 introduction
# estimate the probability of response = 1 p[Republican=1], explanatory  proi_capital_punishment
library(readxl)
political_party <- read_excel("Documents/univariate_data_and_modeling/Data/data_for_univariate_data_and_modeling/political_party.xlsx")
head(political_party)
names(political_party)
#2 regression model with binary response variable
#Yi {0,1}

#3 simple logistic regression 
#3.1
#logistic regression is used to predict a binary(0 or 1) dependent varibale using a given set of independent variables
#p[y=1]=exp(ß0+ß1x)/(1+exp(ß0+ß1x))
#3.2 properties of logistic response function
  #either monotone increasing or monotone decreasing (depend on the sign ß1)
  #is almost linear in the range where E[Y] ranges from 0.2-0.8
  #it approaches 0 and 1 at the two ends of the x range
  #it can be linearized p'=ß0+ß1x, p'=ln(p/(1-p))
#3.3 interpretation of the odds
  # odds=probability of events /probability of no events
  # odds<1 corresponds with p<0.5
  # odds do have a lower bound of 0, but there is no upper bound
  # p=odds/(1+odds)
#3.4 log-llikelihood function
  # p^'=ln(p^/1-p^)=b0+b1x=ln(odds)

#how to obtain parameter estimates in R
pp<-political_party
names(pp)
#model1
glm.log1<-glm(pp$Republican~pp$pro_capital_punishment, family = binomial(link=logit))
summary(glm.log1)#ß0=-1.448, ß1=0.175, p[y=1]=exp(-1.448+0.175x)/(1+exp(-1.448+0.175x))
#model1 1 look at the predicted values
combine<-data.frame(cbind(pp$Republican, pp$pro_capital_punishment, fitted(glm.log1)))
colnames(combine)<-c("republican", "pro_capital_punishment", "fitted_values")
head(combine)

#model1 2 this predicted and observed values can be visualized in the following graph
plot(pp$pro_capital_punishment, pp$Republican)
points(pp$pro_capital_punishment, combine$fitted_values)

#3.6 interpretation of b1
#3.6.1 general
  #p'(x)=b0+b1x, p'(x+1)=b0+b1(x+1)
  #odds radio=OR= odds x+1/odds x=exp(b1), odds x+1 = exp(b1) * odds x
#3.6.2 example interpreting odds ratio for continuous explanatory variable
glm.log1$coefficients
exp(glm.log1$coefficients) #p^=0.235+1.2*x 
                          # the odds of voting republican is 1.20 times larger for
                          # each additional point on the pro_capital_punishment score
    # for example: pro = 3, odds=0.39764, pro=4, odds=0.39764*1.20=0.477

#3.7 simple logistic regression model with categorical explanatory variable

#3.7.1 use of binary predictor variables
#binary predictor variables should be coded as 0 or 1
glm.log2<-glm(pp$Republican~pp$gender, family = binomial(link="logit"))
summary(glm.log2)
exp(glm.log2$coefficients) # the odds ratio to vote republican for males to females is 0.15
                          # the odds to vote republican for males is 0.15 times the odds to vote republican for females
                          # the odds to vote republican for females is 1/0.15=6.67 times the odds to vote republican for males
combine2<-data.frame(cbind(pp$gender, pp$Republican, fitted(glm.log2)))
colnames(combine2)<-c("gender", "republican", "fitted value")
combine2[c(1, 18:22),]
p<-0.5454
p2<-0.1523
f<-p/(1-p)
m<-p2/(1-p2)
f/m #6.68

#3.7.2 use of categorical predictor variable(not binary)
library(readxl)
titanic <- read_excel("Documents/univariate_data_and_modeling/Data/data_for_univariate_data_and_modeling/titanic.xlsx")
head(titanic)
names(titanic)
titanic$Class.f<-as.factor(titanic$Class_New)
glm.log3<-glm(titanic$survived~titanic$Class.f, family = binomial(link=logit))
summary(glm.log3)
#change class_new=4 as the reference category
titanic$class_rf<-relevel(titanic$Class.f, ref="4")
glm.log4<-glm(titanic$survived~titanic$class_rf, family = binomial(link=logit))
summary(glm.log4)
exp(glm.log4$coefficients) 
#(Intercept) titanic$class_rf1 titanic$class_rf2 titanic$class_rf3 
#0.3150074         5.2822069         2.2430799         1.0702008 
# interpret:
# odds ratio of 1st class to crew =5.28
# the odds to survive the titanic is 5 times larger for passengers from first class than for the crew
# odds ratio of 2nd class to crew =2.243
# the odds to survive the titanic is 2 times larger for passengers from second class than the crew
#the odds ratio of 3rd class to crew =1 and is not significant

#3.8 goodness of fit
#chi-square goodness of fit test (to test whether the logistic response is appropriate) Hosmer and Lemeshow
#Wald test of significant coefficients (individual significance)
#deviance: -2log(likelihood)
#pseudo R2
#ROC curve (predictive power of the logistic model)

library(ResourceSelection)
republican<-pp$Republican
#H0: the logistic regression model fits the data
#H1: the logistic regression model does not fit the data
hoslem<-hoslem.test(republican, fitted(glm.log1))
hoslem
combine<-cbind(hoslem$observed, hoslem$expected)
combine
# the test is not powerful when you have a small number of observations. This assumes
# that the expected number of observations in each cell is at least 5 (at least in 20% of the cells)

#3.8.2 Wald test to test significance of regression coefficients
#statement of hypothesis
#H0: ßj=0
#H1: ßj≠0
#W=estimate/standard error, z value
summary(glm.log1) #p value=0.034<0.05, reject H0, hence pro_capital_punishment is a significant variable in he logistic model

#3.8.3 deviance
#Deviance=-2(log-likelihood of fitted model)
#deviance is a statistic that compares the log-likelihood of the fitted model to the log-likelihood of a saturated model
#a saturated model is a model with n parameters that fits the n observations
#->n parameters for n observations
#->perfect fit (residuals will all be zero)
#->log-likelihood for a saturated model =0

# deviance = 2(log-likelihood of saturated model)-2(log-likelihood of fitted model)
# =0-2(log-likelihood of fitted model) (log likelihood of fitted model<log likelihood of saturated model=0)
#this difference is always positive
  # the smaller the deviance, the closer the fitted model is to the saturated model
#-> this statistics can be used as goodness of fit criterion
  # the larger the deviance, the poorer the fit is between the fitted model and the saturated model

#3.8.4 pseudo R2
#pseudo R2=1- -2(log-likelihood of fitted model)/-2(log-likelihood of null model)
library(pscl)
pR2(glm.log1) #pseudo R2= McFadden = 0.0126
              # variable pro_capital_punishment can only explain a small part of deviance

#3.9 classification of observations
#clssification table
table(republican, fitted(glm.log1)>0.5) # manual set 0.5 as the cuttoff value
                                        # if predicted probability >0.5 the observation is classified as voting Republican
                                        # if the predicted probability <0.5 the observation is classified as not voting republican
#ROC curve
#ROC curve is a visual measurement for the predictive ability of the (logistic) regression model
#the area under the ROC curve indicates the performance of a binary classifier in a single value

#3.10.2 how to obtain the ROC curve in R
library(ROCR)
predict<-fitted(glm.log1)
pred<-prediction(predict, republican)
perf<-performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main="sensitivity vs false positive rate", colorize=T, colorkey.relwidth=0.5, lwd=4.5)

#area under the ROC curve
perf.auc<-performance(pred, measure = "auc")
perf.auc@y.values #area under the curve is 0.554, which is not good, the model does not have a good ddiscriminating ability
#models with a higher predictive power has a higher AUC

#3.10.3 example
predict2<-prediction(fitted(glm.log2), republican)
predict2
perf2<-performance(predict2, measure = "tpr", x.measure = "fpr")
plot(perf2, main="sensitivity vs false positive rate", colorize=T, colorkey.relwidth=0.5, lwd=4.5)
performance(predict2,measure = "auc")@y.values #0.72 
#two curve under the same plot
plot(perf, colorize=T, lwd=3)
plot(perf2, add=T, colorize=T, lwd=3)
abline(0,1,lty=2)

#4 multiple logistic regression
#4.1 general
  # monotonic and sigmoid in shape
  # almost linear when p is between 0.2-0.8
  # predictor variables may be interaction effects, curvature, quantitative qualitative
  # logistic regression model with only qualitative variables is called a log-linear model
  # maximum likelihood estimation is used to find estimates for the parameters
#4.2 wald test, hierarchical step by step(manually)

#4.3 partial deviance
#model0: intercept only
#model1: 
glm.log.M1<-glm(pp$Republican~pp$gender, family = binomial(link=logit))
glm.log.M2<-glm(pp$Republican~pp$gender+pp$pro_capital_punishment, family = binomial(link=logit))
glm.log.M3<-glm(pp$Republican~pp$gender+pp$pro_capital_punishment+pp$pro_welfare_reform, 
                family = binomial(link=logit))
glm.log.M4<-glm(pp$Republican~pp$gender+pp$pro_capital_punishment+pp$pro_welfare_reform+pp$pro_fed_support_ed, 
                family = binomial(link=logit))
#compare model2 to model1
#H0: ßpro_capital_punishment=0
#H1: ßpro_captial_punishment≠0
anova(glm.log.M1, glm.log.M2, test = "Chisq") #p value<0.05, keep pro_punishment variable
#compare model3 to model2
#H0: ßpro_welfare_reform=0
#H1: ßpro_welfare_reform≠0
anova(glm.log.M3, glm.log.M2, test="Chisq") #p value>0.05, drop pro_welfare

#compare model4 to model2
#H0: ßpro_fed_support=ßpro_capital_punishment=0
#H1: ßpro_fed_support≠0 or ßpro_capital_punishment≠0
anova(glm.log.M4, glm.log.M3, test="Chisq") #p value =0.778>0.05, accept H0

#4.4 interpret the result
#4.4.1 interpret the parameter estimates
summary(glm.log.M2)
exp(glm.log.M2$coefficients)
#(Intercept)                 pp$gender pp$pro_capital_punishment 
#0.10251833                0.04604147                1.99762585 
#the odds to vote to republican for male(gender=1) is 0.05 times the odds to vote republican for female
#(gender=0), when taking pro_capital_punishment into account
#per one unit increase on the score of pro_capital_punishment, the odds for voting republican is increasing 
#2 times, taking gender into account

#4.4.2 classification table
table(pp$Republican, fitted(glm.log.M2)>0.5)
#4.4.3 generalized R2 value
pR2(glm.log.M2) #pseudo R2 value is 0.24
#4.4.4 create the ROC curve and area under the curve
pred.M2<-prediction(fitted(glm.log.M2), republican)
perf.M2<-performance(pred.M2, measure = "tpr", x.measure = "fpr")
plot(perf.M2, main="sentivity vs false positive rate", colorize=T, colorkey.relwith=0.5, lwd=4.5)
performance(pred.M2, measure = "auc")@y.values
N












