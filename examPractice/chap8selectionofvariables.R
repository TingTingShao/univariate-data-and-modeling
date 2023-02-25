#1 problem formulation
    #search the optimal subset of explanatory variables
#2 all possible regresions
#2.1 purpose
  #possible criteria 
  #the smallest Error SS or highest R^2
  #the highest adjusted R^2
#2.2 example
insurance<-read.table(file=file.choose(), head=T)
head(insurance)
names(insurance)
loss<-insurance$LOSS
age<-insurance$AGE
exper<-insurance$EXPER
towork<-insurance$TOWORK
miles<-insurance$MILES

library(leaps)
leap.r2<-leaps(x=cbind(age, exper, towork, miles), y=loss, method=c("r2"))
head(leap.r2)
r2<-round(leap.r2$r2, 3)
r2
combine<-cbind(leap.r2$which, leap.r2$size, r2)
dimnames(combine)<-list(1:nrow(combine), c("age", "exper", "towork", "miles", "size", "r2"))
combine

#2.3 Mallows' Cp
#Mallows' Cp is to assure: 
#that all important variables are in the model
#that there aren't too many varibles in the model
#2.3.1
  #models with little bias will tend to fall near the line Cp=p+1
  #models with substantial bias will tend to fall considerably above the line
  #models with Cp values below the line are interpreted as showing no bias, being below this line due to sampling error
leap.cp<-leaps(x=cbind(age, exper, miles, towork), y=loss, method=c("Cp"))
combine<-cbind(leap.cp$which, leap.cp$size, round(leap.cp$Cp, 3))
dimnames(combine)<-list(1:nrow(combine), c("age", "exper", "miles", "towork", "size", "Cp"))
combine
attributes(combine)
plot(leap.cp$size, leap.cp$Cp)

#3 model selection criteria
 #when the set of the candidate variables are too large, we can make use of 
 #automatic step-wise selection techniques
#3.1 partial F test: nested models
#model with 2 explanatory variables: age and towork
#model with 4 explanatory variables: age, towork, miles, and exper
reg.lm1<-lm(loss~age+towork)
reg.lm2<-lm(loss~age+towork+exper+miles)
anova(reg.lm1, reg.lm2)#p-value=0.4024>0.05, cH0 is not rejected
#3.2 Akaike information criterion (AIC)
#the smaller AIC, the better the fit
AIC(reg.lm1)#-6231.8
AIC(reg.lm2)#-6229.622 
# the AIC value for the model with age and towork is smaller, this model is to be preferred

#4 selection methods
#4.1 introduction
#in those cases when the pool of potential explanatory variables 
#X contains 40 to 60 varibles, an automatic procedure that develops the "best" 
#subset of explanatory variables X sequentially may be helpful
#4.2
#step-wise selection methods
#forward selection
#backward selection
#bidirectional selection
#4.3
slm.forward<-step(lm(LOSS~1, data=insurance), scope = ~AGE+EXPER+MILES+TOWORK, 
                  direction = "forward", data=insurance)

slm.backwards<-step(lm(LOSS~AGE+EXPER+MILES+TOWORK, 
                       data=insurance),direction = "backward" )

slm.both<-step(lm(LOSS~AGE+EXPER+MILES+TOWORK, 
                       data=insurance),direction = "both" )


#5 exercise
car<-read.table(file=file.choose(), head=T)
head(car)
names(car)
horsepower<-car$Horsepower
length<-car$Length
luggage<-car$Luggage
uturn<-car$Uturn
wheelbase<-car$Wheelbase
width<-car$Width
price<-car$MidrangePrice
leap.cp<-leaps(x=cbind(horsepower, length, luggage, uturn, wheelbase, width), y=price, method = c("Cp"))
combine<-cbind(leap.cp$which, leap.cp$size, round(leap.cp$Cp, 3))
dimnames(combine)<-list(1:nrow(combine), c("horsepower", "length", "luggage", "uturn", "wheelbase", "width", "size", "cp"))
combine#"horsepower", "length", "wheelbase", "width", best model
res.lm1<-lm(price~horsepower+length+wheelbase+width)
summary(res.lm1)
res.lm2<-lm(price~horsepower+wheelbase+width)
summary(res.lm2)

slm.both<-step(lm(MidrangePrice~Horsepower+Length+Luggage+Uturn+Wheelbase+Width, 
                  data=car), direction = "both")














