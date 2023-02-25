#Cp

head(insurance)
loss<-insurance$LOSS
age<-insurance$AGE
exper<-insurance$EXPER
miles<-insurance$MILES
towork<-insurance$TOWORK
install.packages('leaps')
library(leaps)
leap.cp<-leaps(x=cbind(age, exper, miles, towork), y=loss, method='Cp')
combine.cp<-cbind(leap.cp$which, leap.cp$size, leap.cp$Cp)
head(combine.cp)
dim(combine.cp)
dimnames(combine.cp)<-list(1:15, c('age', 'exper', 'miles', 'towork', 'size', 'cp'))
dimnames
round(combine.cp, digits = 3)

#3 model selection criteria
#3.1partial F-test for two nested models
reg.lm1<-lm(loss~age+towork)
reg.lm2<-lm(loss~age+towork+miles+exper)
anova(reg.lm1, reg.lm2)
#3.2 AIC: smaller AIC, better the fit
list(AIC_model1=AIC(reg.lm1), AIC_model2=AIC(reg.lm2))

#4 Selection methods
#4.1
slm.forward<-step(lm(loss~1), scope=~age+towork+miles+exper, direction="forward")
slm.backward<-step(lm(loss~age+towork+miles+exper), direction="backward")
slm.both<-step(lm(loss~age+towork+miles+exper), direction="both")

#5
cars<-cars1
head(cars)
horsepower<-cars$Horsepower
length<-cars$Length
luggage<-cars$Luggage
uturn<-cars$Uturn
wheelbase<-cars$Wheelbase
width<-cars$Width
midrangeprice<-cars$MidrangePrice

leap.r2<-leaps(x = cbind(horsepower, length, luggage, uturn, wheelbase, width), y=midrangeprice, method = "r2")
leap.cp<-leaps(x = cbind(horsepower, length, luggage, uturn, wheelbase, width), y=midrangeprice, method = "Cp")
#leap r2
attach(cars) 
leap <- leaps(x = cbind(Horsepower, Length, Luggage, Uturn, Wheelbase, Width), y=MidrangePrice, method=c("r2"), nbest=3)
leap
combine<-cbind(leap$which, leap$size, leap$r2)
head(combine)
n<-length(leap$size)
dimnames(combine)<-list(1:n, c("horsep", "length", "Lug", "Uturn", "WB", "Width", "size", "r2"))
dimnames
round(combine, digits=3)
#leap r2: midrangeprice ~ horsepower + luggage+uturn+ wheelbase+width+size
#leap Cp
attach(cars)
leap.cp<-leaps(x = cbind(Horsepower, Length, Luggage, Uturn, Wheelbase, Width), y=MidrangePrice, method=c("Cp"), nbest=3)
leap.cp
combine<-cbind(leap.cp$which, leap.cp$size, leap.cp$Cp)
n<-length(leap.cp$size)
dimnames(combine)<-list(1:n, c("horsep", "length", "Lug", "Uturn", "WB", "Width", "size", "Cp"))
round(combine, digits=3)
#model.cp: midrangeprice ~ horsepower + length + luggage+wheelbase+width+size
#step backwards
slm.back<-step(lm(midrangeprice~horsepower+length+luggage+uturn+wheelbase+width), direction = "backward")
#step foraward
slm.for<-step(lm(midrangeprice~1), scope=~horsepower+length+luggage+uturn+wheelbase+width, direction = "forward")
#step both
slm.both<-step(lm(midrangeprice~horsepower+length+luggage+uturn+wheelbase+width), direction = "both")
#model.both: midrangeprice ~ horsepower + wheelbase + width

#suggested model:
res.lm1<-lm(midrangeprice~horsepower+wheelbase+width)
summary(res.lm1)
rs<-rstandard(res.lm1)
fit<-fitted(res.lm1)
shapiro.test(rs)
plot(rs~fit)
plot(rs~horsepower)
plot(rs~wheelbase)
plot(rs~width)
plot(cooks.distance(res.lm1))
cars[cooks.distance(res.lm1)>0.4,]
length(horsepower)

