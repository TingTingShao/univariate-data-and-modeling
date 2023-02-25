#simple linear regression
temp<-read.table(file = file.choose(), head=T)
#1.1.2 in general
  #population Y=å+ßX
  #sample y=a+bx
#1.2 in R
res.lm1<-lm(temp$annual~temp$Latitude)
summary(res.lm1)
#estimated regression model: annual = 35-0.5*latitude
    #interpretation
    #1. for a city with latitude = 0, this model predicts an annual temperature of 35
    #2. since slope<0, there is a negative relationship between latitude and annual, the larger the value for latitude, the lower the annual temperature
    #3. for an increase of 1 in latitude, the regression model predicts average decrease 0.5 in annual temperature
#1.3 statistical model

#variability components
#total SS = Error SS + Model SS
#R^2 = Model SS/total SS
summary(res.lm1)#R-square = 0.815, latitude can explain 81.5% of the total variability in annual
plot(temp$annual~temp$Latitude)
abline(res.lm1)
#3 model diagnostics
#3.1 in general
  #check linearity: 
      #make plot of standardized residuals versus fitted response
      #make plot of standardized residuals versus each regressor
  #check normality of the standardized residuals:
      #perform a shapiro wilk test(and make a histogram)
  #check for influential points
      #make a plot of cooks distance
#example
#step1
fit<-fitted(res.lm1)
rs<-rstandard(res.lm1)
plot(rs~fit) #random pattern
plot(rs~temp$Latitude)#random pattern, the assumption of linearity is satisfied
#step2
shapiro.test(rs)
hist(rs)#normally distributed
#step3
plot(cooks.distance(res.lm1))
#example vapor pressure of water
temp1<-c(10,20,30,40,50,60)
vapor<-c(9.2,17.5,31.8,55.3,92.5,149.4)
plot(vapor~temp1)
res.lm2<-lm(vapor~temp1)
summary(res.lm2)
abline(res.lm2)
#step1
fit<-fitted(res.lm2)
rs<-rstandard(res.lm2)
plot(rs~fit)
plot(rs~temp1) #not randomly distributed
#linearity does not hold
res.lm.q<-lm(vapor~temp1+I(temp1^2))
summary(res.lm.q)
fit<-fitted(res.lm.q)
rs<-rstandard(res.lm.q)
plot(rs~fit)
plot(rs~temp1) #linearity holds

#example
time<-1:15
bact<-c(335,211,197,166,142,106,104,60,56,38,36,32,21,19,15)
res.lm<-lm(bact~time)
summary(res.lm) #R-square = 0.845, time can explain 84.5% of the total variability in bacteria
plot(bact~time)
abline(res.lm)
#linearity
fit<-fitted(res.lm)
rs<-rstandard(res.lm)
plot(rs~time)
plot(rs~fit)#linearity does not hold
ln_bact<-log(bact)
reslog.lm<-lm(ln_bact~time)
summary(reslog.lm) #R-square = 0.989, time can explain 98.9% of the total variability in log(bacteria)
#linearity
fit<-fitted(reslog.lm)
rs<-rstandard(reslog.lm)
plot(rs~fit)
plot(rs~log(bact)) #linearity holds
#normality
shapiro.test(rs) #p value =0.6394 > 0.05, normally distributed
#cooks distance
plot(cooks.distance(reslog.lm))
bact[cooks.distance(reslog.lm)>0.3]
bact_new<-bact[!bact==211]
time_new<-time[!time==2]
res.lm3<-lm(log(bact_new)~time_new)
summary(res.lm3)

#example
amount<-c(0.05, 0.1, 0.15,0.2,0.26,0.5)
surface<-c(70,60,49,41,30,46)
ex2<-data.frame(amount, surface)
res.lm4<-lm(surface~amount)
summary(res.lm4)
plot(surface~amount)
abline(res.lm4)
plot(cooks.distance(res.lm4))
ex2[cooks.distance(res.lm4)>8, ]
amount_new<-amount[-6]
surface_new<-surface[-6]
res.lm3.new<-lm(surface_new~amount_new)
plot(surface_new~amount_new)
abline(res.lm3.new)

# prediction
city<-c("belgium","athens","ankara", "bangkok")
annual<-temp$annual
lat<-temp$Latitude
res.lm1<-lm(annual~lat)
summary(res.lm1)
res.pred.CI<-predict(res.lm1, list(lat=c(50,37,39,13)), interval="confidence")
res.pred.CI
pred_CI<-data.frame(city, res.pred.CI)
pred_CI #average annual temperature

res.pred.PI<-predict(res.lm1, list(lat=c(50,37,39,13)), interval="prediction")
pred_PI<-data.frame(city, res.pred.PI)
pred_PI #annual temperature (individual point)

#5 overview global structure for regression analysis
#6 multiple linear regression
#6.1 illustrative example
#6.2 multiple regression model in general
#6.3 evaluate multiple regression model
    #6.3.1 is there regression F test p value
    #6.3.2 how good is the regression: R^2
    #6.3.3 individual parameter estimates: t.test
annual<-temp$annual
lat<-temp$Latitude
long<-temp$Longitude
res.lm5<-lm(annual~lat+long)
summary(res.lm5)
#is there regression: F test p value<0.05, reject H0
#how good, R^2 0.856, 86% of the total variability in annual can be explained by using variables latitude and longtitude
#the obtained parameter estimates are: 
    #intercept: 34
    #estimates beta1: -0.47: if comparing cities with same longitude, the expected annual temperature decreases with 0.47 for every increase of 1 in latitude
    #estimates of beta2: -0.087: if comparing cities with same latitude, the expected annual temperature decreases with 0.087 for every increase of 1 in latitude
#interpretation of the coefficients

#6.6 checking assumptions in R
    #6.6.a check linearity
    #6.6.b check normality
    #6.6.c check influential points
#6.6.1 check linearity
fit<-fitted(res.lm5)
rs<-rstandard(res.lm5)
plot(rs~fit)
plot(rs~lat)
plot(rs~long)
#6.6.2 check normality
shapiro.test(rs) #normality can be assumed
#6.6.3 check for influential points
plot(cooks.distance(res.lm5))
temp[cooks.distance(res.lm5)>0.3,]
annual_new<-annual[-2]
lat_new<-lat[-2]
long_new<-long[-2]
res.lm6<-lm(annual_new~lat_new+long_new)
summary(res.lm6) #the obervation is not influential

#7 polynomial model
#7.1 a polynomial model with one regressor
#8 interaction model
res.lm8<-lm(annual~lat+long+I(lat*long))
summary(res.lm8) #I(lat * long) p value = 0.429 > 0.05 drop
#9 qualitative independent variable
wine<-read.table(file=file.choose(), head=T)
head(wine)
quality<-wine$quality
flavor<-wine$flavor
res.lm<-lm(quality~flavor)
summary(res.lm)
indic<-wine$indicator
library(ggplot2)
pl1<-qplot(flavor, quality, color=factor(indic), data=wine, size=3)
pl1
#regression model with indicator variable (but without interaction term)
res.lm1<-lm(quality~flavor+indic)
summary(res.lm1)
fit<-fitted(res.lm1)
plot(fit~flavor)
#regression model with indicator variable (but with interaction term)
res.lm1<-lm(quality~flavor+indic+I(flavor*indic))
summary(res.lm1)#drop interaction term

