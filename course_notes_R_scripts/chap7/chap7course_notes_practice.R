temp<-`temp_warm(1)`
head(temp)
annual=temp$annual
lat=temp$Latitude
res.lm1<-lm(annual~lat)
summary(res.lm1)
plot(annual~lat)
abline(res.lm1)
fit<-fitted(res.lm1)
rs<-rstandard(res.lm1)
shapiro.test(rs)
hist(rs, prob=TRUE)
plot(rs~fit)
plot(rs~lat)

#example vapor pressure of water
temp<-c(10, 20, 30, 40, 50, 60)
vapor<-c(9.2, 17.5, 31.8, 55.3, 92.5, 149.4)
plot(vapor~temp)
res.lm2<-lm(vapor~temp)
abline(res.lm2)
summary(res.lm2)
rs<-rstandard(res.lm2)
fit<-fitted(res.lm2)
par(mfrow=c(1,2))
plot(rs~fit)
plot(rs~vapor)
res.lm3<-lm(vapor~temp+I(temp^2))
summary(res.lm3)
fit<-fitted(res.lm3)
rs<-rstandard(res.lm3)
plot(rs~fit)
plot(rs~temp)

#example bacteria deaths
time<-1:15
bact<-c(335, 211, 197, 166, 142, 106, 104, 60, 56, 38, 36, 32, 21, 19, 15)
res.lm4<-lm(bact~time)
summary(res.lm4)
rs<-rstandard(res.lm4)
shapiro.test(rs)
plot(cooks.distance(res.lm4))
in_bact<-log(bact)
reslog.lm<-lm(in_bact~time)
summary(reslog.lm)
rs<-rstandard(reslog.lm)
shapiro.test(rs)
plot(cooks.distance(reslog.lm))

#example surface tension of water based coatings
amount <- c(0.05,0.1,0.15,0.2,0.25,0.5)
surface <-c(70,60,49,41,30,46)
ex2<-data.frame(surface,amount)
res.lm5<-lm(surface~amount)
summary(res.lm5)
rs<-rstandard(res.lm5)
fit<-fitted(res.lm5)
plot(rs~fit)
plot(rs~amount)

plot(cooks.distance(res.lm5))
ex2[cooks.distance(res.lm5)>5,]
amount_new<-amount[-6]
surface_new<-surface[-6]
res.lm6<-lm(surface_new~amount_new)
summary(res.lm6)
rs<-rstandard(res.lm6)
fit<-fitted(res.lm6)
plot(rs~amount_new)
plot(rs~fit)
plot(cooks.distance(res.lm6))

city <- c("Belgium", "Athens", "Ankara", "Bangkok")
res.pred_CI<-predict(res.lm1, list(lat=c(50, 37, 39, 13)), interval = "confidence")
pred_CI<-data.frame(city, res.pred_CI)
pred_CI
res.pred_pre<-predict(res.lm1, list(lat=c(50, 37, 39, 13)), interval = "prediction")
pred_pre<-data.frame(city, res.pred_pre)
pred_pre

long<-temp$Longitude
res.lm7<-lm(annual~lat+long)
summary(res.lm7)
rs<-rstandard(res.lm7)
shapiro.test(rs)
fit<-fitted(res.lm7)
plot(rs~fit)
plot(rs~lat)
plot(rs~long)
plot(cooks.distance(res.lm7))

wine<-`wine(1)`
head(wine)
quality<-wine$quality
flavor<-wine$flavor
res.lm8<-lm(quality~flavor)
summary(res.lm8)
plot(flavor~quality)

indc<-wine$indicator
library(ggplot2)
pl1<-qplot(x=flavor, y=quality, color=factor(indc), data=wine)
pl1
res.lm9<-lm(quality~flavor+indc)
summary(res.lm9)
fit<-fitted(res.lm9)
plot(x=flavor, y=fit)

res.lm10<-lm(quality~flavor+indc+I(flavor*indc))
summary(res.lm10)


#10.1
runtest<-`runtest(1)`
head(runtest)
ox<-runtest$oxygen
rst<-runtest$rstpulse
maxp<-runtest$maxpulse
runt<-runtest$runtime
weight<-runtest$weight
age<-runtest$age
slm.both<-step(lm(ox~rst+maxp+runt+weight+age), direction = "both")
res.lm13<-lm(ox~maxp+runt+age)
summary(res.lm13)
res.lm14<-lm(ox~runt+age)
summary(res.lm14)
res.lm15<-lm(ox~runt)
summary(res.lm15)
rs<-rstandard(res.lm15)
shapiro.test(rs)
fit<-fitted(res.lm15)
plot(rs~fit)
plot(rs~runt)
plot(cooks.distance(res.lm15))
#10.2
chol<-`chol_R(1)`
head(chol)
choll<-chol$CHOL
age<-chol$AGE
height<-chol$HEIGHT
weight<-chol$WEIGHT
slm.both<-step(lm(choll~age+height+weight), direction = "both")
res.lm16<-lm(choll~age+height)
summary(res.lm16)
rs<-rstandard(res.lm16)
shapiro.test(rs)
fit<-fitted(res.lm16)
plot(rs~fit)
plot(rs~age)
plot(rs~height)





