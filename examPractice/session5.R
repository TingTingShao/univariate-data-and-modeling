fev<-read.table(file=file.choose(), header=T)
head(fev)
names(fev)
#1.a
summary(fev)
apply(fev[, 1:3], 2, sd)
cor(fev[,1:3])
pairs(fev[, 1:3])
#1.b
X1<-ifelse(fev$Sex=="Male", 0, 1)
X1<-as.factor(X1)
X2<-ifelse(fev$Smoke=="No", 1, 0)
X2<-as.factor(X2)
fev.dummy<-cbind(fev, X1, X2)
#1.c
#1.c.1
boxplot(fev$Fev~fev$Smoke)
#1.c.2
#model: fev=a+b*X2
res.lm<-lm(fev.dummy$Fev~fev.dummy$X2)
summary(res.lm)
#diagnostics
rs<-rstandard(res.lm)
fit<-fitted(res.lm)
plot(rs~fit)
plot(rs~fev.dummy$X2)
shapiro.test(rs)
hist(rs)
#1.d.a




