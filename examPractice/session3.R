#1
iran<-c(128,125,133,104,146,132,125,118,129,124)
belgium<-c(160,128,169,105,151,164,162,177,185,150,182,158,156,123,141,176,162,172)
#1.a
#normality
shapiro.test(iran)
shapiro.test(belgium)
#check variance
var.test(iran, belgium)
#1.b
t.test(iran, belgium, var.equal = F) #not the same
#1.c
t.test(iran, belgium, var.equal = F, conf.level = 0.90) #-40.67051 -19.97393
#1.d
t.test(iran, belgium, var.equal = F, conf.level = 0.90)
#1.e
t.test(iran, belgium, var.equal = F, alternative="less",conf.level = 0.90)

#2
blood.df<-read.table(file=file.choose(), header=T, sep=",")
head(blood.df)
#2.a
person50<-subset(blood.df, blood.df$age<50)
person68<-subset(blood.df, blood.df$age>68)
#2.b
#H0: both age groups have the same mean level of testosterone level
#H1: both age groups have the different mean level of testosterone level
#2.c
shapiro.test(person50$testost)
shapiro.test(person68$testost)
var.test(person50$testost, person68$testost) # p-value = 0.1803>0.05
#2.d
t.test(person50$testost, person68$testost, var.equal = T, conf.level = 0.95)

#3
library(faraway)
data(tvdoctor)
#3.a
cor.test(tvdoctor$tv,tvdoctor$life) #-0.6058468 
#3.b
cor.test(tvdoctor$doctor, tvdoctor$life) #-0.587
#3.c
cor.test(tvdoctor$doctor, tvdoctor$tv) #0.6197134 
#3.d
pairs(tvdoctor)

#4
senic<-read.table(file=file.choose(), header = T)
head(senic)
names(senic)
#4.a
senic.sub<-select(senic, c(length, risk, fac, xray))
head(senic.sub)
apply(senic.sub, 2, summary)
apply(senic.sub, 2, sd)
apply(senic.sub, 2, var)
par(mfrow=c(2,2))
apply(senic.sub, 2, boxplot)
#4.b
apply(senic.sub, 2, shapiro.test) #normality was rejected for length of stay
cor(senic.sub, method="spearman")
#4.c
#4.c.1
#risk=a+b*length_of_stay
#H0: b=0, H1:b ≠ 0
res.lm<-lm(senic.sub$risk~senic.sub$length)
summary(res.lm) #1.177e-09 < 0.05 there is a linear correlation
#ß1=0.374
#28.4% of the variability can be explained by the regression model
plot(senic.sub$length, senic.sub$risk)
abline(res.lm)

res.lm2<-lm(senic.sub$risk~senic.sub$fac)
summary(res.lm2) #5.592e-06 there is regression
res.lm3<-lm(senic.sub$risk~senic.sub$xray) #p-value: 5.592e-06 R-squared:  0.1702
summary(res.lm3) #p-value: 4.585e-07 R-squared:  0.2056

















