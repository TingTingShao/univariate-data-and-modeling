senic<-read.table(file="/Users/shaotingting/Documents/univariate_data_and_modeling/exercises/data_exercise/SENIC.DAT", header = TRUE)
head(senic)
names(senic)
senic$reg<-as.factor(senic$reg)
#1.a
library(dplyr)
X1<-as.factor(rep(0,nrow(senic)))
X2<-X1
X3<-X1
X1<-ifelse(senic$reg=='1', 1, 0)
X2<-ifelse(senic$reg=='2', 1, 0)
X3<-ifelse(senic$reg=='3', 1, 0)
senic_dummy<-mutate(senic, X1, X2, X3)
head(senic_dummy)
#1.b
lm1<-lm(risk~X1+X2+X3, data=senic_dummy)
summary(lm1)
head(model.matrix(lm1))
plot(lm1)
#mean
library(tidyverse)
description<-senic%>%
  group_by(reg)%>%
  summarise(
    avg=mean(risk),
    sd=sd(risk),
    number=n()
  )
description
boxplot(risk~reg, data=senic, col=c(2,3,4,5), names=levels(senic$reg))
lm2<-lm(risk~reg, data=senic)
summary(lm2)
model.matrix(lm2)
#1.c
library(BSDA)
upper.limit<-c(0,0,0,0)
lower.limit<-c(0,0,0,0)
for (i in 1:4){
y<-tsum.test(mean.x = description$avg[i], s.x = description$sd[i], n.x=description$number[i],
          conf.level = 0.95)
lower.limit[i]=y[[4]][1]
upper.limit[i]=y[[4]][2]
#if(i==4){print(lower.limit, upper.limit)} do not need this one
}
##View((y)) to check which one to choose
description_conf<-mutate(description, upper.limit, lower.limit)
description_conf
#1.d
risk.aov1<-aov(risk~reg, data=senic)
summary(risk.aov1)
#1.e
diffs<-TukeyHSD(risk.aov1, whih = "reg", conf.level = 0.95)
diffs
plot(diffs)
#1.f
#check homogeneity
library(car)
leveneTest(risk~reg, data=senic)
#check normality
by(senic$risk, INDICES = senic$reg, FUN=shapiro.test)
shapiro.test(lm2$residuals)
hist(lm2$residuals)
#check cooks.distance
plot(cooks.distance(lm2))










