temperature<-read.table(file=file.choose(), head=T)
head(temperature)
names(temperature)
#average
mean(temperature$October)
summary(temperature$October)
library(dplyr)
by_area<-temperature%>%group_by(Area)%>%
  summarise(AvgOct=mean(October), n=n())
by_areaN

by_area1<-temperature%>%group_by(Area)%>%
  summarise(avg=mean(October), n=n(),  median=median(October), quantile(October, 0.25),
            quantile(October, 0.75))
by_area1

boxplot(temperature$October~temperature$Area)
#the mode
#mode is the most frequent or most typical value, usually we compute the mode
#for a categorical variable
table<-table(temperature$Area)
which.max(table)
#2 measure of dispersion(or variability)
#2.1 the range
#range=max-mini
#2.2 the variance
varT<-var(temperature$October)
varT
#2.3 standard deviation
sdT<-sd(temperature$October)
sdT
#2.4 interquartile range
IQRT<-IQR(temperature$October)
IQRT

#exercise
#1.a
beta.df<-read.table(file=file.choose(), head=T)
head(beta.df)
#1.b
names(beta.df)
#2.a
chick.df<-read.table(file=file.choose(), head=T)
head(chick.df)
#2.b
names(chick.df)
#2.c
summary(chick.df$weight)
var(chick.df$weight)
sd(chick.df$weight)
#2.d
summary(chick.df$chicken)
var(chick.df$chicken)
sd(chick.df$chicken)#no it does not make sense
#2.e
transmute(chick.df, No=as.factor(chick.df$chicken), weight=chick.df$weight, feed=chick.df$feed)
#2.f
by(chick.df$weight, chick.df$feed, summary)
by(chick.df$weight, chick.df$feed, var)
#by(chick.df$weight, chick.df$feed, sd)
#2.g
table(chick.df$feed)
#3.a
monica.df<-read.table(file=file.choose(), head=T, sep=";")
head(monica.df)
#3.b
by(monica.df$age, monica.df$sex, summary)
by(monica.df$age, monica.df$sex, var)
by(monica.df$age, monica.df$sex, boxplot)
boxplot(monica.df$age~monica.df$sex)
#3.c
boxplot(monica.df$age)
#3.d
boxplot(monica.df$age~monica.df$sex)
#3.e
fage<-subset(monica.df, monica.df$sex=="f", select=age)
head(fage)
denf<-density(fage$age)
mage<-subset(monica.df, monica.df$sex=="m", select=age)
denm<-density(mage$age)
#3.f
hist(fage$age, probability = T)
lines(denf)
hist(mage$age, probability = T)
lines(denm)
