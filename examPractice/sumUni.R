#chap1descriptive
temperature<-read.table(file=file.choose(), head=T)
head(temperature)
names(temperature)
#average
mean(temperature$October)
summary(temperature$October)
library(dplyr)
by_area<-temperature%>%group_by(Area)%>%
  summarise(AvgOct=mean(October), n=n())
by_area

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
fivenum(temperature$October)
IQRT

#exercise1
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
par(mfrow=c(1,2))
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
#or
denf<-density(monica.df$age[which(monica.df$sex == 'f')])
plot(denf, main="females")
denm<-density(monica.df$age[which(monica.df$sex=="m")])
plot(denm, main="males")
#3.f
hist(fage$age, probability = T)
lines(denf)
hist(mage$age, probability = T)
lines(denm)

#chap2visualizedata
#categorical data: tables bar plots, and pie charts
#continuous data histograms and boxplots
temperature<-read.table(file=file.choose(), head=T)
head(temperature)
names(temperature)
#2 categorical variables
#2.1 frequency table
area.freq<-table(temperature$Area)
area.freq
xtabs(~temperature$Area)
#relative frequencies
area.Rfreq<-table(temperature$Area)/nrow(temperature)
round(area.Rfreq, 2)
library(gmodels)
CrossTable(temperature$Area, digit=2, prop.r = F, prop.c = F, prop.chisq = F)

#2.2 bar charts
par(mfrow=c(1,2))
bp.f<-barplot(area.freq, xlab="Area", ylab="absolute frequency", ylim=c(0,10), col=3)
bp.Rf<-barplot(area.Rfreq, xlab="Area", ylab="relative frequency", ylim=c(0, 0.3), col=3)
barplot(sort(area.freq, decreasing=T))
barplot(sort(area.Rfreq))

#2.3 pie chart
pp.f<-pie(area.freq)
pie(area.freq, main = "Area")
pie(area.freq, main="Area", col=rainbow(length(area.freq)))

#2.4 dot chart
area.df<-data.frame(area.freq)
dp.f<-dotchart(area.df$Freq, labels=area.df$Var1)
areaR.df<-data.frame(area.Rfreq)
dotchart(areaR.df$Freq, labels = areaR.df$Var1)

#3 continuous variables
#3.1 histograms
hist(temperature$annual)
hist(temperature$annual, probability = T)
lines(density(temperature$annual))
#3.2 boxplots
par(mfrow=c(1,2))
boxplot(temperature$annual, col=11)
boxplot(temperature$annual~temperature$Area, col=12)
#3.3 scatter plot
plot(temperature$annual~temperature$Latitude, col=2, cex=2)
plot(temperature)


#chap3
#1 sample versus population
#2 discrete and continuous random variable
#2.1 discrete variable
#2.2 continuous variable
#3 discrete distributions
#3.2 binomial distribution
#example Y~B(n=3, p=0.25)
k<-0:3
densbin<-dbinom(k, 3, 0.25) #d means density
names(densbin)<-k
densbin
#compute comulative density
cumdens<-pbinom(k, 3, 0.25) #p means probability
names(cumdens)<-k
cumdens
#visualize 
par(mfrow=c(1,2))
barplot(densbin)
barplot(cumdens)
#3.3 poisson distribution
dpois(17, lambda = 12)
#the probability of having 17 cars crossing the bridge in a particular minute is 3.83%
cumprob<-ppois(16, lambda=12)
1-cumprob
#the probablity of having 17 cars or more crossing the bridge in a particular minute is 10.12%
#4 continuous distribution
#4.1 normal distribution
#µ=1.9, sigma=0.5
pnorm(1.4, 1.9, 0.5) #p[Y<=1.4]= 0.1586
1-pnorm(1.4, 1.9, 0.5)
pnorm(2.4, 1.9, 0.5)-pnorm(1.4, 1.9, 0.5)
#standard normal distribution
pnorm(1, mean=0, sd=1)-pnorm(-1, mean=0, sd=1)
#4.2 T distribution
pt(2, df=10)-pt(-2, 10) #pt percentage point
#F distribution
#equality of two variances
1-pf(2, 32, 9)
1-pf(2, 32, 9)
#4.4 the chi square distribution
#p(Y<=8)0, with 5 df
pchisq(8,5)

#chap4
temp<-read.table(file=file.choose(), head=T)
head(temp)
area<-as.factor(temp$Area)
area

westT<-temp%>%filter(Area=="West")%>%
  select(October)
summary(westT)
westT<-subset(temp, temp$Area=="West", select=October)
summary(westT)
nrow(westT)
#check normality
shapiro.test(westT$October) #p value = 0.7584, larger than 0.05, can not reject H0, is normally distributed
#t.test(westT)
#assume ∑ in a population is known as 1.5
library(BSDA)
z.test(westT$October, sigma.x = 1.5)
#if population ∑ is not known
t.test(westT$October)
#confidence interval for a proportion
prop.test(240, 400)

#6.1
sigma<-8
mu<-101.4
n<-4
conf.in<-0.99
zsum.test(mean.x=101.4, sigma.x = sigma, n.x=n, conf.level = conf.in)
#6.2
x<-(2.576*8)/2
n<-x^2
n
ceiling(n)

#session2
#1
#1.a
#1.b
#1.c
dice<-1:6
dice.p<-rep(1/6, 6)
dice.c<-rep(1/6, 6)
for (i in 2:length(dice.c)) {
  dice.c[i]<- dice.c[i-1]+1/6
}
dice.c
barplot(dice.p)
plot(dice, dice.c, type="s")

#2.d
pbinom(6, 10, 0.5)
#2.e
1-pbinom(6, 10, 0.5)
#2.f
qbinom(0.5, 10, 0.5)
#2.g
qbinom(0.75, 10, 0.5)

#3.c
ppois(0, 2)
#3.d
1-ppois(0,2)
#3.e
ppois(0, 2*50)
#3.f
p<-seq(from=0, to=1, by=0.05)
number<-qpois(p, 4)
barplot(dpois(number, 4))
#or
number<-seq(0, 16, 1)
barplot(dpois(number, 4))

#4.a
#4.b
#4.c
dnorm(100, 85, sqrt(500))
#4.d
pnorm(100, 85, sqrt(500))
#4.e
pnorm(80, 85, sqrt(500))
#4.f
1-pnorm(60, 85, sqrt(500))
#4.g
lim<-qnorm(0.975, 85, sqrt(500))
y<-60:170
den<-dnorm(y, 85, sqrt(500))
plot(y, den, type="l")
polygon(c(60,y,170), c(0,den,0), col="green")
#4.h
qnorm(0.975, 85, sqrt(500))

#5.a
pt(1, 15)
#5.b
qt(0.95, 15)
#5.c
x.95 <- round(qt(0.95, 15), 2)
lim.u <- 4 #round(qt(0.999, 15), 2)
lim.l <- -lim.u
x <- seq(lim.l,lim.u,length=1000)
i.95 <- x >= x.95 & x <= lim.u
hx <- dt(x, 15)

plot(x, hx, type="n", xlab="", ylab="",
     main="Student t with 15 DF",
     axes=FALSE)
lines(x, hx)
polygon(c(x.95,x[i.95],lim.u), c(0,hx[i.95],0), col="green")
result <- "P(T > t) = 0.05"
mtext(result,3)
axis(1, at=c(lim.l, 0, x.95, lim.u), pos=0)

#6.a
qchisq(0.05, 10)
#7.a
pf(10, 4, 9) - pf(5, 4, 9)
#7.b
qf(0.95, 4, 9)

#8.a
sigma <- 0.3
n <- 10
xmean <- 41.924
conf <- 0.95
alpha <- 1-conf

lcl <- xmean-qnorm(1-alpha/2)*sigma/sqrt(n)
ucl <- xmean+qnorm(1-alpha/2)*sigma/sqrt(n)
CI <- list(lcl=lcl, ucl=ucl)
CI
#or
library(BSDA)
zsum.test(mean.x = 41.924, sigma.x = 0.3, n.x = 10, conf.level = 0.95) # 41.73806 42.10994

#9.a
tsum.test(mean.x = 750, s.x = 30, n.x = 20, conf.level = 0.95) #735.9596 764.0404

#10
blood.df<-BLOOD
head(blood.df)
names(blood.df)
#10.a
z.test(blood.df$X.age., sigma.x=5) #60.53468 61.40257
#10.b
t.test(blood.df$X.prolactn.) #12.36901 15.67076
#10.c
age<-subset(blood.df$X.age., blood.df$X.age.>50 & blood.df$X.age.<60)
n<-nrow(blood.df)
n
m<-length(age)
m
m/n
#10.d
prop.test(m,n) # 0.2928577 0.3763604
#10.e
subset.df<-subset(blood.df, blood.df$X.age.<50)
#10.f
shapiro.test(subset.df$X.testost.) #p=0.7296 normally distributed
t.test(subset.df$X.testost., conf.level = 0.99) #  14.07455 38.92545

#chap5hypothesistesting

#structure of a hypothesis test five steps
#1 state the appropriate hypotheses
#H0, H1
#2 choose a level of significance for the test
#3 state the appropriate test statistics
#variance is known Z test
#variance is unknown t test
#4 compute the observed value of the test statistic and p value

#hypothesis test for the mean when sd is assumed to be known
#H0: µ=12 versus H1: µ<12 with µ the average October temperature in Western Europe
#step2: select alpha = 0.05
#step3 z test
#compute
temp<-read.table(file=file.choose(), head=T)
head(temp)
westT<-subset(temp, temp$Area=="West", select=October)
z.test(westT$October, alternative = "less", mu=12, sigma.x = 1.5, conf.level = 0.95)
#p value = 0.01738<0.05, reject H0, average October temperature in Western Europe is less than 12
mean=mean(westT$October)
nrow(westT)
zsum.test(mean, alternative = "less", n.x=9, mu=12, sigma.x = 1.5, conf.level = 0.95)

zsum.test(mean, alternative = "two.sided", n.x=9, mu=12, sigma.x = 1.5, conf.level = 0.95)

#hypothesis test for the mean when sigma is unknown
#test normality: 
#if size >=25, no need to test, t.test
#if size is small, shapiro-test
#step1 the hypothesis
#H0 µ=12 versus H1 µ<12 with µ the average october temperature in western europe
#step2 alpha = 0.05
#step3 t.test
t.test(westT$October, mu=12, alternative = "less", conf.level = 0.95)
#p value = 0.03136 < 0.05, reject H0, average October temperature is significantly less than 12
#non parametric alternative the sign test
#if the normality does not hold
#hypothesis H0: median = 12 H1: median<12
#alpha = 0.05
#test statistic 
library(BSDA)
SIGN.test(westT$October, md=12, alternative = "less")
#inference for two independent samples: t test
#descriptive statistics
westA<-subset(temp, temp$Area=="West", select = annual)
eastA<-subset(temp, temp$Area=="East", select = annual)
meanW<-mean(westA$annual)
meanE<-mean(eastA$annual)
varW<-var(westA$annual)
varE<-var(eastA$annual)
nW<-nrow(westA)
nE<-nrow(eastA)
boxplot(westA$annual, eastA$annual, names=c("west annual", "east annual"))
#t.test
#H0: µeast=µwest versus µeast≠µwest
shapiro.test(westA$annual)
shapiro.test(eastA$annual)
var.test(westA$annual, eastA$annual)#p=0.07814>0.05 variances are the same H0
t.test(westA$annual, eastA$annual, var.equal =T)

#distribution free test(non parametric test): wilcoxon rank sum test
wilcox.test(westA$annual, eastA$annual)

#test for paired data: dependent data
comb_decathlon<-read.table(file=file.choose(), head=T)
head(comb_decathlon)
nrow(comb_decathlon)
shapiro.test(comb_decathlon$decastar_1500-comb_decathlon$olympic_1500)
#shapiro.test(comb_decathlon$decastar_1500)
#shapiro.test(comb_decathlon$olympic_1500)
t.test(comb_decathlon$decastar_1500, comb_decathlon$olympic_1500, paired = T, alternative = "greater")

##distribution free test(non parametric test): wilcoxon rank sum test
SIGN.test(comb_decathlon$decastar_1500, comb_decathlon$olympic_1500, alternative = "greater")


#10 test for proportions
#10.1 hypothesis test for one proportion
#H0: p=p0 versus H1: p≠p0
#X~B(n,p), let p^ = X/n, the observed proportion of successes
#10.1.1 Normal approximation
#X~B(n,p), with X the number of success, np>=5, and n(1-p)>=5. then X~N(np, np(1-p)).
#X/n~N(p, p(1-p)/n)
#example
#H0: p=0.5 versus H1: p≠0.5
#alpha=0.05
n<-length(temp$annual)
n #n=35
m<-sum(temp$annual>10)
m #m=14>5
prop.test(x=14, n=35, p=0.5) #p=0.3105 > 0.05, can not reject H0, 
#the proportion of European cities with annual temp>10 is not significantly different from 0.5

#10.1.2 normal approximation can not be used
binom.test(x=14, n=35, p=0.5, alternative="two.sided")

#hypothesis test for two proportions
#H0: pNM=pSE versus H1: pNM≠pSE
NM_area<-subset(temp, temp$Area=="North" | temp$Area=="West", select=annual)
SE_area<-subset(temp, temp$Area=="South" | temp$Area=="East", select = annual)
n_NM<-length(NM_area$annual)
n_SE<-length(SE_area$annual)
number_nm<-sum(NM_area$annual>=9)
number_se<-sum(SE_area$annual>=9)
x<-c(number_nm, number_se)
n<-c(n_NM, n_SE)
x
n
prop.test(x,n) #P = 0.6324 >0.05, do not reject H0. There is no significant difference
#between the proportion of cities in N and W Europe which have an annual temp of at least 9, and with the proportion of cities in S and E which ahve an annual temp of at least 9
#11 chi-square goodness of fit test
#H0: p1=p2=p3=p4
#H1: there is no uniform distribution
birthdays<-c(10, 12, 10, 16)
predprob<-c(0.25, 0.25, 0.25, 0.25) 
chisq.test(birthdays, p=rep(1/length(birthdays), 4)) #p=0.5724>0.05, do not reject H0

#12 power and sample size (in case sigma is known)
#12.1 power of the one sample t test
# power of a test is the probability of making a correct decision(to reject H0) when H0 is false
#Power=P(reject H0|H1 is true)
#example
#H0: µ=10 versus H1: µ≠10
westT<-subset(temp, temp$Area=="West", select = October)
n<-length(westT$October)
delta<-1
sd<-1.5
sig.level<-0.05
power.t.test(n=n, delta = delta, sd=sd, sig.level = sig.level, power=NULL, type="one.sample", alternative = "two.sided")
#power = 0.42
#meaning when H1(µ≠10) is true, reject H0, there is only 0.42 chance
#A power of 0.4209651 is considered low and it means that 
#the test has a low chance of detecting a true difference between the sample mean and the hypothesized mean

#larger difference in hypothesized mean 
delta2<-seq(from=1, to=2.5, by=0.25)
power.t.test(n=n, delta = delta2, sd=sd, sig.level = sig.level, 
             power=NULL, type="one.sample", 
             alternative = "two.sided")#larger difference in means, the power of your test will increase

power.t.test(n=n, delta = delta, sd=sd, sig.level = sig.level, power=NULL, 
             type="one.sample", 
             alternative = "one.sided")#one.sided, the power = 0.572, increased

#sd
power.t.test(n=n, delta = delta, sd=1, sig.level = sig.level, 
             power=NULL, type="one.sample", 
             alternative = "two.sided")#power=0.748
power.t.test(n=n, delta = delta, sd=2, sig.level = sig.level, 
             power=NULL, type="one.sample", 
             alternative = "two.sided")#power=0.262
#sd increase, power decreases

#sample size
n<-seq(from=10, to=40, by=5)
power.t.test(n=n, delta = delta, sd=sd, sig.level = sig.level, 
             power=NULL, type="one.sample", 
             alternative = "two.sided")# power = 0.4691805, 0.6708562, 0.8072909, 0.8920169, 0.9415758, 0.9692876, 0.9842420
#sample size increases, power increases
#12.2 sample size computation
delta<-1
sd<-1.5
sig.level<-0.05
power.t.test(n=NULL, delta=delta, sd=sd, sig.level = sig.level, 
             power=0.8, type="one.sample", 
             alternative = "two.sided") #n=20
#12.3 relationship between power and sample size
#compute power (and sample sizes) for proportion, then use the power.prop.test
#H0: p1=p2 versus H1: p1≠p2
#with alpha = 0.05
#with p1=0.4, p2=0.1, n1=9 and n2=8
#power.prob.test assumes equal sample sizes, in case this is not so, we take n as min(n1,n2)
power.prop.test(n=8, p1=0.4, p2=0.1, power=NULL, alternative = "two.sided")

#example gene expression
#mean expression level of the given gene, measures with the same technology is µ=1.5, sd=0.44
#experiment: 1.9, 2.5, 1.3, 2.1, 1.5, 2.7, 1.7, 1.2
#H0: µ=1.5 versus H1: µ>1.5
#expect the gene to be up-regulated in the condition of the study, and have an increase of 20%
#power of the test
n<-9
delta<-0.2*1.5
sd<-0.44
sig.level<-0.05
power.t.test(n=n, delta = delta, sd=sd, sig.level = sig.level, power=NULL, 
             type="one.sample", alternative = "one.sided")

#H0 will be rejected in favor of H1 in 58% of the times when the true underlying population meanis 1.8
#vary the values of the alternative hypothesis
delta2<-seq(from=0.3, to=1, by=0.1)
power.t.test(n=n, delta=delta2, sd=sd, sig.level = sig.level, power=NULL, type="one.sample", alternative="one.sided")
#compute size of the sample needed
delta<-0.3
sd<-0.44
sig.level=0.05
power.t.test(n=NULL, delta = delta, sd=sd, sig.level = sig.level, 
             power=0.8, type="one.sample",
             alternative="one.sided")


#chap6correlation
temp<-read.table(file=file.choose(), head=T)
head(temp)
temp_small<-select(temp, c("Latitude", "Longitude", "annual"))
pairs(temp_small)
#2 pearson correlation coefficient
#2.1 definition
# indication if there is a linear relationship, r always has a value between -1 and +1
# r<0: negative linear trend
# r>0: positive linear trend
# r=0: no LINEAR relation
#2.3 population correlation coefficient
#step1: hypothesis
#H0: p=0 versus H1: p≠0 with p=population correlation coefficient
#step2: alpha=0.05
#step3: test
#2.4 correlation coefficients in R
cor(temp_small)
cor.test(temp_small$annual, temp_small$Latitude) # cor = -0.903 very strong association
cor.test(temp_small$annual, temp_small$Longitude) #cor = -0.477 association is less stronger

#3 spearman correlation coefficient
#non-parametric alternative to pearson correlation coefficient
shapiro.test(temp_small$annual) #p=0.03879 <0.05, normality can not hold
cor(temp_small, method="spearman")
cor.test(temp_small$annual, temp_small$Latitude, method="spearman") 
cor.test(temp_small$annual, temp_small$Longitude, method="spearman") 

#chap7linearregression

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
  #quality=a+ß1*flavor ß1 significant, different intercept
  #quality=a+ß1*flavor+ß2*(flavor*indicator) ß2 is significant, different slope
res.lm1<-lm(quality~flavor+indic+I(flavor*indic))
summary(res.lm1)#drop interaction term

#session3
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

#session4
###########################
#### EXERCISE SESSION 4 ###
###########################

body.df <- read.table(file=file.choose(), row.names = 1, header=TRUE)

## a ##

summary(body.df)

# preparing for plotting, we need columns 1 to 15
body.names <- names(body.df[,1:15])

# Check your working directory for the output of the following!!
par(mfrow = c(3, 5))
# boxplot by for-loop
for (i in 1:length(body.names)){
  png(paste(paste("boxplot",i, sep = "_"),"png", sep = "."))
  boxplot(body.df[,i], main = paste(body.names[i])) 
  dev.off()
}


# density plots by for-loop
for (i in 1:length(body.names)){
  png(paste(paste("density",i, sep = "_"),"png", sep = "."))
  plot(density(body.df[,i]), main = paste(body.names[i]))
  dev.off()
}

# index plot by for-loop
for (i in 1:length(body.names)){
  png(paste(paste("index",i, sep = "_"),"png", sep = "."))
  plot(sort(body.df[,i]), ylab = "", main = paste(body.names[i]), pch = 16)
  dev.off()
}
par(mfrow = c(1, 1))
plot(sort(body.df[,3]), ylab = "", main = paste(body.names[3]), pch = 16)
identify(sort(body.df[,3]), labels=row.names(body.df), n = 2)

body.cor <- cor(body.df[,1:15])
body.cor[1,]

## b ##

X1 <- as.vector(rep(0,nrow(body.df)))
X1 <- ifelse(body.df$Sex == "Male", 0, 1)
body.df <- data.frame(body.df, X1)

## c ##

body.s <- sample(1:nrow(body.df), 50, replace=FALSE)
body.mod <- body.df[-body.s,]
body.new <- body.df[body.s,]

## d ##

d.2
body.names <- names(body.df[1:15])
# par(mfrow = c(2, 7))
for (k in 2:length(body.names)){
  png(paste(paste("scatter",k, sep = "_"),"png", sep = "."))
  plot(body.mod[,1]~body.mod[,k], xlab = paste(body.names[k]), ylab = "Weight", main = "")
  lines(stats::lowess(body.mod[,1]~body.mod[,k]), col = "RED")
  dev.off()
}

# d.3
# full model - no interaction
body.lm.F <- lm(Weight~Height+Shoulder+Chest+Waist+Abdo+Hip+Thigh+Bicep+Forearm+Knee+Calf+Ankle+Wrist+Age,
                data = body.mod)

summary(body.lm.F)

# sorting P-values
lab <- labels(summary(body.lm.F)$coefficients)
body.p <- data.frame(lab[[1]],summary(body.lm.F)$coefficients[,4])
names(body.p) <- c("lab","P")

body.p <- body.p[order(-body.p$P),]
body.p

# reduced model based on P-value
body.lm.R <- lm(Weight~Height+Shoulder+Chest+Waist+Hip+Thigh+Forearm+Knee+Calf+Age,
                data = body.mod)

summary(body.lm.R)

# reduced model based on AIC
body.AIC <- step(lm(Weight~1, data = body.mod),
                 scope=~Height+Shoulder+Chest+Waist+
                   Abdo+Hip+Thigh+Bicep+
                   Forearm+Knee+Calf+Ankle+
                   Wrist+Age, 
                 direction="forward")

summary(body.AIC)

# d.4
# Model misspecification
body.fit <- fitted(body.lm.R) # gives the fitted values used in calculating the residuals
body.rs <- rstandard(body.lm.R) # gives standardized residuals, that is, residuals divided by the variance

png("residuals.png")
plot(body.rs~body.fit, main = "Standardized residuals vs. Fitted values")
lines(lowess(body.rs~body.fit))
abline(h = c(-3,3))
abline(h = 0, col = "red", lty = 2)
dev.off()

names.R <- c("Height","Shoulder","Chest","Waist","Hip","Thigh","Forearm","Knee","Calf","Age")
data.R <- body.mod[names.R]
for (l in 1:length(names.R)){
  png(paste(paste("residuals",l, sep = "_"),"png", sep = "."))
  plot(body.rs~data.R[,l], xlab = paste(names.R[l]), ylab = "Standardized residuals", main = "")
  abline(h = c(-3,3))
  abline(h = 0, col = "red", lty = 2)
  dev.off()
}

# Normality of errors
shapiro.test(body.rs)

png("NormRes.png")
hist(body.rs, prob=TRUE)
dev.off()

png("QQplot.png")
plot(body.lm.R, which = 2)
dev.off()

# Influential points
png("CooksD.png")
plot(body.lm.R, which = 5)
dev.off()

# d.5 
r2.R <- data.frame(summary(body.lm.R)$r.squared, summary(body.lm.R)$adj.r.squared)
names(r2.R) <- c("R squared", "Adj.R squared")
round(r2.R, digits = 3)

# d.6
# install.packages("leaps")
library(leaps)

leap.Cp <- leaps(body.mod[,2:15], body.mod[,1], method="Cp")
leap.Ar2 <- leaps(body.mod[,2:15], body.mod[,1], method="adjr2")
leap.r2 <- leaps(body.mod[,2:15], body.mod[,1], method="r2")

# Example of model selection with Cp, adj R^2 and R^2
# Look at the variables selected for each model size
colnames( body.mod[ , leap.Cp$which[which.min(leap.Cp$Cp), ], ]) #Min Cp
colnames( body.mod[ , leap.Ar2$which[which.max(leap.Ar2$adjr2), ] ]) # Max adj R^2
colnames( body.mod[ , leap.r2$which[which.max(leap.r2$r2), ] ]) #Max R^2, NOTE all variables have been selected
co
combine.F <- cbind(leap.Cp$which,leap.Cp$size, leap.Cp$Cp, leap.Ar2$adjr2, leap.r2$r2)
dimnames(combine.F) <- list(1:131,c("Height","Shoulder","Chest","Waist","Abdo","Hip",
                                    "Thigh","Bicep","Forearm","Knee","Calf","Ankle","Wrist","Age", "size", "Cp", "Adj. R2", "R2"))
round(combine.F, digits=3)

png("Cp.png")
plot(leap.Cp$Cp~leap.Cp$size, xlim = c(0,15), ylim=c(0,15))
abline(a=0, b=1)
identify(leap.Cp$Cp~leap.Cp$size, labels=row.names(combine.F))
dev.off()

#round(combine.F[c(91,103,104,115,116),], digits = 3)

# d.7
install.packages("scatterplot3d")
library(scatterplot3d)
fit <- lm(Weight~Height+Waist, data = body.mod)
png("Scatter3D.png")
s3d <- scatterplot3d(body.mod$Height,body.mod$Waist,body.mod$Weight, main="3D Scatterplot")
s3d$plane3d(fit, lty.box = "solid")
dev.off()

## e ##

body.pred <- predict(body.lm.R, body.new, interval = "prediction", se.fit = TRUE)
round(body.pred$fit, digits = 2)
round(as.vector(body.pred$se.fit), digits = 2)

## f ##

# f.1
# Full model - all interactions
body.lm.F <- lm(Weight~Height+Shoulder+Chest+Waist+Abdo+Hip+Thigh+Bicep+Forearm+Knee+Calf+Ankle+Wrist+Age+X1+
                  I(X1*Height)+I(X1*Shoulder)+I(X1*Chest)+I(X1*Waist)+I(X1*Abdo)+I(X1*Hip)+I(X1*Thigh)+I(X1*Bicep)+
                  I(X1*Forearm)+I(X1*Knee)+I(X1*Calf)+I(X1*Ankle)+I(X1*Wrist)+I(X1*Age),data = body.mod)

summary(body.lm.F)

# sorting P-values
# Alternatively, use AIC
lab <- labels(summary(body.lm.F)$coefficients)
body.p <- data.frame(lab[[1]],summary(body.lm.F)$coefficients[,4])
names(body.p) <- c("lab","P")

body.p <- body.p[order(-body.p$P),]
body.p

# Reduced model based on p-value
body.lm.R2 <- lm(Weight~Height+Shoulder+Chest+Waist+Hip+Thigh+Forearm+Knee+Calf+Age+X1+
                   I(X1*Height)+I(X1*Shoulder), data = body.mod)

summary(body.lm.R2)

# f.2
r2.R2 <- data.frame(summary(body.lm.R2)$r.squared, summary(body.lm.R2)$adj.r.squared)
names(r2.R2) <- c("R squared", "Adj.R squared")
r2.com <- rbind(r2.R, r2.R2)
round(r2.com, digits = 3)

# f.3
lm.temp <- lm(Weight~Height+X1+I(Height*X1), data = body.mod)
reg.RM <- as.vector(c(summary(lm.temp)$coefficients[1,1], summary(lm.temp)$coefficients[2,1]))
reg.RF <- as.vector(c(summary(lm.temp)$coefficients[1,1] + summary(lm.temp)$coefficients[3,1],
                      summary(lm.temp)$coefficients[2,1] + summary(lm.temp)$coefficients[4,1]))

png("ScatterHeight.png")
plot(body.mod$Weight~body.mod$Height)
abline(coef = reg.RF, col = "red")
abline(coef = reg.RM, col = "blue")
dev.off()

## g ##

body.pred2 <- predict(body.lm.R2, body.new, interval = "prediction", se.fit = TRUE)

# Compute the prediction errors
body.pred.error1 <- as.vector(body.new[, 1] - body.pred$fit[, 1])
body.pred.error2 <- as.vector(body.new[, 1] - body.pred2$fit[, 1])
# Compute the standard deviations
sd(body.pred.error1)
sd(body.pred.error2)
# Model 2 performs better, although not by much
plot(body.pred.error1, body.pred.error2)
curve(1*x, add = TRUE, lwd = 3)

#session5
###########################
#### EXERCISE SESSION 5 ###
###########################

fev.df <- read.table(file=file.choose(), header=TRUE, sep=" ")

## a ##

summary(fev.df)
fev.cor <- cor(fev.df[,1:3])
fev.cor

pairs(fev.df[,1:3])

# boxplots
par(mfrow=c(1,3))
boxplot(fev.df[,1], main = "Age")
boxplot(fev.df[,2], main = "FEV")
boxplot(fev.df[,3], main = "Height")


# density plots
par(mfrow=c(1,3))
plot(density(fev.df[,1]), main = "Age")
plot(density(fev.df[,2]), main = "FEV")
plot(density(fev.df[,3]), main = "Height")

# index plots
par(mfrow=c(1,3))
plot(sort(fev.df[,1]), ylab = "", main = "Age", pch = 16)
plot(sort(fev.df[,2]), ylab = "", main = "FEV", pch = 16)
plot(sort(fev.df[,3]), ylab = "", main = "Height", pch = 16)

## b ##

X1 <- as.vector(rep(0,nrow(fev.df)))
X2 <- X1
X1 <- ifelse(fev.df$Sex == "Male", 0, 1) # Females are represented by an 1!
X2 <- ifelse(fev.df$Smoke == "No", 0, 1) # Smokers are represented by an 1!
fev.df <- data.frame(fev.df[,1:4], X1, fev.df[,5], X2)
names(fev.df) <- c("Age", "Fev", "Height", "Sex", "X1", "Smoke", "X2")

## c ##

# c.1
par(mfrow = c(1,1))
boxplot(fev.df$Fev~fev.df$Smoke)
#### CONFOUNDING ####

# c.2
fev.s <- lm(Fev~X2, data = fev.df)

summary(fev.s)

plot(fev.df[,2]~fev.df[,7])
abline(fev.s, col = "red")

# c.3

fev.fit <- fitted(fev.s) # gives the fitted values used in calculating the residuals
fev.rs <- rstandard(fev.s) # gives standardized residuals

par(mfrow = c(2, 2))
plot(fev.s)

plot(fev.rs~fev.fit, ylim = c(-4.5, 4.5))
abline(h = c(-3,3))
abline(h = 0, col = "red", lty = 2)

# We need to control for the other variables too!

## d ##

# d.2
# Full model
fev.F <- lm(Fev~Height+Age+X1+X2+
              I(X1*Height)+I(X1*Age)+
              I(X2*Height)+I(X2*Age), 
            data = fev.df)

summary(fev.F)
# Results are now in line with intuition

fevF.fit <- fitted(fev.F) # gives the fitted values used in calculating the residuals
fevF.rs <- rstandard(fev.F) # gives standardized residuals

# Looking for polynomial terms
par(mfrow = c(1, 1))
plot(fevF.rs~fevF.fit, ylim = c(-4.5, 4.5))
abline(h = c(-3,3))
abline(h = 0, col = "red", lty = 2)

scatter.smooth(fev.df$Age, fev.df$Fev, lpars = list(col="Red"))
scatter.smooth(fev.df$Height, fev.df$Fev, lpars = list(col="Red"))

# Include polynomial term in full model
fev.P <- lm(Fev~Height+Age+X1+X2+
              I(Height^2)+
              I(X1*Height)+I(X1*Age)+
              I(X2*Height)+I(X2*Age), 
            data = fev.df)

summary(fev.P)
par(mfrow = c(2, 2))
plot(fev.P)

# Checking polynomial fit
fevP.fit <- fitted(fev.P) # gives the fitted values used in calculating the residuals
fevP.rs <- rstandard(fev.P) # gives standardized residuals

par(mfrow=c(1,1))
plot(fevF.rs~fevF.fit, ylim = c(-4.5, 4.5), main = "First-order")
abline(h = c(-3,3))
abline(h = 0, col = "red", lty = 2)

# Significant improvement
plot(fevP.rs~fevP.fit, ylim = c(-4.5, 4.5), main = "Polynomial")
abline(h = c(-3,3))
abline(h = 0, col = "red", lty = 2)
# Mild heteroscedasticity is present but otherwise OK! 

# Reduce the model via AIC (backward selection)
fev.R <- step(fev.P, direction = "backward")
summary(fev.R)
# What is the effect of smoking on FEV, according to this model?

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

#chap8
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


#chap9anova
#1 one way anova is a testig procedure to compare the means of multiple groups
pollution<-read.table(file=file.choose(), header = T)
summary(pollution)
head(pollution)
names(pollution)
#1.1 descriptive statistics
library(psych)
rain<-pollution$Rain
region<-pollution$regio
describe<-describeBy(rain, region, mat=T)
names(describe)
describe.st<-subset(describe, select = c("group1", "n", "mean", "sd",
                                         "median","min","max"))
describe.st
boxplot(rain~region)

library(tidyverse)
pollution%>%group_by(regio)%>%
  summarise(mean(Rain))

# problem formulation
#2 F-test for multiple means in one-way anova
#2.1 anova-testing principal
#H0: µ1=µ2=...=µr
#H1: the means are not all the same
#example
glm1<-lm(Rain~regio, data=pollution)
summary(glm1)#p value = 4.245e-06 < 0.05 reject H0, not all the means are the same

#3 one way anova as a linear model
#3.1 use of treatment coding
#3.2 use of sum coding
glm2<-lm(Rain~regio, data=pollution, contrasts = list(regio="contr.sum"))
summary(glm2)
#model diagnostics
#4.1 assumptions made
#anova make the following assumptions
#assumption of normality
#assumption of homogeneity of variance
#assumption of independence

#4.2 checking assumptions

#independence: design of the experiment
#constant within-group variance: visual check of the boxplot, levene test and Bartlett test
#normality: shapiro test, or shapiro test of the within group residuals + histogram of within group residuals
#influential observations: cook's distance

#4.3 check assumption of homogeneity of variance
library(car)
leveneTest(Rain~regio, data=pollution) #p value = 0.1799>0.05 homogeneity holds
#remark1 if the levene test is rejected, be aware that there exists some robustness
#remark2 in case there is no homogeneity of variance, a modification of F test can be used
oneway.test(Rain~regio, data=pollution, var.equal = F)

#4.4 chechk assumption of normality
fit<-fitted(glm2)
rs<-rstandard(glm2)
shapiro.test(rs)
hist(rs)#though the normality is rejected with shapiro.test, histogram is symmetric
#in case of asymmetric, non parametric version: kruskal-wallis test
plot(density(rs))
#4.5 checking for influential observations
plot(cooks.distance(glm2)) #there are no influential observations

#5 pariwise comparisons of treatment effects
#is there a pair of regions with the same average amount of rain
#test all pairs of regions on the same average amount of rain

#5.1 
poll.aov1<-aov(Rain~regio, data=pollution)
summary(poll.aov1) #p<0.05 H0 all the means are equal rejected
#5.2
#planned comparison of means in ANOVA
#sd is based on the observations of all the groups
#in case there is homogeneity of variances, use LSD in anova
#in case there is no homogeneity of variances, use teh two-sample t-test for independent groups

#H0: µNO=µC versus H1: µNO≠µC
pairwise.t.test(rain, region, p.adjust.method = "none") #p value>0.05 accept H0

#5.3 multiple comparisons
#Tukey HSD test
diffs<-TukeyHSD(poll.aov1, which="regio", conf.level = 0.95)
diffs
#forestplot
plot(diffs) # the adjusted P-value by the aov function depends on assumptions of the residuals. For the 
# p-value to be correct, these residuals need to be independent, normally distributed and 
# have constant variance. the following section: non-parametric function that does not require the normality assumption
#remark1
library(agricolae)
HSD.test(poll.aov1, "regio", group = F)$comparison
scheffe.test(poll.aov1, "regio", group = F)$comparison
pairwise.t.test(rain, region, p.adjust.method = "bonferroni")
#remark2 homogenous groups
Hgroups.scheffe<-scheffe.test(poll.aov1, "regio", group=T) 
Hgroups.scheffe$groups
#remark3 Holm's approach
pairwise.t.test(rain, region, p.adjust.method = "holm")
#remark4 aov function in R needs the explanatory variables to be a factor, var1.f<-as.factor(var1)

#6 extensions
#6.1 the kruskal wallis test
# kruskal wallis test is a non-parametric version of one-way analysis of variance. The assumption underlying this test is that the 
# measurements come from a contiuous distribution, but not necessarily a normal distribution
# the test is based on an analysis of variance using the ranks of the data values, not the data values
#statement of hypothesis
# H0: the location parameters of the distribution of X are the same in each group
# H1: the location parameters differ in at least one group
kruskal.test(rain~region) #p values = 6.649e-05, reject H0
#multiple comparisons
library(pgirmess)
kruskalmc(rain, region)

###TWO WAY ANOVA#####
########################

#7.1 introduction
diet.df<-read.table(file=file.choose(), header=T)
head(diet.df)
names(diet.df)
#descriptive statistics
by_diet_jogging<-diet.df%>%group_by(JOGGING, DIET)%>%
  summarise(mean=mean(LOSS), sd=sd(LOSS), n=n())
by_diet_jogging

boxplot(diet.df$LOSS~diet.df$JOGGING)
boxplot(diet.df$LOSS~diet.df$DIET) #remark: when we only consider one factor at a time, we miss the "joint" effect
# such joint effect is called interaction,interaction plot from package "stat", to reveal potential interactions

diet.f<-as.factor(diet.df$DIET)
jogging.f<-as.factor(diet.df$JOGGING)
loss<-diet.df$LOSS
interaction.plot(diet.f, jogging.f, loss, type="b", pch=c(1,2,3), col=c(1,2,3)) # interaction is not important

#7.2 two way anova model
#7.3 strategy for the analysis of two way anova studies
#step1 test if the interaction is significant
# if interaction is not significant, drop interaction, refit the model, check diagnostics, use pairwise comparisons on main effects
# if interaction is significant, check diagnostics, use pairwise comparisons on interaction effect
#7.3.1 example in R
diet.avo1<-aov(LOSS~JOGGING+DIET+JOGGING*DIET, data=diet.df, 
               contrasts = list(JOGGING="contr.sum", DIET="contr.sum"))
summary(diet.avo1) # p value JOGGING:DIET = 0.227 drop

diet.aov2<-aov(LOSS~JOGGING+DIET, data=diet.df, 
               contrasts = list(JOGGING="contr.sum", DIET="contr.sum"))
summary(diet.aov2)
#7.4 diagnostics
#7.4.1 checking homogeneity of variances
leveneTest(LOSS~JOGGING, data=diet.df) # p value = 0.2151 > 0.05 
leveneTest(LOSS~DIET, data=diet.df) # 0.7669>0.05
#in case levene's test is rejected, we can also robust analysis
Anova(diet.aov2, type="III", white.adjust="hc3") #robust analysis also indicated that JOGGING and DIET are significant

#7.4.2 checking normality of residuals
rs<-rstandard(diet.aov2)
shapiro.test(rs) # p-value = 0.699 > 0.05 residuals are normally distributed
hist(rs)
plot(density(rs))
#7.4.3 influential observations
plot(cooks.distance(diet.aov2))

diet.df[cooks.distance(diet.aov2)>0.3,]
diet.df.small<-diet.df[-22,]
loss.small<-diet.df.small$LOSS
jogging.small<-diet.df.small$JOGGING
diet.small<-diet.df.small$DIET

diet.aov1.small<-aov(loss.small~jogging.small+diet.small+jogging.small*diet.small, 
                     contrasts = list(jogging.small="contr.sum", diet.small="contr.sum"))

summary(diet.aov1.small) # drop interaction 

diet.aov2.small<-aov(loss.small~jogging.small+diet.small, 
                     contrasts = list(jogging.small="contr.sum", diet.small="contr.sum"))
summary(diet.aov2.small) # keep obersvation 22 in the data

#7.5 multiple comparisons for the main effects (in case interaction is not significant)
library(agricolae)
library(sp)

out<-HSD.test(diet.aov2, "JOGGING", group=F)
out$means
out$comparison

out1<-HSD.test(diet.aov2, "DIET", group = F)
out1$means
out1$comparison

#7.6 two-way anova when cells have unequal sample size
training<-read.table(file=file.choose(), header=T)
head(training)
names(training)
#7.6.2 descriptive 
by_method_sepperiod<-training%>%group_by(Method, Sep_Period)%>%
  summarise(mean=mean(score, na.rm=T), sd=sd(score, na.rm=T), n=n())
by_method_sepperiod
interaction.plot(training$Sep_Period, training$Method, training$score, type="b", pch=c(1,2,3), col=c(1,2,3))

#7.6.3 anova table
# for unbalanced design, we use a regression approach
training.lm<-lm(score~Method+Sep_Period+Method*Sep_Period, data=training, contrasts = 
                  list(Method="contr.sum", Sep_Period="contr.sum"))
Anova(training.lm, type="III")

#check homogeneity 
leveneTest(score~Method*Sep_Period, data=training) #p=0.8661>0.05 
#check normality within groups
shapiro.test(training.lm$residuals) #p=0.2985 > 0.05
#cooks distance
plot(cooks.distance(training.lm))

#pairwise comparisons of treatment effects
head(training)
method_sep_period<-1:length(training$score)
for(i in 1:length(training$score))
{
  method_sep_period[i]<-paste(substring(training$Method[i], 1, 4),
                              training$Sep_Period[i], sep="")  
}
new_df<-data.frame(scre=training$score, method_sep_period)
head(new_df)

descriptive<-describeBy(new_df$scre, new_df$method_sep_period, mat=T)
describe.st<-subset(describe, select=c("group1","n","mean","sd","median", "min", "max"))
describe.st
#one-way anova
new.df.aov<-aov(scre~method_sep_period, new_df, 
                contrasts = list(method_sep_period="contr.sum"))
summary(new.df.aov)
out2<-HSD.test(new.df.aov, "method_sep_period", group=F)
out2$comparison

#chap10logisticregression
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
#sumlevel
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

#chap11poisson
#1 introduction
#poisson regression is used when the response variable is a count of something per unit or per time interval
#poisson regression model assumes that the response variable Y has a poisson distribution(Y~poisson(lambda))
#µ=E(Y)=lambda
#sigma^2=lambda
crab<-read.table(file=file.choose(), header = T)
head(crab)
names(crab)
hist(crab$Sa, breaks=15)
list(mean=mean(crab$Sa), var=var(crab$Sa))

#illustration in R
model0<-glm(crab$Sa~1, family = poisson(link=log))
summary(model0)
model1<-glm(crab$Sa~crab$W, family = poisson(link=log))
summary(model1)
exp(model1$coefficients) #for one unit of increase in the width, the expected number of satellites will increase 
# and it will be multiplied by 1.8
#visualize the poisson model
pred<-fitted(model1)
plot(crab$W, crab$Sa, col=4)
points(crab$W, pred, col=3)

#how good is the fit
anova(model1)
anova(model0, model1, test="Chisq") #model1 is significantly better than the intercept-only model
#if the model fits the data well, the ratio of the deviance to DF deviance/DF, should be one
#ratio 560.84/171=3.2 much larger than 1 
#indicates the overdispersion. overdispersion means that oberseved variance is larger than the assumed variance



































































