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

#exercise session2 distributions and confidence interval

#1.a frequency
#1.b discrete
#1.c
dice<-1:6
dice.p<-rep(1/6, 6)
dice.c<-rep(1/6, 6)
dice.c
for(i in 2:length(dice.p)){
  dice.c[i]<-dice.c[i-1]+dice[i]
}
barplot(dice.p, names.arg = c("1", "2", "3", "4", "5", "6"))
axis(2, at=c(0, 1/6), labels=c("0", "1/6"), lty=1)

plot(dice, dice.c, type="n", main="cumulative distribution function", xlab="",
     ylab="Fx", axes=FALSE)
lines(dice, dice.c, type="s")
axis(2, at=c(0, dice.c), 
     labels=c("0", "1/6", "1/3", "1/2", "2/3", "5/6", "1"))
axis(1, at=dice, lty=1)

#exercise2
#2.d
prob<-pbinom(6, 10, 0.5)
round(prob, 2)
#2.e
1-round(prob,2)
#2.f
qbinom(0.5, 10, 0.5)
#2.g
qbinom(0.75, 10, 0.5)

#exercise 3
#3.a
#poisson distribution
#3.c
ppois(0, 2)
#3.d
1-ppois(0,2) # or equivalent
ppois(0,2, lower.tail = F)
#3.e
ppois(0, 2*5)
#3.f
range<-seq(0,16,1)
density<-dpois(range, 4)
barplot(density)

#4.a
#continuous
#normal ditribution
#4.c
pnorm(100, mean=85, sd=sqrt(500))
#4.d
pnorm(100, mean=85, sd=sqrt(500))
#4.e
pnorm(80, mean=85, sd=sqrt(500))
#4.f
1-pnorm(60, mean=85, sd=sqrt(500)) 
#4.g
s.d<-sqrt(500)
x.60<-60
lim<-qnorm(0.975, 85, s.d)
lim.u<-170
lim.l<-0
x<-seq(lim.l, lim.u, length=1000)
i.60<-x>=60 & x<= lim.u
hx<-dnorm(x, mean=85, sd=s.d)
plot(x, hx, type="n", xlab="", ylab="",
     main=expression(mu~"=85kg, " ~sigma~"= 22.36kg"), 
     axes=F)
lines(x, hx)
polygon(c(x.60, x[i.60], lim.u), c(0, hx[i.60], 0), col="green")
result<-"P(x>60)"
mtext(result, 3)
axis(1, at=c(lim.l, x.60, 85, lim.u), pos=0)
#4.h
qnorm(0.975, 85, s.d)

#5.a
pt(1, 15)
1-pt(1,15)
#5.b
qt(1-0.05, 15)
#5.c
x.95 <- round(qt(0.95, 15), 2)
x.95
x.1.75<-1.75
lim.u<-4
lim.l<--4
x<-seq(lim.l, lim.u, length=1000)
i.1.75<-x>=x.1.75 & x<= lim.u
hx<-dt(x, 15)
plot(x, hx, type="n", xlab="", ylab="",
     axes=F)
lines(x, hx)
polygon(c(x.1.75, x[i.1.75], lim.u), c(0, hx[i.1.75], 0), col="green")
result<-"P(x>1.75)"
mtext(result, 3)
axis(1, at=c(lim.l, 0, x.1.75, lim.u), pos=0)

#6.a
qchisq(0.95, 10)

#7.a
pf(10, 4, 9)-pf(5, 4, 9)
#7.b
qf(0.95, 4, 9)

#8.a
confidence_interval<-0.95
alpha<-1-confidence_interval
sigma<-0.3
n<-10
mean<-41.924
lcl<-mean-qnorm(1-alpha/2)*sigma/sqrt(n)
lcl
ucl<-mean+qnorm(1-alpha/2)*sigma/sqrt(n)
ucl
list(mean=mean, lcl=lcl, ucl=ucl)
#8.b
zsum.test(mean.x = mean, n.x=10, sigma.x = sigma, mu=41.924, conf.level = 0.95)

#9.a
tsum.test(mean.x = 750, s.x = 30, n.x = 20, conf.level = 0.95)

#10
blood<-read.table(file=file.choose(), head=T)
head(blood)
names(blood)
#10.a
library(BSDA)
z.test(blood$age, sigma.x = 5, conf.level = 0.9)
#10.b
t.test(blood$prolactn, conf.level = 0.95)
#10.c
agesub<-subset(blood, 50<age & age<60, select=age)
n<-length(agesub$age)
n
m<-length(blood$age)
n/m
#10.d
prop.test(n, m, conf.level = 0.95)
#10.e
subset.df<-subset(blood, age<50)
head(subset.df)
#10.f
shapiro.test(subset.df$testost) #p = 0.7296 >0.05, can not reject H0, normally distributed
t.test(subset.df$testost, conf.level = 0.99) # [14.07455 38.92545]








