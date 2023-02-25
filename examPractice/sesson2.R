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



