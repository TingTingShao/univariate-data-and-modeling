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





































