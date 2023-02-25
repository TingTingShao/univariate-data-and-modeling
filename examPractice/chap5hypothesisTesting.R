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
#inference for two independent smaples: t test
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

































