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













