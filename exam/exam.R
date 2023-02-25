library(readxl)
car <- read_excel("Documents/univariate_data_and_modeling/exam/car.xlsx")
head(car)
names(car)
Education_New <- read_excel("Documents/univariate_data_and_modeling/exam/Education_New.xlsx")
head(Education_New)
names(Education_New)
orange <- read_excel("Documents/univariate_data_and_modeling/exam/orange(3).xlsx")
head(orange)
names(orange)
auteurs2 <- read_excel("Documents/univariate_data_and_modeling/exam/auteurs2.xlsx")
head(auteurs2)
names(auteurs2)
auteurs <- read_excel("Documents/univariate_data_and_modeling/exam/auteurs.xlsx")
head(auteurs)
names(auteurs)

confidence_level<-0.95
n<-4
lcl<-80
ucl<-120
alpha<-1-confidence_level
alpha
m<-qnorm(1-alpha/2)
m
n<-sqrt(n)
diff<-ucl-lcl
sigma<-diff*n/(2*m)
sigma
40/m

auteurs2 <- read_excel("Documents/univariate_data_and_modeling/exam/auteurs2.xlsx")
head(auteurs2)
names(auteurs2)
auteurs <- read_excel("Documents/univariate_data_and_modeling/exam/auteurs.xlsx")
head(auteurs)
names(auteurs)

unique(auteurs2$writer)
glm1<-lm(age~writer, data=auteurs2)
summary(glm1)
aov1<-aov(age~writer, data=auteurs2)
summary(aov1)
library(car)
leveneTest(age~writer, data=auteurs2)
shapiro.test(glm1$residuals)
hist(glm1$residuals)
plot(density(glm1$residuals))
kruskal.test(age~writer, data=auteurs2)
kruskalmc(age~writer, data=auteurs2)
tapply(auteurs2$age, auteurs2$writer, mean)

pairwise.t.test(auteurs2$age, auteurs2$writer, p.adjust.method = "holm")
pairwise.t.test(auteurs2$age, auteurs2$writer,p.adjust.method = "bonferroni")

tapply(auteurs2$age, auteurs2$writer, sd)
tapply(auteurs2$age, auteurs2$writer, median)
boxplot(age~writer, data=auteurs2)

orange <- read_excel("Documents/univariate_data_and_modeling/exam/orange(3).xlsx")
head(orange)
names(orange)

glucose<-orange$Glucose
fructose<-orange$Fructose
mean(glucose)
mean(fructose)
shapiro.test(glucose)
shapiro.test(fructose)
sd(glucose)
sd(fructose)
var.test(glucose, fructose)
t.test(fructose, glucose, mu=1.5, var.equal = T, alternative="greater")

x<-length(glucose)
x
y<-length(fructose)
y
delta<-2.5
sd<-0.9
sig.level<-0.05
power.t.test(n=c(6,6), delta = delta, sd=sd, sig.level = sig.level, power=NULL, type="two.sample", alternative = "one.sided")

power.t.test(n=NULL, delta = delta, sd=sd, sig.level = sig.level, power=0.8, type="two.sample", alternative = "one.sided")

Education_New <- read_excel("Documents/univariate_data_and_modeling/exam/Education_New.xlsx")
head(Education_New)
names(Education_New)

#Education_New$Covid<-as.factor(Education_New$Covid)
lm2<-lm(September~Hours_Math+Covid+I(Hours_Math*Covid), data=Education_New)
summary(lm2)
26.7163-6.6048

unique(Education_New$Advice_teacher)

sub6<-subset(Education_New, Education_New$Hours_Math=="6" & Education_New$Advice_teacher=="Complete_pos")
sub8<-subset(Education_New, Education_New$Hours_Math=="8" & Education_New$Advice_teacher=="Complete_pos")

total6<-subset(Education_New, Education_New$Hours_Math=="6")
ntotal6<-nrow(total6)
ntotal6

total8<-subset(Education_New, Education_New$Hours_Math=="8")
ntotal8<-nrow(total8)
ntotal8

n6<-nrow(sub6)
n8<-nrow(sub8)
n8
n6
prop.test(n6, ntotal6)

prop.test(n8, ntotal8)

x<-c(n8, n6)
n<-c(ntotal8, ntotal6)
x
n
prop.test(x,n, alternative="greater")

1+1.75+3.5+2.25+1.5



