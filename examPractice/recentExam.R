#buying a new car
carp<-car_purchase_1_
head(carp)
#logistic modelA E(Y=1)=exp(ß0+ß1X1+ß2X2+ß3X1X2)/1+exp(ß0+ß1X1+ß2X2+ß3X1X2)
glm.log.M1<-glm(carp$Y~carp$X1+carp$X2+carp$X1*carp$X2, family = binomial(link=logit))
summary(glm.log.M1)
#logistic modelB E(Y=1)=exp(ß0+ß1X1)/1+exp(ß0+ß1X1)
#H0: ß2=ß3=0 H1: not both ß2 ß3 = 0
glm.log.M2<-glm(carp$Y~carp$X1, family = binomial(link=logit))
summary(glm.log.M2)
anova(glm.log.M2, glm.log.M1, test="Chisq") #p value=0.1422<0.05, can not reject H0, X2 and X1*X2 should not be added to the model
                                            # as it won't improve the model
#movie data
library(readxl)
movies_sml_5_ <- read_excel("Documents/univariate_data_and_modeling/course_notes_R_scripts/recent_exam/movies_sml(5).xlsx")
movie<-movies_sml_5_
head(movie)
action<-subset(movie$length, movie$Action=="1")
head(action)
length(action)
nonaction<-subset(movie$length, movie$Action=="0")
head(nonaction)
length(nonaction)
#H0: length action - length nonaction = 17 vs H1: length action - length nonaction > 17
#both>25, no need to check normality
#check homogeneity
var.test(action, nonaction) #p-value = 0.007382, variance is not equal
t.test(action, nonaction, mu=17, var.equal = F, alternative="greater") #p-value = 0.4486
                                                                      # can not reject H0
                                                                      # difference in mean is not greater than 17

n<-9
delta<-0.3 *(0.2*1.5)
sd<-0.44
sig.level<-0.05
power.t.test(n=n, delta = delta, sd=sd, sig.level = sig.level, power=NULL, 
             type="one.sample", alternative = "one.sided")
power.t.test(n=n, delta = delta, sd=sd, sig.level = sig.level, power=NULL, 
             type="one.sample", alternative = "two.sided")
#one sided>two sided









