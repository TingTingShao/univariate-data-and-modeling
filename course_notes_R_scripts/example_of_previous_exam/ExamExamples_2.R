boston<-read.table(file="/Users/shaotingting/Documents/univariate_data_and_modeling/course_notes_R_scripts/example_of_previous_exam/boston2(1).txt", header = TRUE)
head(boston)
names(boston)
boston$internatfact<-as.factor(boston$internatfact)
#1
lm1<-lm(rm~internatfact, data=boston)
summary(lm1) #p value <0.05, reject H0, the average number of room in these four types 
             #of districts are not the same
#2
res.lm2<-lm(crim~dis, data=boston)
summary(res.lm2)
crim<-boston$crim
dis<-boston$dis
plot(crim~dis)
abline(res.lm2)
fit<-fitted(res.lm2)
rs<-rstandard(res.lm2) 
plot(rs~fit)
plot(dis~fit)#residual is not randomly distributed (normality)
res.lm3<-lm(crim~I(dis^2))
summary(res.lm3)
fit<-fitted(res.lm3)
rs<-rstandard(res.lm3) 
plot(rs~fit)
plot(dis~fit)


#3
##prefer model2
