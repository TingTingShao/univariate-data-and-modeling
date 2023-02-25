boston<-read.table(file="/Users/shaotingting/Documents/univariate_data_and_modeling/course_notes_R_scripts/example_of_previous_exam/boston2(1).txt", header = TRUE)
head(boston)
names(boston)
boston$internatfact<-as.factor(boston$internatfact)
#1
library(dplyr)
X1<-as.factor(rep(0,nrow(boston)))
X2<-X1
X3<-X1
X1<-ifelse(boston$internatfact=='low', 1, 0)
X2<-ifelse(boston$internatfact=='medium', 1, 0)
X3<-ifelse(boston$internatfact=='high', 1, 0)
boston_dummy<-mutate(boston, X1, X2, X3)
head(boston_dummy)
lm1<-lm(rm~X1+X2+X3, data=boston_dummy)
summary(lm1)
head(model.matrix(lm1))




