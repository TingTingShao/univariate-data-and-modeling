#multiple comparison for the main effects
library(agricolae)
library(sp)
diet.aov2<-aov(LOSS~JOGGING+DIET, data=diet, contrast=list(JOGGING="contr.sum", DIET="contr.sum"))
summary(diet.aov2)
#Since there is no interaction between JOGGING AND DIET, multiple comparison methods such as bon, holm's
#for multiple comparison. change into one way anova

training<- read.table(file = "/Users/shaotingting/Documents/univariate_data_and_modeling/Data/data_for_anova/training.txt",header = TRUE )
head(training)
names(training)
#1 descriptive statistics
description<-training%>%
  group_by(Method, Sep_Period)%>%
  summarise(
    avg=mean(score),
    sd=sd(score),
    number=n()
  )
description
#2 model
training.aov1<-aov(score~Method+Sep_Period+Method*Sep_Period, data=training, contrast=list(Method="contr.sum", Sep_Period="contr.sum"))
summary(training.aov1)
#3 diagnostics
#3.1 homogeneity
leveneTest(score~Method*Sep_Period, data=training)
#3.2 normality
shapiro.test(training.aov1$residuals)
#3.3 cooks.distance
plot(cooks.distance(training.aov1))
#4 pairwise comparisons of treatment effects
#4.1 convert into one way anova, by creating a new variable with the interaction term
#4.1.1 new variable
method_sep_period<-character(length(training$score))
#4.1.2 store value in new variable
for (i in 1: length(training$score))
{method_sep_period[i]<-paste(substring(training$Method[i], 1, 4), training$Sep_Period[i], sep="")}
#4.1.3 create a new data frame
new_df<-data.frame(score=training$score, method_sep_period)
head(new_df)
#ano
new_df.aov<-aov(score~method_sep_period, new_df, contrasts=list(method_sep_period="contr.sum"))
summary(new_df.aov)
#multiuple comparison test










