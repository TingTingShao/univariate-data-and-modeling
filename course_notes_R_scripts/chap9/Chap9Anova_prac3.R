#two way anova
diet<-read.table(file = "/Users/shaotingting/Documents/univariate_data_and_modeling/Data/data_for_anova/diet.txt", header = TRUE)
head(diet)
names(diet)
#1
description<-diet%>%
  group_by(DIET, JOGGING)%>%
  summarise(
    avg=mean(LOSS),
    SD=sd(LOSS),
    number=n()
  )
description
boxplot(LOSS~JOGGING, col=2, data=diet)
boxplot(LOSS~DIET, col=3, data=diet)
#interaction plot
diet.f<-as.factor(diet$DIET)
jogging.f<-as.factor(diet$JOGGING)
loss<-diet$LOSS
interaction.plot(diet.f, jogging.f, loss, type="b", pch=c(18, 24, 22), col=c(1,2,3))
#2
#two way anova, starting from interaction model
diet.aov1<-aov(LOSS~JOGGING+DIET+JOGGING*DIET, data=diet, contrast=list(JOGGING="contr.sum", DIET="contr.sum"))
summary(diet.aov1) #drop interactive, rerun, formulate the model LOSS~JOGGING+DIET
diet.aov2<-aov(LOSS~JOGGING+DIET, data=diet, contrast=list(JOGGING="contr.sum", DIET="contr.sum"))
summary(diet.aov2)
#3
#Diagnostics
#3.1 homogeneity
leveneTest(LOSS~JOGGING, data=diet)
leveneTest(LOSS~DIET, data=diet) #p>0.05 
#3.2 normality
shapiro.test(diet.aov2$residuals)
hist(diet.aov2$residuals)
#3.3cooks.distance
plot(cooks.distance(diet.aov2)) #delete point>0.4, rerun
































