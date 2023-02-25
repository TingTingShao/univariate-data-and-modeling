lab.df<-read.table(file="/Users/shaotingting/Documents/univariate_data_and_modeling/exercises/data_exercise/LAB.DAT", header = TRUE)
head(lab.df)
#a
#a.1
#b
library(dplyr)
lab_Mavg<-mutate(lab.df,
                 Mavg=(M1+M2+M3+M4)/4)
head(lab_Mavg)
#c
lab_Mavg_diff<-mutate(lab_Mavg,
                 diff=Target-Mavg)
head(lab_Mavg_diff)
#d.1 balanced

#d.2
plt1<-interaction.plot(lab_Mavg_diff$Lab, lab_Mavg_diff$Rep, 
                       lab_Mavg_diff$diff, type='b', pch=c(18,24), col=c(1,2))
plt1
#d.3
lab<-as.factor(lab_Mavg_diff$Lab)
rep<-as.factor(lab_Mavg_diff$Rep)
diff<-lab_Mavg_diff$diff
lab.aov1<-aov(diff~lab+rep+lab*rep, contrast=list(lab="contr.sum", rep="contr.sum"))
summary(lab.aov1)#interactive term is not significant
                 #drop interactive term, rerun
lab.aov2<-aov(diff~lab+rep, contrast=list(lab="contr.sum", rep="contr.sum"))
summary(lab.aov2)
#d4
#d4.1 check homogeneity
library(car)
leveneTest(diff~lab)#p value<0.05 
df<-data.frame(cbind(diff, rep, lab)) 
par(mfrow=c(1,2))
boxplot(diff~rep, col=2, data=df)
boxplot(diff~lab, col=3, data=df)
df%>%group_by(lab, rep)%>%
  summarise(mean=mean(diff, na.rm=T), 
            var=var(diff, na.rm=T), n=n())%>%
  select(lab, rep, mean, var, n)
Anova(lab.aov2, type="III", white.adjust="hc3")#is significant move on
leveneTest(diff~rep)
#d4.2 check normality of residuals
shapiro.test(lab.aov2$residuals)
hist(lab.aov2$residuals) #not normally distributed ####use non paramatric method
#d4.3 check cooks distance
plot(cooks.distance(lab.aov2))#no need to remove any value balanced
#d5
library(agricolae)
library(sp)
out<-HSD.test(lab.aov2, "rep", group=FALSE)
out$means
out$comparison
out1<-HSD.test(lab.aov2, "lab", group=FALSE)
out1$comparison
out1$means
#d5 no we can not use Tukey test because normality is rejected
sublab<-df[, c('diff', 'rep')]
library("doBy")
sortsublab<-orderBy(~diff, data=sublab)
head(sortsublab)
kruskal.test(diff~rep, data=sortsublab) #p<0.05, rep parameter are not the same
#multiple comparison
install.packages("pgirmess")
library(pgirmess)
kruskalmc(diff~rep, data=sortsublab)

sublab<-df[, c('diff', 'lab')]
library("doBy")
sortsublab1<-orderBy(~diff, data=sublab)
head(sortsublab1)
kruskal.test(diff~lab, data=sortsublab1) #p<0.05, rep parameter are not the same
#multiple comparison
library(pgirmess)
kruskalmc(diff, lab, data=sortsublab1)








#d6 H