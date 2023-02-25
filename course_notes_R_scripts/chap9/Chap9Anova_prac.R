#descriptive statics #1
install.packages("psych")
library(psych)
pollution<-pollutie2
head(pollution)
rain<-pollution$Rain
region<-pollution$regio
describe<-describeBy(rain, region, mat=TRUE)
describe.st<-subset(describe, select=c("group1", "mean", "sd"
                                       , "median", "min", "max"))
describe.st
boxplot(rain~region, col=2, names=levels(pollution$regio))
#descriptive statics #2
library(tidyverse)
by_region<-pollution %>%
  group_by(regio)%>%
  summarise(
    Avgrain<-mean(Rain)
  )
by_region
#model1
glm1<-lm(Rain~regio, data=pollution)
summary(glm1) #p<0.05, not all means are the same
#model2
glm2<-lm(Rain~regio, data=pollution, contrasts = list(regio="contr.sum"))
summary(glm2)
head(model.matrix(glm2))

#model diagnostic
#check homogeneity
library(car)
leveneTest(rain~region)
#check normaility of the within group residuals
shapiro.test(glm1$residuals) #p,0.05, H0 was rejected
hist(glm1$residuals) #obtain histogram, which is symmetric
#check for influential observations
plot(cooks.distance(glm1)) #there is not influential observations





