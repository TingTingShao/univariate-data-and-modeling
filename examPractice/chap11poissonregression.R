#1 introduction
#poisson regression is used when the response variable is a count of something per unit or per time interval
#poisson regression model assumes that the response variable Y has a poisson distribution(Y~poisson(lambda))
#µ=E(Y)=lambda
#sigma^2=lambda
crab<-read.table(file=file.choose(), header = T)
head(crab)
names(crab)
hist(crab$Sa, breaks=15)
list(mean=mean(crab$Sa), var=var(crab$Sa))

#illustration in R
model0<-glm(crab$Sa~1, family = poisson(link=log))
summary(model0)
model1<-glm(crab$Sa~crab$W, family = poisson(link=log))
summary(model1)
exp(model1$coefficients) #for one unit of increase in the width, the expected number of satellites will increase 
                        # and it will be multiplied by 1.8
#visualize the poisson model
pred<-fitted(model1)
plot(crab$W, crab$Sa, col=4)
points(crab$W, pred, col=3)

#how good is the fit
anova(model1)
anova(model0, model1, test="Chisq") #model1 is significantly better than the intercept-only model
#if the model fits the data well, the ratio of the deviance to DF deviance/DF, should be one
#ratio 560.84/171=3.2 much larger than 1 
#indicates the overdispersion. overdispersion means that oberseved variance is larger than the assumed variance














