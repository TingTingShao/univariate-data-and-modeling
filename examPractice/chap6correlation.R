temp<-read.table(file=file.choose(), head=T)
head(temp)
temp_small<-select(temp, c("Latitude", "Longitude", "annual"))
pairs(temp_small)
#2 pearson correlation coefficient
#2.1 definition
    # indication if there is a linear relationship, r always has a value between -1 and +1
    # r<0: negative linear trend
    # r>0: positive linear trend
    # r=0: no LINEAR relation
#2.3 population correlation coefficient
    #step1: hypothesis
        #H0: p=0 versus H1: p≠0
    #step2: alpha=0.05
    #step3: test
#2.4 correlation coefficients in R
cor(temp_small)
cor.test(temp_small$annual, temp_small$Latitude) # cor = -0.903 very strong association
cor.test(temp_small$annual, temp_small$Longitude) #cor = -0.477 association is less stronger

#3 spearman correlation coefficient
    #non-parametric alternative to pearson correlation coefficient
shapiro.test(temp_small$annual) #p=0.03879 <0.05, normality can not hold
cor(temp_small, method="spearman")
cor.test(temp_small$annual, temp_small$Latitude, method="spearman") 
cor.test(temp_small$annual, temp_small$Longitude, method="spearman") 
N











