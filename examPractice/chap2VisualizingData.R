#categorical data: tables bar plots, and pie charts
#continuous data histograms and boxplots
temperature<-read.table(file=file.choose(), head=T)
head(temperature)
names(temperature)
#2 categorical variables
#2.1 frequency table
area.freq<-table(temperature$Area)
area.freq
xtabs(~temperature$Area)
#relative frequencies
area.Rfreq<-table(temperature$Area)/nrow(temperature)
round(area.Rfreq, 2)
library(gmodels)
CrossTable(temperature$Area, digit=2, prop.r = F, prop.c = F, prop.chisq = F)

#2.2 bar charts
par(mfrow=c(1,2))
bp.f<-barplot(area.freq, xlab="Area", ylab="absolute frequency", ylim=c(0,10), col=3)
bp.Rf<-barplot(area.Rfreq, xlab="Area", ylab="relative frequency", ylim=c(0, 0.3), col=3)
barplot(sort(area.freq, decreasing=T))
barplot(sort(area.Rfreq))

#2.3 pie chart
pp.f<-pie(area.freq)
pie(area.freq, main = "Area")
pie(area.freq, main="Area", col=rainbow(length(area.freq)))

#2.4 dot chart
area.df<-data.frame(area.freq)
dp.f<-dotchart(area.df$Freq, labels=area.df$Var1)
areaR.df<-data.frame(area.Rfreq)
dotchart(areaR.df$Freq, labels = areaR.df$Var1)

#3 continuous variables
#3.1 histograms
hist(temperature$annual)
hist(temperature$annual, probability = T)
lines(density(temperature$annual))
#3.2 boxplots
par(mfrow=c(1,2))
boxplot(temperature$annual, col=11)
boxplot(temperature$annual~temperature$Area, col=12)
#3.3 scatter plot
plot(temperature$annual~temperature$Latitude, col=2, cex=2)
plot(temperature)























