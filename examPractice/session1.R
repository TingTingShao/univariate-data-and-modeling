#1.1
beta.df<-read.table(file=file.choose(), header=T)
head(beta.df)
#1.2
names(beta.df)

#2.a
chick.df<-read.table(file=file.choose(), header=T)
#2.b
names(chick.df)
#2.c
summary(chick.df$weight)
var(chick.df$weight)
sd(chick.df$weight)
#2.d
# This is nonsense, as these numbers are simply labels
#2.e
names(chick.df)[1]<-"No."
names(chick.df)
chick.df$No.<-as.factor(chick.df$No.)
#2.f
by(chick.df$weight, chick.df$feed, summary)
#2.g
table(chick.df$feed)

#3.a
monica.df<-read.table(file=file.choose(), header=T, sep=";")
head(monica.df)
#3.b
by(monica.df$age, monica.df$sex, summary)
by(monica.df$age, monica.df$sex, var)
by(monica.df$age, monica.df$sex, sd)
#3.c
boxplot(monica.df$age~monica.df$sex)
#3.d
par(mfrow=c(1,2))
by(monica.df$age, monica.df$sex, boxplot)
#3.e
#fage<-monica.df%>%filter(sex=="m")%>%select(age)
denf<-density(monica.df$age[which(monica.df$sex == 'f')])
plot(denf, main="females")
denm<-density(monica.df$age[which(monica.df$sex=="m")])
plot(denm, main="males")
#another way to plot density
hist(monica.df$age[which(monica.df$sex == 'f')], probability = T)
lines(denf)



