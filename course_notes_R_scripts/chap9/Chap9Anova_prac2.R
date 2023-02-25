#multiple comparison
#1
pollution<-pollutie2
poll.aov1<-aov(Rain~regio, data=pollution)
summary(poll.aov1) #p<0.05, not all pairs are the same

#2
#planned comparison whether the mean amount of rain in region C and region NO are teh same or not
#pooled estimator of the variance is used
rain<-pollution$Rain
region<-pollution$regio
pairwise.t.test(rain, region, p.adjust.method = "none")#p>0.05, there is no difference in NO and C

#3
#multiple comparison
#3.1Turkey HSD test
diffs<-TukeyHSD(poll.aov1, whih = "region", conf.level = 0.95)
diffs
plot(diffs) #if 0 is not included in 95%interval, significant

#3.2 scheffe.test
install.packages("agricolae")
library(agricolae)
scheffe.test(poll.aov1, "regio", group=FALSE)$comparison
hgroups.scheffe <- scheffe.test(poll.aov1, "regio", group=TRUE)
hgroups.scheffe
#3.3 Bonferroni method
pairwise.t.test(rain, region, p.adj="bonferroni")
#holm's apprroach
pairwise.t.test(rain, region, p.adj="holm")








