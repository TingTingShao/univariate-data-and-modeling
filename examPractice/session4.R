#1
body<-read.table(file=file.choose(), header = T)
head(body)
names(body)
#1.a
names<-names(body[,2:16])
summary(body[,2:16])
apply(body[, 2:16], 2, boxplot)
density<-apply(body[, 2:9], 2, density)
n<-length(density)
for (i in 1:n) {
  plot(density[[i]])
}

#1.b
X1<-ifelse(body$Sex=="Male", 0, 1)
X1<-as.factor(X1)
body.dummy<-cbind(body, X1)
#quality=a+bX+cX1 OR quality=a+bX+cX1+d(X*X1)

#1.c
body.s<-sample(1:nrow(body), 50, replace=FALSE)
body.model<-body[-body.s, ]
body.new<-body[body.s,]
#1.d
names(body.model)
res.lm<-lm(Weight~Height+Shoulder+Chest+Waist+Abdo+Hip+Thigh+Bicep+
         Forearm+Knee+Calf+Ankle+Wrist+Age, data=body.model)
summary(res.lm)
par(mfrow=c(3,5))
for (i in 2:16) {
  rs<-rstandard(res.lm)
  plot(rs~body.model[, i], main=names(body.model)[i])
}
#
res.lm1<-lm(Weight~Height+Shoulder+Chest+Abdo+Hip+Thigh+Bicep+
             Forearm+Knee+Calf+Ankle+Wrist+Age, data=body.model)
summary(res.lm1)
res.lm2<-lm(Weight~Height+Shoulder+Chest+Abdo+Hip+Thigh+Bicep+
              Forearm+Knee+Calf+Ankle+Age, data=body.model)
summary(res.lm2)
res.lm3<-lm(Weight~Height+Shoulder+Chest+Abdo+Hip+Thigh+
              Forearm+Knee+Calf+Ankle+Age, data=body.model)
summary(res.lm3)
res.lm4<-lm(Weight~Height+Shoulder+Chest+Abdo+Hip+Thigh+
              Forearm+Knee+Calf+Ankle, data=body.model)
summary(res.lm4)
res.lm5<-lm(Weight~Height+Shoulder+Chest+Abdo+Hip+Thigh+
              Forearm+Knee+Calf, data=body.model)
summary(res.lm5)
slm.forward<-step(lm(Weight~1, data=body.model), scope=~Height+Shoulder+Chest+Abdo+Hip+Thigh+
                    Forearm+Knee+Calf, direction="forward", data=body.model)

res.lm6<-lm(Weight~Chest + Knee + Hip + Height + Calf + Forearm + Abdo + 
              Shoulder + Thigh, data=body.model)

fit<-fitted(res.lm6)
shapiro.test(fit)
rs<-rstandard(res.lm6)
hist(rs)
plot(rs~fit)
library(leaps)
leaps(x=body.model[, 3:16], y=body.model[, 2], method="Cp")
names(body.model)










