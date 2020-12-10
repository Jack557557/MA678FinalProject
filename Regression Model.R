data<-read.csv("data.csv")


#simple linear regression
fit1<-lm(log(price)~factor(bedrooms)+factor(bathrooms)+log(sqft_living)+log(sqft_lot)+factor(floors)+factor(view)+factor(condition)+age+log(income)+factor(ES)+factor(HS)+factor(MS),data = data)
summary(fit1)
par(mfrow = c(2,2))
plot(fit1)

library(lme4)

#Multilevel linear model

fit2<-lmer(log(price)~log(sqft_living)+log(sqft_lot)+ES+HS+bathrooms+view+condition+(1|statezip7),data = data)
summary(fit2)

fit3<-lmer(log(price)~log(sqft_living)+log(sqft_lot)+ES+HS+bathrooms+view+condition+(ES|statezip7),data = data)
summary(fit3)

fit4<-lmer(log(price)~log(sqft_living)+log(sqft_lot)+ES+HS+MS+bedrooms+bathrooms+view+condition+(1|statezip7),data = data)
summary(fit3)
