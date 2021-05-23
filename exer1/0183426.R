#8
library(ISLR)
attach(Auto)
lm.fit = lm(mpg ~ horsepower)
summary(lm.fit)

predict(lm.fit, data.frame(horsepower=c(98)), interval="confidence")
predict(lm.fit, data.frame(horsepower=c(98)), interval="prediction")

plot(horsepower, mpg)
abline(lm.fit)

par(mfrow=c(2,2))
plot(lm.fit)

#9
pairs(Auto)

cor(subset(Auto, select=-name))

lm.fit1 = lm(mpg~.-name, data=Auto)
summary(lm.fit1)

par(mfrow=c(2,2))
plot(lm.fit1)
plot(predict(lm.fit1), rstudent(lm.fit1))

lm.fit2 = lm(mpg~cylinders*displacement+displacement*weight)
summary(lm.fit2)

lm.fit3 = lm(mpg~log(weight)+sqrt(horsepower)+acceleration+I(acceleration^2))
summary(lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit3)
plot(predict(lm.fit3), rstudent(lm.fit3))
lm.fit2<-lm(log(mpg)~cylinders+displacement+horsepower+weight+acceleration+year+origin,data=Auto)
summary(lm.fit2)
par(mfrow=c(2,2)) 
plot(lm.fit2)
plot(predict(lm.fit2),rstudent(lm.fit2))

#10
library(ISLR)
summary(Carseats)
attach(Carseats)
lm.fit = lm(Sales~Price+Urban+US)
summary(lm.fit)
lm.fit2 = lm(Sales~Price+US)
summary(lm.fit2)
confint(lm.fit2)
plot(predict(lm.fit2), rstudent(lm.fit2))
par(mfrow=c(2,2))
plot(lm.fit2)