#第八题a
library(ISLR)
Auto = na.omit(Auto)
attach(Auto)
lm.fit = lm(mpg ~ horsepower)
summary(lm.fit)
predict(lm.fit, data.frame(horsepower=c(98)), interval="confidence")#置信区间
predict(lm.fit, data.frame(horsepower=c(98)), interval="prediction")#预测区间
#8.(b)
plot(horsepower, mpg)
abline(lm.fit,col="red")
#8.(c)
par(mfrow=c(2,2))
plot(lm.fit)

#第九题a
pairs(Auto)
#9(b)
cor(subset(Auto, select=-name))
#9(c)
lm.fit1 = lm(mpg~.-name, data=Auto)
summary(lm.fit1)

#9(d)
par(mfrow=c(2,2))
plot(lm.fit1)
plot(predict(lm.fit1), rstudent(lm.fit1))

#9(e)
lm.fit2 = lm(mpg~cylinders*displacement+displacement*weight)
summary(lm.fit2)

#9(f)
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

#10.a
summary(Carseats)
attach(Carseats)
lm.fit = lm(Sales~Price+Urban+US)
summary(lm.fit)
#10.e
lm.fit2 = lm(Sales~Price+US)
summary(lm.fit2)

#10.g
confint(lm.fit2)

#10.h
plot(predict(lm.fit2), rstudent(lm.fit2))
par(mfrow=c(2,2))
plot(lm.fit2)

