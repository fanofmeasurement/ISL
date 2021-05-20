#第8题.
library(MASS)
library(ISLR)
library(car)
#引入要用的包
attach(Auto)
summary(Auto)
#与数据集绑定
lm.fit1=lm(mpg ~ horsepower)
summary(lm.fit1)
predict(lm.fit1,data.frame(horsepower=c(98)),interval="confidence")
predict(lm.fit1,data.frame(horsepower=c(98)),interval="prediction")
plot(horsepower,mpg)
abline(lm.fit1,lwd=3,col="red")
par(mfrow=c(2,2))
plot(lm.fit1)

#第9题
pairs(Auto)
cor(subset(Auto,select=-name))
lm.fit2=lm(mpg~.-name,data=Auto)
summary(lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)
plot(predict(lm.fit2), rstudent(lm.fit2))
lm.fit3=lm(mpg~displacement*weight+year*origin)
summary(lm.fit3)
lm.fit4=lm(mpg~log(horsepower)+sqrt(horsepower)+horsepower+I(horsepower^2))
summary(lm.fit4)
par(mfrow=c(2,2))
plot(lm.fit4)


#第10题.
attach(Carseats)
lm.fit5=lm(Sales~Price+Urban+US)
summary(lm.fit5)
lm.fit6=lm(Sales~Price+US)
summary(lm.fit6)
confint(lm.fit6)
plot(predict(lm.fit6),rstudent(lm.fit6))
par(mfrow=c(2,2))
plot(lm.fit6)
