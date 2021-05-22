library(MASS)
library(ISLR)
names(Auto)
auto.fit=lm(mpg~horsepower,Auto)#简单线性回归
summary(auto.fit)
predict(auto.fit,data.frame(horsepower=(c(98))),interval = "confidence")#置信区间
predict(auto.fit,data.frame(horsepower=(c(98))),interval = "prediction")#预测区间
attach(Auto)
plot(horsepower,mpg)#关系图
abline(auto.fit)#回归线
par(mfrow=c(1,1))
plot(auto.fit)
pairs(Auto[1:9], 
      main = "auto data plot",
      pch = 21)#散点图矩阵
options(digits=2)
cor(Auto[1:8])#相关系数矩阵
auto.fit2=lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year+origin,Auto)
summary(auto.fit2)
plot(auto.fit2)
auto.fit3=lm(mpg~year*acceleration,Auto)
summary(auto.fit3)
auto.fit4=lm(mpg~log(cylinders)+log(displacement)
             +log(horsepower)+log(weight)+log(acceleration)+log(year)
             +log(origin),Auto)
summary(auto.fit4)
Carseats
names(Carseats)
fit5=lm(Sales~Price+Urban+US,Carseats)
summary(fit5)
fit6=lm(Sales~Price+US,Carseats)
summary(fit6)
confint(fit6)
plot(fit6)
