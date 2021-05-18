#8.
#(a)
library(ISLR)
attach(Auto)
lm.fit = lm(mpg~horsepower)
summary(lm.fit)
#预测变量和响应变量间有关系；60.5948%的mpg可以被horsepower解释；负相关
predictor=mpg
response=horsepower
lm.fit2=lm(predictor~response)
predict(lm.fit2,data.frame(response=c(98)),interval="confidence")
predict(lm.fit2,data.frame(response=c(98)),interval="prediction")
#马力98时mpg预测值24.46708，置信区间(23.97308,24.96108),预测区间(14.8094,34.12476)
#(b)
plot(horsepower, mpg)
abline(lm.fit)
#(c)
par(mfrow=c(2,2))
plot(lm.fit)
#mpg与horsepower非线性相关

#9.
library(car)
pairs(Auto)
#(b)
library(psych)
Auto = na.omit(Auto)
Auto$name = NULL
corr.test(Auto)
#(c)
fit = lm( mpg~., data=Auto )
summary(fit)
#有关系，displacement、weight 、year 、origin有显著关系，表示汽车mpg与year正相关
#(d)
par(mfrow=c(2,2))
plot(fit)
#有离群点，有异常高杠杆作用的点
#(e)
lm.fit4=lm(mpg~displacement*weight+year*origin)
summary(lm.fit4)
#存在统计显著的交互作用
#(f)
lm.fit5 = lm(mpg~log(horsepower)+sqrt(horsepower)+horsepower+I(horsepower^2))
summary(lm.fit5)

#10.
library(ISLR)
attach(Carseats)
names(Carseats)
fit=lm(Sales~Price+Urban+US)
summary(fit)
#随着价格的升高销量下降，商场是否在郊区与销量无关，商场在美国销量会更多
#(c)Sales = 13.04 + -0.05 Price + -0.02 UrbanYes + 1.20 USYes
#(d)Priece和USYES可以拒绝零假设
#(e)
fit2=lm(Sales~Price+US)
summary(fit2)
#(f)(e)的拟合度更好一点
#(g)
confint(fit2)
#(h)
par(mfrow=c(2,2))
plot(fit2)
#没有离群点和高杠杆点

