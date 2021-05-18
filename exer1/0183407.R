#8.
#(a)
library(MASS)
library(ISLR)

attach(Auto)
summary(Auto)
lm.fit=lm(mpg ~ horsepower)
summary(lm.fit)
predict(lm.fit,data.frame(horsepower=c(98)),interval="confidence")
predict(lm.fit,data.frame(horsepower=c(98)),interval="prediction")

#1.因为p值远小于0.05,认为horsepower和mpg有关系。
#2. R-squared为0.6059,60.5948%的mpg可以被horsepower解释。
#3.线性回归系数小于零,mpg与horsepower负相关。
#4.mpg的预测值是24.46708,95%置信区间为(23.97308,24.96108),95%预测区间为(14.8094,34.12476)。

#(b)如图所示
plot(horsepower,mpg)
abline(lm.fit,lwd=3,col="red")

#(c)
par(mfrow=c(2,2))
plot(lm.fit)
#拟合效果不佳，mpg与horsepower非线性相关。

#9.
#(a)
pairs(Auto)

#(b)
cor(subset(Auto,select=-name))

#(c)
lm.fit2=lm(mpg~.-name,data=Auto)
summary(lm.fit2)
#1.p值小于0.05,预测变量和相应变量有关系。
#2.p值小于0.05,displacement,weight,year,origin和相应变量有显著关系。
#3.year对mpg有正向影响,year增加1单位,mpg增加0.750773单位。

#(d)
par(mfrow=c(2,2))
plot(lm.fit2)
plot(predict(lm.fit2), rstudent(lm.fit2))
#331残差很大，是离群点。

#(e)
lm.fit3=lm(mpg~displacement*weight+year*origin)
summary(lm.fit3)
#存在显著的交互作用。

#(f)
lm.fit4=lm(mpg~log(horsepower)+sqrt(horsepower)+horsepower+I(horsepower^2))
summary(lm.fit4)
par(mfrow=c(2,2))
plot(lm.fit4)

#10.

attach(Carseats)
lm.fit5=lm(Sales~Price+Urban+US)
summary(lm.fit5)
#(a)随着价格上升，销量下降。销量与商场是否在郊区无关,在美国销量更高
#(c)Sales = 13.04 + -0.05 Price + -0.02 UrbanYes + 1.20 USYes
#(d)Priece和USYES可以拒绝零假设。

#(e)
lm.fit6=lm(Sales~Price+US)
summary(lm.fit6)
#(f) (a)和(e)差不多

#(g)
confint(lm.fit6)
#Price：(-0.06475984,-0.04419543)
#USYes：(0.69151957,1.70776632)

#(h)
plot(predict(lm.fit6),rstudent(lm.fit6))
par(mfrow=c(2,2))
plot(lm.fit6)
#存在离群点69,51,377,和高杠杆点26,50,368