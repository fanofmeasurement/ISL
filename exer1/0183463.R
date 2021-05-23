#8
library(MASS)
library(ISLR)
library(car)
Auto=read.csv("Auto.csv",header=T,na.strings="?")
Auto=na.omit(Auto)
attach(Auto)
summary(Auto)
#线性回归
lm.fit=lm(mpg~horsepower)
summary(lm.fit)
#a).零假设 H0：βhorsepower=0，假设horsepower与mpg不相关。 
#由于F-statistic值远大于1，p值接近于0，拒绝原假设，则horsepower和mpg具有统计显著关系。
#mpg的平均值为23.45，线性回归的RSE为4.906，有20.9248%的相对误差。R-squared为0.6059，说明60.5948%的mpg可以被horsepower解释。
#线性回归系数小于零，说明mpg与horsepower之间的关系是消极的。

#预测mpg
predict(lm.fit,data.frame(mpg=c(98)),interval="prediction")
#修改办法
predictor=mpg
response=horsepower
lm.fit2=lm(predictor~response)
predict(lm.fit2,data.frame(response=c(98)),interval="confidence")

predict(lm.fit2,data.frame(response=c(98)),interval="prediction")
#b).绘制mpg与horsepower散点图和最小二乘直线
plot(response,predictor)
abline(lm.fit2,lwd=3,col="red")
#c).诊断最小二乘法
par(mfrow=c(2,2))
plot(lm.fit2)
#表明，mpg与horsepower非线性相关。


#9
#a).绘制散点图矩阵
pairs(Auto)
#b).计算相关性矩阵
cor(subset(Auto,select=-name))
#c).多元线性回归
lm.fit3=lm(mpg~.-name,data=Auto)
summary(lm.fit3)
#零假设 ：假设mpg与其他变量不相关。 
#由于F-statistic值远大于1，p值接近于0，拒绝原假设，则mpg与其他变量具有统计显著关系。
#参照每个变量的P值，displacement、weight 、year 、origin在统计显著关系。
#汽车对于能源的利用率逐年增长
#d)
par(mfrow=c(2,2))
plot(lm.fit3)
#表明残差仍未明显的曲线，说明多元线性回归不正确。
plot(predict(lm.fit3), rstudent(lm.fit3))
#有许多可能的离群值 
#由权重图知，14号点没有较大的残差也有非常大的权重
#e)
lm.fit4=lm(mpg~displacement*weight+year*origin)
summary(lm.fit4)
#可以发现具有统计显著关系，残差也有很大的下降。
#f)
lm.fit5 = lm(mpg~log(horsepower)+sqrt(horsepower)+horsepower+I(horsepower^2))
summary(lm.fit5)
#诊断回归
par(mfrow=c(2,2))
plot(lm.fit5)


#10
#a）
summary(Carseats)
#多元线性回归
attach(Carseats)
lm.fit=lm(Sales~Price+Urban+US)
summary(lm.fit)
#b)随着价格的升高销量下降,商场是否在郊区与销量无关,商场在美国销量会更多
#c)Sales = 13.04 + -0.05 Price + -0.02 UrbanYes + 1.20 USYes 
#d)Priece和USYES可以，根据p值和F-statistic可以拒绝零假设。

#e)
lm.fit2=lm(Sales~Price+US)
summary(lm.fit2)
#f) a)和e)RSE相近，但是e)稍微好一点 
#g
confint(lm.fit2)
#h
plot(predict(lm.fit2),rstudent(lm.fit2))
#所有归一化的残差都在-3到3之间，没有明显的离群值
par(mfrow=c(2,2))
plot(lm.fit2)
#其中没有权重值超过(p+1)/n，说明没有明显重要的点。
