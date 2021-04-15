##8
#a
library(MASS)
library(ISLR)
library(car)
Auto=read.csv("Auto.csv",header=T,na.strings="?")
Auto=na.omit(Auto)
attach(Auto)
summary(Auto)
#（i）有关系 （ii）mpg的平均值为23.45，线性回归的RSE为4.906，有20.9248%的相对误差。R-squared为0.6059，说明60.5948%的mpg可以被horsepower解释 （iii）线性回归系数小于零，说明mpg与horsepower之间的关系是负相关
predict(lm.fit, data.frame(horsepower=c(98)), interval="confidence")
predict(lm.fit, data.frame(horsepower=c(98)), interval="prediction")
#（iiii）结果24.47，置信区间是(23.97, 24.96) ；预测区间是(14.81, 34.12)

#b
plot(horsepower, mpg)
abline(lm.fit)

#c
par(mfrow=c(2,2))
plot(lm.fit)
#有许多证据表明，mpg与horsepower非线性相关

##9
#a
pairs(Auto)

#b
cor(subset(Auto,select=-name))

#c
lm.fit3=lm(mpg~.-name,data=Auto)
summary(lm.fit3)
#零假设:假设mpg与其他变量不相关。 
#由于F-statistic值远大于1，p值接近于0，拒绝原假设，则mpg与其他变量具有统计显著关系。
#参照每个变量的P值，displacement、weight 、year 、origin在统计显著关系。
#汽车对于能源的利用率逐年增长

#d
par(mfrow=c(2,2))
plot(lm.fit3)
#残差仍未明显的曲线，说明多元线性回归不正确。
plot(predict(lm.fit3), rstudent(lm.fit3))
#由权重图知，14号点没有较大的残差也有非常大的权重。

#e
lm.fit4=lm(mpg~displacement*weight+year*origin)
summary(lm.fit4)
#可以发现具有统计显著关系，残差也有很大的下降。 

#f
lm.fit5 = lm(mpg~log(horsepower)+sqrt(horsepower)+horsepower+I(horsepower^2))
summary(lm.fit5)
par(mfrow=c(2,2))
plot(lm.fit5)

##10
#a
library(ISLR)
summary(Carseats)
attach(Carseats)
lm.fit = lm(Sales~Price+Urban+US)
summary(lm.fit)

#b
#由summary(lm.fit)的结果的p-value和t-statistic可知，Price和US与Sales有关，Urban和Sales无关

#c
#Sales = 13.04 + -0.05*Price - 0.02*Urban + 1.20*US，其中Urban和US为YES时，值为1，否则为0

#d
#Price and US

#e
#由上面分析可知，Urban与Sales无关，所以我们可以去掉这个变量
lm.fit2 = lm(Sales~Price+US)
summary(lm.fit2)

#f
#(a)中Multiple R-squared:  0.239,  Adjusted R-squared:  0.234，(e)中Multiple R-squared:  0.239,  Adjusted R-squared:  0.235 ，可知两者拟合度差不多，而(e)稍微好点

#g
confint(lm.fit2)

#h
plot(predict(lm.fit2), rstudent(lm.fit2))

#通过这个命令得到的图，我们可知，stuendtize residuals的范围在-3到3之间，所以没有离群点
par(mfrow=c(2,2))
plot(lm.fit2)
#通过这个命令得到的图，我们可知，有一些点远远超过了其他点，故存在高杆点