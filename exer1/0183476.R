# 8.利用Auto数据集进行简单线性回归
library(MASS)
library(margins)
library(car)
library(ISLR)
Auto=data<-read.csv("Auto.csv",header=T,na.strings="?")
Auto=na.omit(Auto)
attach(Auto)
#  a)线性回归
summary(Auto)
lm.fit=lm(mpg~horsepower)
summary(lm.fit)
#零假设H0：β horsepower=0，假设horsepower与mpg不相关。由于F-statistic值远大于1，
#p值接近于0，拒绝原假设，则horsepower和mpg具有显著关系。
#mpg的平均值为23.45，线性回归的RSE为4.906，有20.9248%的相对误差。R-squared为0.6059，说明60.5948%的mpg可以被horsepower解释。
#线性回归系数小于零，说明mpg与horsepower之间的关系是消极的。
#预测mpg
predictor=mpg
response=horsepower
lm.fit2=lm(predictor~response)
predict(lm.fit2,data.frame(response=c(98)),interval="confidence")
predict(lm.fit2,data.frame(response=c(98)),interval="prediction")
# b)绘制mpg与horsepower散点图和最小二乘直线
plot(response,predictor)
abline(lm.fit2,lwd=2,col="blue")
# c)诊断最小二乘法
par(mfrow=c(2,2))
plot(lm.fit2)
#mpg与horsepower非线性相关。

# 9.利用联合的线性回归处理Auto数据集 
# a)绘制所有数据的散点图矩阵
pairs(Auto)
#b)计算相关性矩阵
cor(subset(Auto,select=-name))
#c)多元线性回归
lm.fit3=lm(mpg~.-name,data=Auto)
summary(lm.fit3)
#零假设 ：假设mpg与其他变量不相关。 由于F-statistic值远大于1，p值接近于0，拒绝原假设，则mpg与其他变量具有统计显著关系。
#参照每个变量的P值，displacement、weight 、year 、origin在统计显著关系。
#汽车对于能源的利用率逐年增长。
# d)线性回归拟合诊断图
par(mfrow=c(2,2))
plot(lm.fit3)
plot(predict(lm.fit3), rstudent(lm.fit3))
#残差仍为明显的曲线，说明多元线性回归不正确。
#有很多可能的离群值。由权重图知，14号点没有较大的残差也有非常大的权重。
# e)交互作用的线性回归模型拟合
lm.fit4=lm(mpg~displacement*weight+year*origin)
summary(lm.fit4)
#可以发现具有统计显著关系，残差也有很大的下降。 
# f)对预测变量变换
lm.fit5 = lm(mpg~log(horsepower)+sqrt(horsepower)+horsepower+I(horsepower^2))
summary(lm.fit5)
par(mfrow=c(2,2))
plot(lm.fit5)

#10.Carseats数据集 
# a)多元回归
attach(Carseats)
lm.fit=lm(Sales~Price+Urban+US)
summary(lm.fit)
# b)系数解释
#随着价格的升高销量下降 、商场是否在郊区与销量无关 、商场在美国销量会更多 
# c)写成方程：Sales = 13.04 + -0.05 Price + -0.02 UrbanYes + 1.20 USYes 
# d)根据p值和F-statistic知，Priece和USYES变量可以拒绝零假设
# e)更小模型
lm.fit2=lm(Sales~Price+US)
summary(lm.fit2)
# f）a)和e)中模型的RSE相近，但是e)稍微好一点 
# g)计算置信区间
confint(lm.fit2)
# h)
plot(predict(lm.fit2),rstudent(lm.fit2))
par(mfrow=c(2,2))
plot(lm.fit2)
#所有归一化的残差都在-3到3之间，没有明显的离群值。没有权重值超过(p+1)/n，说明没有明显重要的点即没有高杠杆点。
