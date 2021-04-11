library(MASS)
library(ISLR)
library(car)
summary(Auto)
##8.
##a.
Auto = na.omit(Auto)
attach(Auto)
lm.fit=lm(mpg~horsepower)
summary(lm.fit)
predict(lm.fit,data.frame(mpg=c(98)),interval="prediction")
predictor=mpg
response=horsepower
lm.fit2=lm(predictor~response)
predict(lm.fit2,data.frame(response=c(98)),interval="confidence")
predict(lm.fit2,data.frame(response=c(98)),interval="prediction")
##1)根据输出结果预测变量与响应变量之间有关系
##2）线性回归的RSE为4.906，r-square=0.6059，说明60.5948%的预测变量可以响应变量解释
##3）线性回归系数小于零，mpg与horsepower之间的关系是负相关
##4）预测结果为24.47，95%的置信区间和预测区间分别为（23.97，24.96）（14.8096，34.12476）
##b.
plot(response,predictor)
abline(lm.fit2,lwd=3,col="red")
##c
par(mfrow=c(2,2))
plot(lim.fit2)
##mpg与horsepower非线性相关，可能要考虑非线性变换


##9.
##a散点图矩阵
pairs(Auto)
##b相关系数矩阵
cor(subset(Auto,select=-name))
##c
lm.fit3=lm(mpg~.-name,data=Auto)
summary(lm.fit3)
##i p值接近于0，f统计量远大于1 ，故预测变量与响应变量之间有关系
##ii根据p值，displacement、weight 、year 、origin在统计显著关系
##iii.说明随着车龄的增加会越来越耗油
##d
par(mfrow=c(2,2))
plot(lm.fit3)
##由高杠杆作用点，没有异常的大离群点。
##e
lm.fit4=lm(mpg~displacement*weight+year*origin)
summary(lm.fit4)
##具有统计显著关系，残差也有很大的下降
##f
lm.fit5 = lm(mpg~log(horsepower)+sqrt(horsepower)+horsepower+I(horsepower^2))
summary(lm.fit5)
par(mfrow=c(2,2))
plot(lm.fit5)
##可以看出经过非线性变换后，模型拟合度更好了，原本不显著的变量变显著



##10.
##a
summary(Carseats)
attach(Carseats)
lm.fit=lm(Sales~Price+Urban+US)
summary(lm.fit)
##b
##随着价格的升高销量下降 商场是否在郊区与销量无关 商场在美国销量会更多 
## c.Sales = 13.04 + -0.05 Price + -0.02 UrbanYes + 1.20 USYes 
## d.Priece和USYES可以
##e
lm.fit2=lm(Sales~Price+US)
summary(lm.fit2)
##f a和e RSE相近，但是e会稍微好一点 
##g
confint(lm.fit2)
##Price的系数置信区间为（-0.065，-0.441）
##US的系数置信区间为（0.692，1.708）

##h
plot(predict(lm.fit2),rstudent(lm.fit2))
plot(lm.fit2)
plot(lm2)
##由图可知，没有离群点，有高杠杆点
