#8
library(ISLR)
attach(Auto)
lm.fit<-lm(mpg~horsepower,data=Auto)
##a
summary(lm.fit)
predict(lm.fit, data.frame(horsepower=c(98)), interval="confidence")
predict(lm.fit, data.frame(horsepower=c(98)), interval="prediction")
##有关系，由结果可知，mpg=39.9-0.15*horsepower
##两个变量呈现系数为-0.15的负相关关系
##预测值是24.47，置信区间是(23.97308,24.96108),预测区间是(14.8094,34.12476)

##b
plot(horsepower, mpg)
abline(lm.fit)
##c
par(mfrow=c(2,2))
plot(lm.fit)
##由残差拟合图，看出残差和拟合值有非线性关系，模型可以继续改进
##QQ图显示残差符合正态分布，
##Scale-Location图显示方差大致相等，符合等方差假定
##残差杠杆图显示，极值点影响较小


#9
pairs(Auto)#a
cor(subset(Auto, select=-name))#b
lm.fit1 = lm(mpg~.-name, data=Auto)
summary(lm.fit1)#c 由p值< 2.2e-16可知模型是显著的，因此预测变量和响应变量有关
###ii   displacement, weight, year, and origin  
###iii  正相关
##d
plot(lm.fit1)#残差在6-8，处有较大离群点，标准残差在2以上有高杠杆点
lm.fit2 = lm(mpg~cylinders*displacement+displacement*weight)
summary(lm.fit2) 
##e displacement:weight有交互作用
lm.fit3 = lm(mpg~log(weight)+sqrt(horsepower)+acceleration+I(acceleration^2))
summary(lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit3)
##用用log 根号 和平方作为项，拟合效果较好，所有预测变量均显著。，残差符合正态要求，离群点少

##10
library(ISLR)
summary(Carseats)
attach(Carseats)
lm.fit = lm(Sales~Price+Urban+US)
summary(lm.fit)
##a Price、US与Sales有关，Urban和Sales无关
##bc Sales = 13.04 + -0.05*Price - 0.02*Urban + 1.20*US，Urban和US为YES时，值为1，否则为0
##d Price US
lm.fit2 = lm(Sales~Price+US)
summary(lm.fit2)
##e去掉urban
##f a的 R-squared:  0.239,  Adjusted R-squared:  0.234，e的 R-squared:  0.239,  Adjusted R-squared:  0.235 ，e好
##g
confint(lm.fit2)
##h 有离群点
plot(predict(lm.fit2), rstudent(lm.fit2))


