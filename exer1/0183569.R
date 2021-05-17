###第八题
library("ISLR")
data(package='ISLR')
##(a)
Auto=na.omit(Auto)
attach(Auto)
lm.fit=lm(mpg~horsepower)
summary(lm.fit)
#1)从summary的结果中可以看到，F值很大，但P值很小，这说明mpg和horsepower两者之间是相关的
#2）拟合效果可看RSE和R^2，RSE越小说明拟合效果越好，这里说明预测变量能被响应变量解释的占了60.59%
#3）由拟合结果的参数可知，是负相关的
predict(lm.fit,data.frame(horsepower=c(98)),interval = 'confidence')
predict(lm.fit,data.frame(horsepower=c(98)),interval = 'prediction')
#4）故预测值为24.47，置信区间为（23.97,24.96），预测区间为（14.81,34.12)
##(b)
plot(horsepower,mpg)
abline(lm.fit)
##(c)
par(mfrow=c(2,2))
abline(lm.fit)

###第九题
##a)
Auto=data(Auto,package = 'ISLR')
Auto=na.omit(Auto)
pairs(Auto)
##b)
cor(subset(Auto,select=-name))
##c
lm.fit1=lm(mpg~.-name,data=Auto)
summary(lm.fit1)
#1)根据F值和P值知道有关
#2）P值小于0.05表明有显著关系，故displacement,weight,year,origin这几个预测变量和响应变量有显著关系
#3）其系数为0.75，这说明随着车龄的增加，车子会越来越耗油
##d
par(mfrow=c(2,2))
plot(lm.fit1)
plot(predict(lm.fit1),rstudent(lm.fit1))
##e
lm.fit2=lm(mpg~cylinders*displacement+displacement*weight)
summary(lm.fit2)
##f
lm.fit3=lm(mpg~log(weight)+sqrt(horsepower)+acceleration+I(acceleration)^2)
summary(lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit3)
plot(predict(lm.fit3),rstudent(lm.fit3))

###第十题
##a）
library(ISLR)
summary(Carseats)
attach(Carseats)
lm.fit=lm(Sales~Price+Urban+US)
summary(lm.fit)
##b  由结果中的p值和F值可知，Price和US、Sales有关，Urban和Sales无关
##c  Sales=13.04-0.05*Price-0.02*Urban+1.20*US  其中Urban和us为yes的时候，其值为1,否则是0
##d  Price和US
##e  由b可知，Sales和US无关，所以我们可以去掉这个变量
lm.fit2=lm(Sales~Price+US)
summary(lm.fit2)
##f  由两者中修正前和修正后的R值可知，拟合效果差不多，但e略高于a
##g 
confint(lm.fit2)
##h
plot(predict(lm.fit2),rstudent(lm.fit2))
#从所得图形中，我们知道，stuendtize residuals的范围在-3到3之间，故没有离群点
par(mfrow=c(2,2))
plot(lm.fit2)
#从所得图形中可知，有一些点远远超过了其他点，即存在高杆点