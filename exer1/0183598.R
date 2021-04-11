setwd("C:/Users/asus/Desktop")
library(ISLR)


Auto = na.omit(Auto)

#8
#a
attach (Auto)
lim.fit = lm(mpg~horsepower)
lim.fit
summary(lim.fit)

#i.由输出结果看，p值很小，说明两者是有相关性的。

#ii.R^2=0.6059，这说明Y中有60.59%能被X解释

#iii.负相关。

#iv.预测值是是24.47，置信区间是（23.97，24.96）
#预测区间是(14.81, 34.12)

#b
plot(horsepower,mpg)
abline(lim.fit)

#c
par(mfrow=c(2,2))
plot(lim.fit)
#由图我们可以看出数据是非线性的，要考虑的预测变量进行非线性变换



#9
#a
pairs(Auto)

#b
cor(subset(Auto, select=-name))

#c
lim.fit1 = lm(mpg~.-name, data=Auto)
summary(lim.fit1)

#i。预测变量和响应变量有关系，p值很小，F值也大。
#ii。 即p值<0.05，displacement,weight,year，origin和响应变量有显著关系。
#iii。系数是0.75，即随着车龄增加会越来越耗油


#d
par(mfrow=c(2,2))
plot(lim.fit1)
plot(predict(lm.fit1), rstudent(lm.fit1))
#由图可知，没有异常的大离群点，有高杠杆作用点。


#e
lim.fit2 = lm(mpg~cylinders*displacement+weight:acceleration)
summary(lim.fit2)
#由p值<0.05可以看出，存在统计显著的交互作用。


#f
lim.fit3 = lm(mpg~log(cylinders)+I(displacement^2)+horsepower+sqrt(weight)+year,data=Auto)
summary(lim.fit3)
par(mfrow=c(2,2)) 
plot(lim.fit3)
#可以看出经过非线性变换后，原本不显著的变量变显著，模型拟合度更好了。


#10
#a

attach(Carseats)
lm1 = lm(Sales~Price+Urban+US)
summary(lm1)

##b Price和US与Sales有关，p值均小于0.05，Urban和Sales无关。其中us和urban为定性变量。

#c。Sales = 13.04  -0.05 * Price -0.02 *Urban+ 1.20 *US，当Urban和US为YES时值为1，no时为0.

#d 其中peice和us可以拒绝零假设，因为它们的p值<0.05#

#e
lm2 = lm(Sales ~ Price + US)
summary(lm2)

#f a中Multiple R-squared:  0.2393,	Adjusted R-squared:  0.2335 
#e中Multiple R-squared:  0.2393,	Adjusted R-squared:  0.2354 
#拟合度差不多

#g
confint(lm2)

##Price的系数置信区间为（-0.065，-0.441）
##US的系数置信区间为（0.692，1.708）

#h
plot(predict(lm2), rstudent(lm2))

par(mfrow=c(2,2))
plot(lm2)
#由图可知，没有离群点，但是有高杠杆点。