Auto = read.table("Auto.data.txt", header = T ,na.strings="?")
Auto = na.omit(Auto)
#(a)
attach(Auto)
lm.fit = lm(mpg ~ horsepower,data=Auto)
summary(lm.fit)
#i.由summary的结果来看，F-statistic很大而p-value很小，说明两者是有相关性的。
#ii.看拟合效果如何，得看RSE和R-square。
#R-square为0.6059，这说明Y的变异中能被X解释的部分所占比例有60.59%
#iii.由拟合出的参数可知，负相关。
#iv.
predict(lm.fit, data.frame(horsepower=c(98)), interval="confidence")
#结果是24.47，置信区间是(23.97, 24.96)
predict(lm.fit, data.frame(horsepower=c(98)), interval="prediction")
#预测区间是(14.81, 34.12)
#(b)
plot(horsepower, mpg)
abline(lm.fit)
#(c)
par(mfrow=c(2,2))
plot(lm.fit)


#9.(a)
Auto = read.table("Auto.data.txt", header = T ,na.strings="?")
Auto = na.omit(Auto)
pairs(Auto)
#(b)
cor(subset(Auto, select=-name))
#(c)
lm.fit1 = lm(mpg~.-name, data=Auto)
summary(lm.fit1)
#i.有关系，由f-statistic和p-value值可以判断
#ii.由p-value小于0.05可知，displacement, weight, year, and origin这几个预测变量和响应变量有显著关系。
#iii.车龄变量的系数是0.75，这说明随着车龄的增加，车子会越来越耗油。
#(d)
par(mfrow=c(2,2))
plot(lm.fit1)
plot(predict(lm.fit1), rstudent(lm.fit1))
#(e)
lm.fit2 = lm(mpg~cylinders*displacement+displacement*weight)
summary(lm.fit2)
#(f)
lm.fit3 = lm(mpg~log(weight)+sqrt(horsepower)+acceleration+I(acceleration^2))
summary(lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit3)
plot(predict(lm.fit3), rstudent(lm.fit3))
lm.fit2<-lm(log(mpg)~cylinders+displacement+horsepower+weight+acceleration+year+origin,data=Auto)
summary(lm.fit2)


#10.(a)
library(ISLR)
summary(Carseats)
attach(Carseats)
lm.fit = lm(Sales~Price+Urban+US)
summary(lm.fit)
#(b)由summary(lm.fit)的结果的p-value和t-statistic可知，Price和US与Sales有关，Urban和Sales无关
#(c)Sales = 13.04 + -0.05*Price - 0.02*Urban + 1.20*US，其中Urban和US为YES时值为1，不然为0
#(d)Price and US
#(e)由上面分析可知，Urban与Sales无关，所以我们可以去掉这个变量
lm.fit2 = lm(Sales~Price+US)
summary(lm.fit2)
#(f)在(a)中Multiple R-squared:  0.239,  Adjusted R-squared:  0.234；
#(e)中Multiple R-squared:  0.239,  Adjusted R-squared:  0.235 ，
#可知两者拟合度差不多，而(e)稍微好点
#(g)
confint(lm.fit2)
#(h)
plot(predict(lm.fit2), rstudent(lm.fit2))
#通过图可知，stuendtize residuals的范围在-3到3之间，所以没有离群点
par(mfrow=c(2,2))
plot(lm.fit2)
#通过图可知，有一些点远远超过了其他点，所以存在高杆点
