install.packages("MASS")
install.packages("ISLR")
library("ISLR")
library("MASS")
##1 (a)
Auto = na.omit(Auto)
attach(Auto)
lm.fit = lm(mpg ~ horsepower)
summary(lm.fit)
predict(lm.fit, data.frame(horsepower=c(98)), interval="confidence")

predict(lm.fit, data.frame(horsepower=c(98)), interval="prediction")
"(a)
i.预测变量与响应变量之间有关系,F-statistic(599.7)比较大，p-value(<2.2e-16)比较小，说明预测变量与响应变量有关”

ii.
由结果可知，RSE为4.906，有20.9248%的相对误差，R-square为0.6059，这说明Y的变异中能被X解释的部分所占比例有60.59%
iii.
由拟合的结果可知，为负相关（-0.15784）
iV.
预测值是24.46708   
置信区间是（23.97, 24.96）
预测区间是（14.81, 34.12）
"
##(b)
plot(horsepower, mpg)
abline(lm.fit)
##(c)
par(mfrow=c(2,2))
plot(lm.fit)
detach(Auto)
"在残差图与拟合图中可以清楚地看到一个曲线关系，这表明可能要对回归模型加上一个二次项，其他图都满足假设"

##9
#(a)
attach(Auto)
pairs(Auto)
#(b)
cor(subset(Auto, select=-name))
#(c)
lm.fit_1 = lm(mpg~.-name, data=Auto)
summary(lm.fit_1)
"
i.由于F-statistic值远大于1，p值很小（接近于0），则预测变量与响应变量具有关系。
ii.观察每个变量的p值，displacement、weight 、year 、origin在统计显著关系。
iii.车龄变量的系数是0.75，这说明随着车龄的增加，车子会越来越耗油。
"
#d
par(mfrow=c(2,2))
plot(lm.fit_1)
plot(predict(lm.fit_1), rstudent(lm.fit_1))
"残差图与拟合图没有明显的曲线
有异常大的离群点，14号点没有较大的残差，但有非常大的权重"
#e
lm.fit_2 = lm(mpg~cylinders*displacement+displacement*weight)
summary(lm.fit_2)
"由p值可以看出有统计显著关系"
#f
lm.fit_3 = lm(mpg~log(horsepower)+sqrt(horsepower)+horsepower+I(horsepower^2))
summary(lm.fit_3)
par(mfrow=c(2,2))
plot(lm.fit_3)
"残差图与拟合图没有明显的曲线，说明因变量与自变量线性相关。位置尺度图水平线的点未随机分布，不满足同方差性"
#10

library(ISLR)
summary(Carseats)
(a)
attach(Carseats)
lm.fit=lm(Sales~Price+Urban+US)
summary(lm.fit)
#b
"根据拟合结果，随着价格的升高销量下降，美国的销量更多"
#c
"sales = 13.04-0.0545Price-0.0219UrbanYes+1.2005USYes"
#d
"Price和USYes可以，根据p值和F-statistic可以拒绝零假设"
#e
lm.fit2=lm(Sales~Price+US)
summary(lm.fit2)
#f
"a和e的RSE相近，但是e的拟合效果更好一些"
#g
confint(lm.fit2)
"                 2.5 %      97.5 %
(Intercept) 11.79032020 14.27126531
Price       -0.06475984 -0.04419543
USYes        0.69151957  1.70776632"
#h
plot(predict(lm.fit2),rstudent(lm.fit2))
par(mfrow=c(2,2))
plot(lm.fit2)
"所有归一化的残差都在-3到3之间，说明没有明显的离群值。
没有权重值超过(p+1)/n，说明没有明显重要的点。"



