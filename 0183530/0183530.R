library(ISLR)
library(MASS)
#8
attach(Auto)
q8data <- lm(formula = mpg ~ horsepower, data = Auto)
summary(q8data)
#(a)
predict( q8data , data.frame(horsepower=98), interval = "confidence")
#i.由summary的结果来看，F-statistic很大而p-value很小，说明两者是有相关性的。
#ii.由书的54页可知，看拟合效果如何，得看RSE和R-square。
#R-square为0.6059，这说明Y的变异中能被X解释的部分所占比例有60.59%
#iii.由拟合出的参数可知，负相关。
#iv.predict(lm.fit, data.frame(horsepower=c(98)), interval="confidence")。结果是24.47，置信区间是(23.97, 24.96)
#predict(lm.fit, data.frame(horsepower=c(98)), interval="prediction")。预测区间是(14.81, 34.12)

#(b)
plot(horsepower , mpg)
abline(q8data , lwd = 3 , col = "green")
#(c)
par(mfrow=c(2,2))
plot(q8data)


#9
#(a)
library(GGally)
library(ggplot2)
ggpairs(Auto, columns=1:8) + ggtitle("q9 scatter plot matrix")
library(WVPlots)
PairPlot(Auto, colnames(Auto)[1:8],  "q9 Scatter plot matrix")
#(b)
Auto_exclude <- subset(Auto , select = -name)
cor(Auto_exclude)
#(c)
q9data <- lm( formula = mpg ~ ., data = Auto_exclude)
summary(q9data)
#i.有。有f-statistic和p-value值可以判断
#ii.由p-value小于0.05可知，displacement, weight, year, and origin这几个预测变量和响应变量有显著关系。
#iii.车龄变量的系数是0.75，这说明随着车龄的增加，车子会越来越耗油。

#(d)
par(mfrow=c(2,2))
plot(q9data)
#(e)
lm.fit2 = lm(mpg~cylinders*displacement+displacement*weight,data = Auto)
summary(lm.fit2)

#(f)
q9flog <- lm(formula = mpg ~horsepower + log(horsepower), data = Auto_exclude)
summary(q9flog)
anova(q8data , q9flog)
q9flog2 <- lm(formula = mpg ~horsepower + log(horsepower), data = Auto_exclude)
summary(q9flog2)
anova(q9flog2 , q9flog)
#优先度纯对数最高
#square
q9fsquare <- lm(formula = mpg ~ I(horsepower^2), data = Auto_exclude)
summary(q9fsquare)
anova(q9fsquare , q8data)
#一次项拟合度更高
#sqrt
q9fsqrt <- lm(formula = mpg ~ I(horsepower^0.5), data = Auto_exclude)
summary(q9fsqrt)
anova(q8data , q9fsqrt)
#平方根拟合度更高

#10
#(a)
q10data <- lm(formula = Sales ~ Price + Urban + US , data = Carseats)
summary(q10data)
#(b)
#p-value和t-statistic可知，Price和US与Sales有关，Urban和Sales无关
#(c)
#Sales = 13.04 + -0.05*Price - 0.02*Urban + 1.20*US，其中Urban和US为YES时，值为1，否则为0
#(d)
#Price and US
#(e)
q10data_better <- update( q10data, . ~ . - Urban )
summary(q10data_better)
#(f)
#两者拟合度差不多，而(e)稍微好点
#(g)
confint(q10data_better , level = 0.95)
#(h)
par(mfrow=c(2,2))
plot(q10data_better)