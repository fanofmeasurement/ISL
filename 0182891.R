library(ISLR)
library(MASS)
#第8题
q8fit <- lm(formula = mpg ~ hP, data = Auto)
summary(q8fit)
#(a)
#(1)有关系
#(2)R-square为0.6059，说明mpg的变异中能被hp解释的的部分所占比例为60.59%
#(3)由拟合的参数可知负相关
#(4)置信区间（23.97,24.96）
predict( q8fit , data.frame(hp = 98), interval = "confidence")
#(b)
attach(Auto)
plot(hp , mpg)
abline(q8fit , lwd = 3 , col = "black")
#(c)
par(mfrow=c(2,2))
plot(q8fit)


#第9题
#(a)
library(GGally)
ggpairs(Auto, columns=1:8) + 
  + ggtitle("q9(a) matrix of scatter plot ")

#(b)
Auto_exclude <- subset(Auto , select = -name)
cor(Auto_exclude)
#(c)
q9fit <- lm( formula = mpg ~ ., data = Auto_exclude)
summary(q9fit)
#(1)有关系，从f-statistic 和 P-value的值可以判断
#(2)由P-value小于0.05可知，displacement，weight，year和origin这几个变量有显著关系
#(3)year的系数为0.75，说明随着车年龄增加，车的耗油也会增加
#(d)
par(mfrow=c(2,2))
plot(q9fit)
#(e)
lm.fit2 = lm(mpg ~ cylinders*displacement + displacement*weight)
summary(lm.fit2)
#(f)displacement horsepower weight acceleration
#log
q9flog <- lm(formula = mpg ~hp + log(hp), data = Auto_exclude)
summary(q9flog)
anova(q8fit , q9flog)
q9flog2 <- lm(formula = mpg ~hp + log(hp), data = Auto_exclude)
summary(q9flog2)
anova(q9flog2 , q9flog)
#优先度纯对数最高
#square
q9fsquare <- lm(formula = mpg ~ I(horsepower^2), data = Auto_exclude)
summary(q9fsquare)
anova(q9fsquare , q8fit)
#一次项拟合度更高
#sqrt
q9fsqrt <- lm(formula = mpg ~ I(horsepower^0.5), data = Auto_exclude)
summary(q9fsqrt)
anova(q8fit , q9fsqrt)
#平方根拟合度更高

#第10题
#(a)
q10fit <- lm(formula = Sales ~ Price + Urban + US , data = Carseats)
summary(q10fit)
#(b)
# Price和US与Sales有关，Urban和Sales无关
#(c)
#方程为：Sales = 13.04 - 0.05*Price - 0.02*Urban + 1.20*US
#(d)
#变量Price和US可以拒绝零假设
#(e)
q10fit_better <- update( q10fit, . ~ . - Urban )
summary(q10fit_better)
#(f)
#（a）中的R-square为0.239，adjusted R-square为0.234
#（e）中的R-square为0.239，adjusted R-square为0.235
#两者的拟合度差不多，（e）的拟合度稍微好一点点
#(g)
confint(q10fit_better , level = 0.95)
#(h)
par(mfrow=c(2,2))
plot(q10fit_better)
#没有离群点，但存在高杆点











