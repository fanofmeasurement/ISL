library(ISLR)
library(MASS)
#8
q8fit <- lm(formula = mpg ~ horsepower, data = Auto)
summary(q8fit)
#(a)
#(1)存在显著不为0的关系
#(2)R² = 0.6059，horsepower可解释响应变量60.59%的部分
#(3)summary结果表明horsepower系数显著小于0，故预测变量和响应变量之间负相关
#(4)
predict( q8fit , data.frame(horsepower=98), interval = "confidence")
predict( q8fit , data.frame(horsepower=98), interval = "prediction")
#预测值为24.46708，95%置信区间为（23.97308,24.96108），预测区间为(14.8094,34.12476)

#(b)
attach(Auto)
plot(horsepower , mpg)
abline(q8fit , lwd = 2 , col = "blue")

#(c)
par(mfrow=c(2,2))
plot(q8fit)
#分析：残差vs拟合图表明可能需要对模型做出非线性变换（并不为正负交替），模型并非线性；Q-Q图表明可认为模型的数据满足正态分布假定；
# scale-location表明该模型可能需要加权进行计算；最后一张图表明117样本和84样本是杠杆点，对模型的拟合起了较大作用


#9
#(a)
library(GGally)
ggpairs(Auto, columns=1:8) +
  ggtitle("q9(a) scatter plot matrix")
#or
library(WVPlots)
PairPlot(Auto, colnames(Auto)[1:8],  "q9(a) Scatter plot matrix")

#(b)
Auto_exclude <- subset(Auto , select = -name)
cor(Auto_exclude)

#(c)
q9fit <- lm( formula = mpg ~ ., data = Auto_exclude)
summary(q9fit)
#(1)整体p值极小，且存在显著不为0的预测变量，这表明预测变量和响应变量之间存在关系
#(2)在p=0.001的显著性水平下，displacement，weight，year，origin都有显著关系，若显著性水平近似为0则仅有后三者
#(3)年龄的系数显著大于0，这表明其与mpg正相关，这表明年龄变大会使得mpg增大

#(d)
par(mfrow=c(2,2))
plot(q9fit)
#模型拟合的问题与8(c)中类似；323与327为残差较大的离群点；14为有异常高杠杆作用的点

#(e)
q9e <-  update( q9fit, . ~ . + horsepower:weight ) 
summary(q9e)
#horsepower:weight显著
q9e2 <-lm(formula = mpg ~horsepower*weight, data = Auto_exclude)
summary(q9e2)
#horsepower*weight交互项显著

#(f)displacement horsepower weight acceleration
#log
q9flog <- lm(formula = mpg ~horsepower + log(horsepower), data = Auto_exclude)
summary(q9flog)
anova(q8fit , q9flog)
#加入对数项后拟合程度更好
q9flog2 <- lm(formula = mpg ~ log(horsepower), data = Auto_exclude)
summary(q9flog)
anova(q9flog , q9flog2)
#三者之中纯对数拟合程度最好
#square
q9fsquare <- lm(formula = mpg ~ I(horsepower^2), data = Auto_exclude)
summary(q9fsquare)
anova(q9fsquare , q8fit)
#horsepower一次项拟合度更高
#sqrt
q9fsqrt <- lm(formula = mpg ~ I(horsepower^0.5), data = Auto_exclude)
summary(q9fsqrt)
anova(q8fit , q9fsqrt)
#horsepower平方根拟合度更高


#10
#(a)
q10fit <- lm(formula = Sales ~ Price + Urban + US , data = Carseats)
summary(q10fit)
#(b)
#UrbanYes系数不显著，USYes水平显著为正，与结果正相关；Price水平显著为负，与结果负相关
#(c)
#Sales = a0+a1Price+a2UrbanYes+a3USYes,后两者为虚拟变量
#(d)
#Price和USYes
#(e)
q10fit_better <- update( q10fit, . ~ . - Urban )
summary(q10fit_better)
#(f)
#二者R²近似但是(e)中模型标准误更小，(e)模型更加优秀
#(g)
confint(q10fit_better , level = 0.95)
#结果如上
#(h)
par(mfrow=c(2,2))
plot(q10fit_better)
#69/377/51为明显的离群点，而26/50/368为高杠杆点
