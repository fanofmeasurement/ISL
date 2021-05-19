# 8
# install.packages("ISLR")
library(ISLR)
library(car)
head(Auto)

# (a)
# 进行简单线性回归：
fit <- lm(mpg~horsepower, Auto)
# 输出结果：
summary(fit)

## i
# 预测变量和响应变量之间有关系

## ii
# 预测变量和响应变量之间关系不大，因为马力的回归系数是-0.1578，较小

## iii
# 由于系数为-0.1578为负，所以两者是负相关

## iv
new.horsepower <- data.frame(horsepower = 98)
predict(fit, newdata = new.horsepower, interval = "confidence")
# 当马力为98时，mpg的预测值为24.4671
# 95%置信区间是(23.9731, 24.9611)
predict(fit, newdata = new.horsepower, interval = "prediction")
# 预测区间是(14.8094, 34.12476)

# (b)
plot(mpg~horsepower, Auto)
abline(fit, col="red")
# abline函数所画曲线为最小二乘回归线

# (c)
par(mfrow=c(2, 2))
# 最小二乘回归的拟合诊断图：
plot(fit)
# 从图中可以看出，模型基本符合残差正态性假设(除了343, 330, 334等点)，
# 同时满足线性假设，以及同方差性假设(水平线周围的点随机分布)

############################################

# 9
# (a)
library(car)
head(Auto)
# Auto数据集所有变量的散点图矩阵：
scatterplotMatrix(~ mpg + displacement + horsepower + weight + acceleration
                  + year + origin + name, data = Auto, 
                  spread = FALSE, smoother.args = list(lty = 2))

# (b)
# 排除定性变量name：
new.Auto <- Auto[, -9]
# 计算相关系数矩阵：
cor_Auto <- cor(new.Auto)
cor_Auto

# (c)
# 对变量(除定性变量外)的多元线性回归：
fit2 <- lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year+
             origin, new.Auto)
# 输出结果：
summary(fit2) 

## i
# 预测变量和响应变量之间有关系

## ii
# 与响应变量在统计上关系显著的预测变量有：displacment，weight，year，origin

## iii
# year的系数为0.750773，说明year与mpg为正相关且year对mpg影响较小。

# (d)、
par(mfrow = c(2, 2))
# 生成回归拟合诊断图：
plot(fit2)
# 从左上图和左下图可以看出模型基本符合线性假设以及同方差性假设
# 右上图说明大部分样本符合残差正态性假设(除了323，327，326等点)
# 从残差图(右下图)中可以看出327，394等离群点以及高杠杆值点14

# (e)
# 用两个变量及其交互项进行多元线性回归拟合(查看交互项显著性)：
# cylinders变量与其他变量的交互作用试验：
fit3 <- lm(mpg~cylinders+displacement+cylinders:displacement, new.Auto)
summary(fit3)
fit3 <- lm(mpg~cylinders+horsepower+cylinders:horsepower, new.Auto)
summary(fit3)
fit3 <- lm(mpg~cylinders+weight+cylinders:weight, new.Auto)
summary(fit3)
fit3 <- lm(mpg~cylinders+acceleration+cylinders:acceleration, new.Auto)
summary(fit3)   # 交互项的p值为0.0549，不显著
fit3 <- lm(mpg~cylinders+year+cylinders:year, new.Auto)
summary(fit3)
fit3 <- lm(mpg~cylinders+origin+cylinders:origin, new.Auto)
summary(fit3)   # 交互项的p值为0.131，不显著

# displacement变量与其他变量的交互作用试验：
fit3 <- lm(mpg~displacement+horsepower+displacement:horsepower, new.Auto)
summary(fit3)
fit3 <- lm(mpg~displacement+weight+displacement:weight, new.Auto)
summary(fit3)
fit3 <- lm(mpg~displacement+acceleration+displacement:acceleration, new.Auto)
summary(fit3)
fit3 <- lm(mpg~displacement+year+displacement:year, new.Auto)
summary(fit3)
fit3 <- lm(mpg~displacement+origin+displacement:origin, new.Auto)
summary(fit3)

# horsepower变量与其他变量的交互作用试验：
fit3 <- lm(mpg~horsepower+weight+horsepower:weight, new.Auto)
summary(fit3)
fit3 <- lm(mpg~horsepower+acceleration+horsepower:acceleration, new.Auto)
summary(fit3)
fit3 <- lm(mpg~horsepower+year+horsepower:year, new.Auto)
summary(fit3)
fit3 <- lm(mpg~horsepower+origin+horsepower:origin, new.Auto)
summary(fit3)

# weight变量与其他变量的交互作用试验：
fit3 <- lm(mpg~weight+acceleration+weight:acceleration, new.Auto)
summary(fit3)
fit3 <- lm(mpg~weight+year+weight:year, new.Auto)
summary(fit3)
fit3 <- lm(mpg~weight+origin+weight:origin, new.Auto)
summary(fit3)

# acceleration变量与其他变量的交互作用试验：
fit3 <- lm(mpg~acceleration+year+acceleration:year, new.Auto)
summary(fit3)    # 交互项的p值为0.54277，不显著
fit3 <- lm(mpg~acceleraton+origin+acceleraion:origin, new.Auto)
summary(fit3)    # 交互项的p值为0.54727，不显著

# year变量与其他变量的交互作用试验：
fit3 <- lm(mpg~year+origin+year:origin, new.Auto)
summary(fit3)   # 交互项的p值为0.0621，不显著

# 从上述试验可知，除了cylinders与acceleration，cylinders与origin，
# acceleration与year，acceleration与origin，以及year与orgin的交互项
# 不显著之外，其他变量之间的交互项都是显著的，即存在统计显著的交互作用

# (f)
# 对变量cylinders进行对数变换：
fit4 <- lm(mpg~log(cylinders)+displacement+horsepower+weight+acceleration+year+
             origin, new.Auto)
# 查看结果：
summary(fit4)
# 回归诊断：
plot(fit4)
# 对cylinders进行对数变换之后，该变量的显著性降低了，但是线性图以及残差图的
# 变化不大，可见对其进行对数变换对模型的作用不大

# 对horsepower进行二次项处理：
fit5 <- lm(mpg~cylinders+displacement+I(horsepower^2)+weight+acceleration+year+
             origin, new.Auto)
# 查看结果：
summary(fit5)
# 回归诊断：
plot(fit5)
# horsepower显著性虽有所提升，但仍然不显著，并且同时模型的
# 残差正态性有所下降

# 对horsepower进行平方差处理：
fit6 <- lm(mpg~cylinders+displacement+sqrt(horsepower)+weight+acceleration+year+
             origin, new.Auto)
# 查看结果：
summary(fit6)
# 回归诊断：
plot(fit6)
# 对horsepower进行开方处理后，变量的p值为0.000274，变量显著，
# 说明对horsepower的开方处理能够提高模型的拟合优度

################################################

# 10
head(Carseats)

# (a)
# 改写定性变量Urban：
num1 <- which(levels(Carseats$Urban) == "Yes")
levels(Carseats$Urban)[num1] <- 1
num0 <- which(levels(Carseats$Urban) == "No")
levels(Carseats$Urban)[num0] <- 0

# 改写定性变量US：
num1 <- which(levels(Carseats$US) == "Yes")
levels(Carseats$US)[num1] <- 1
num0 <- which(levels(Carseats$US) == "No")
levels(Carseats$US)[num0] <- 0

# 多元线性拟合：
fit <- lm(Sales~Price+Urban+US, Carseats)
# 查看结果：
summary(fit)

# (b)
# 模型中各个系数的解释：
# Price的系数为-0.055，即价格每增加一个单位时售价的平均增长
# 当Price每增加1个单位，Sales平均减少$0.055

# Urban的系数为-0.022，解释为Urban与非Urban的平均售价差异
# 说明Urban比非Urban售价平均便宜$0.022

# US的系数为1.200，解释为US与非US的平均售价差异
# 说明US比非US的车辆售价平均高出$1.200

# (c)
# 模型的方程形式：
# Sales = 13.043469-0.054459*Price-0.021916*Urban+1.200573*US

# (d)
# 变量Price和US可以拒绝零假设
# 变量Price和US的p值小于0.05，统计意义上显著，因此可以拒绝零假设

# (e)
# 用Price和US拟合模型：
fit2 <- lm(Sales~Price+US, Carseats)

# (f)
summary(fit)
summary(fit2)
# 原模型与缩小后的模型R方几乎一样，都解释了23.93%的方差波动
# 但是缩小后的模型的调整后的R方为23.54%，比原模型的调整后的R方更大
# 说明缩小后的模型拟合程度较好一些

# (g)
# 计算系数95%的置信区间：
confint(fit2, "Price", level = 0.95)
# Price系数的95%置信区间是(-0.065, -0.044)
confint(fit2, 'US1', level = 0.95)
# US系数的95%置信区间是(0.692, 1.708)

# (h)
par(mfrow = c(2, 2))
# 回归诊断：
plot(fit2)
# 从图中可以看出该模型满足线性假设，同方差性假设以及残差正态性假设
# 同时发现并无明显离群点
# 检验高杠杆值点
hat.plot <- function(fit) { 
  p <- length(coefficients(fit)) 
  n <- length(fitted(fit)) 
  plot(hatvalues(fit), main="Index Plot of Hat Values") 
  abline(h=c(2,3)*p/n, col="red", lty=2) 
  identify(1:n, hatvalues(fit), names(hatvalues(fit))) 
} 
par(mfrow = c(1, 1))
hat.plot(fit2)
# 得到点43，126，166，175在帽子均值的3倍以上，是高杠杆值点