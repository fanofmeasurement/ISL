#8.

library(ISLR)
attach(Auto)
fit1 <- lm(mpg ~ horsepower, data = Auto)
summary(fit1)

predict(fit1, data.frame(horsepower = c(98)), interval = "confidence")
predict(fit1, data.frame(horsepower = c(98)), interval = "prediction")

# a）
# 
# i 从summary的中的p值较小可以看出，mpg和horse是相关的。
# 
# ii 从summary中的R^2 = 0.6049，预测变量能解释响应变量的60%以上，且相关系数R>0.8，说明两者之间有较强的相关性。
# 
# iii 估计的预测变量的系数小于0，因而两者是负相关。
# 
# iv 预测值为24.46708，95%置信区间为[23.97308，24.96108]，预测区间为[14.8094，34.12476]"



plot(horsepower, mpg)
abline(fit1)


#b）用plot和abline两个指令，先绘制散点图，再在散点图上绘制回归线，结果如图所示。
  
par(mfrow = c(2, 2))
plot(fit1)


# C）由四个诊断图可以看出:
#   
#   第一个图表明残差散点图并非线性的，这说明我们应该拟合一个非线性模型更符合；
# 
# 第二个图QQ图说明残差大部分接近直线，说明残差接近正态分布；
# 
# 第三个图用来检验等方差假设，但图中出现了类似“U型”，无法通过检验，所以我们的回归模型中除了mpg以外可以加入更多的变量来优化它；
# 
# 第四个图可以看出哪些样本点有最大杠杆，不难看出，样本点117和样本点94的两个样本点对回归模型中的系数影响最大。


#9.

attach(Auto)
n <- ncol(Auto)
Auto2 <- Auto[,-n]
pairs(Auto2)
cor(Auto2)


# a） 作图如代码结果所示
# 
# b） 相关系数矩阵如代码结果所示



fit2 <- lm(mpg~., data = Auto2)
summary(fit2)


# c） 
# 
# i) 从F统计量 和p-value来看，各个预测变量都与响应变量有一定的关系。
# 
# ii) 与响应变量关系显著性最强的是weight, year, origin这三个预测变量。
# 
# iii) year变量的系数是正的，而且数值较大，说明对mpg具有正相关关系，并且对mpg有比较大的影响。


par(mfrow = c(2, 2))
plot(fit2)


# d） 在样本点中第323,326和第327个数据具有非常大的残差，是离群点，第14个样本点是杠杆点。


fit3 <- update( fit2, . ~ . + horsepower:weight )
summary(fit3)
anova(fit2,fit3)


# e） 尝试使用horsepower:weight作为交互项,再使用anova（）函数进一步检验添加交互项后拟合程度是否更优，显然，P值<0.05，添加交互项后模型显著优于未添加的。


#分别做这三种变量的变换，并构建线性模型
Auto_log <- sapply(Auto2[2:ncol(Auto2)],log)
Auto_log <- as.data.frame(Auto_log)
fit3 <- lm(mpg~., data = Auto_log )
summary(fit3)


Auto_sqrt <- sapply(Auto2[2:ncol(Auto2)],sqrt)
Auto_sqrt <- as.data.frame(Auto_sqrt)
fit4 <- lm(mpg~., data = Auto_sqrt )
summary(fit4)


square = function(x){
  return(x^2)
}

Auto_square <- sapply(Auto2[2:ncol(Auto2)], square)
Auto_square <- as.data.frame(Auto_square)
fit5 <- lm(mpg~., data = Auto_square)
summary(fit5)


# f)分别对各个变量做log,平方根和平方三种运算，分别保存在新的数据集中，
#   并用这三个数据集进行线性模型的拟合，结果显示对预测变量进行平方后的数据构建的模型R-squared值最小，为 0.7981，拟合程度最优。
#   观察其他两个变换的模型，都有一定的优化作用，有其合理性。


#10.
  
fit6 <- lm(Sales ~ Price + Urban + US, data = Carseats)
summary(fit6)


# a） 结果如代码所示
# 
# b） 这表明价格系数是负的，并且是显著的，这说明随着价格的上升，销售额会显著下降。UrbanYes系数为负，但并不显著。可以说在城市的销售额和在农村的销售额差别不大。US的系数为正，表明在美国的商店比起不在美国的商店销售额有所增加。
# 
# c） Y=13.043469-0.054459*Price-0.021916*UrbanYes+1.200573*USYes
# (在城市则UrbanYes值取1，否则取0；在美国则USYes值取1，不在则取0)
# 
# d） Price 和 USYes P值较小，可以拒绝原假设。



fit7 <- lm(Sales ~ Price + US, data = Carseats)
summary(fit7)


# e） 去掉变量UrbanYes，再构建一次线性回归模型，结果如代码所示。
# 
# f） (a)中模型的R^2= 0.2393，同时误差方差= 2.472；(e) 中模型 R^2= 0.2393，误差方差= 2.469。e中的模型有更小的估计误差。



confint(fit7)


# g） 用confint指令计算，得Price 系数的置信区间为[-0.06475984， -0.04419543] USYes 系数的置信区间为[0.69151957， 1.70776632]。



plot(fit7)


# f） 对e中模型做回归诊断图，我们发现样本点51、69和377具有相对较大的esidual值，是离群点；而样本26、50和368对响应变量具有较大的影响，是高杠杆点。

