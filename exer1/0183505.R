#第8题
###a
library(ISLR)
attach(Auto)
lm.fit <- lm(mpg ~ horsepower, data = Auto)
summary(lm.fit)
predict(lm.fit, data.frame(horsepower = c(98)), interval = "confidence")
predict(lm.fit, data.frame(horsepower = c(98)), interval = "prediction")
  #1、 mpg和horse是相关的。
  #2、两者之间有较强的相关性。
  #3、两者是负相关。
  #4、当马力为98时，mpg(油耗)的预测值为24.46708，
  #   相应的95%置信区间为[23.97308，24.96108],预测区间为[14.8094，34.12476]

###b
plot(horsepower, mpg)
abline(lm.fit)

###c
par(mfrow = c(2, 2))
plot(lm.fit)
  #由四个诊断图可以看出，第一个图表明残差散点图并非线性的，这说明我们应该拟合一个非线性模型更符合；
  #第二个图QQ图说明残差大部分接近直线，说明残差接近正态分布；
  #第三个图可以更方便地看出误差分布的范围,不存在异方差；
  #第四个图看出指数为117和94的两个样本点对回归模型中的系数影响最大。

#第9题
###a
pairs(Auto)
###b
temp <- Auto[,-ncol(Auto) ]
cor(temp)
###c
lm.fit2 <- lm(mpg~., data = temp)
summary(lm.fit2)
  #1、各个预测变量与响应变量具有一定的关系。
  #2、displacement、weight、year、origin三个预测变量。
  #3、year对mpg具有正相关关系，并且对mpg有比较大的影响。
  #系数为0.750773，说明年份与马力呈正相关，随着年份增加，汽车的马力会越来越足。

###d
par(mfrow = c(2, 2))
plot(lm.fit2)
  #在样本点中第323、第326、第327个数据具有非常大的残差，是离群点
  #第14个样本点是杠杆点。
###e
lm.fit3 <- lm(mpg ~ .+ horsepower:weight , data = temp)
summary(lm.fit3)
anova(lm.fit2, lm.fit3)
  #通过添加horsepower与weight的交互项，使得检验是显著的，
  #再次通过anova()函数判断两个回归模型，F = 111.88，远远优于交互项的模型。
###f
temp2 <- sapply(temp[2:ncol(temp)],log)
temp2 <- as.data.frame(temp2)
lm.fit4 <- lm(mpg~., data = temp2 )
summary(lm.fit4)


temp3 <- sapply(temp[2:ncol(temp)],sqrt)
temp3 <- as.data.frame(temp3)
lm.fit5 <- lm(mpg~., data = temp3 )
summary(lm.fit5)

square = function(x){
  a <- x^2
  return(a)
}
temp4 <- sapply(temp[2:ncol(temp)], square)
temp4 <- as.data.frame(temp4)
lm.fit6 <- lm(mpg~., data = temp4 )
summary(lm.fit6)
anova(lm.fit2, lm.fit4)
anova(lm.fit2, lm.fit5)
anova(lm.fit2, lm.fit6)
  #通过取对数、开方、平方，得到相应的新的回归模型，通过anova()函数进行
  #相应的比较，发现取对数模型是更加拟合的。

#第10题
###a
fit1 <- lm(Sales ~ Price + Urban + US, data = Carseats)
summary(fit1)


###b
  #Price的系数是负的，为-0.054459是显著的；
  #UrbanYes的系数是负的，为-0.021916，系数不显著；
  #USYes的系数为1.200573，正相关，系数显著。

###c
attach(Carseats)
contrasts(US)
contrasts(Urban)
  # Y=13.043469-0.054459*Price-0.021916*UrbanYes+1.200573*USYes

###d
  #Price和UrbanYes

###e
summary(fit1)
fit2 <- lm(Sales ~ Price + US, data = Carseats)
summary(fit2)

###f
  #(a)中模型的调整后的R方为0.2335，(e)中模型调整后的R方为 0.2354,拟合度略有提高。
  #(a)中模型的方差= 2.472,(e) 中模型方差= 2.469,e中的模型有更小的估计误差

###g
attach(Carseats)
confint(fit2)
  #Price 系数的95%置信区间为[-0.06475984， -0.04419543] 
  #USYes 系数的95%置信区间为[0.69151957， 1.70776632]


###h
par(mfrow = c(2, 2))
plot(fit2)
  #样本点69、377和51是离群点；样本点26、50和368是高杠杆点。
