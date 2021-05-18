library(ISLR)
attach(Auto)
a <- lm(mpg ~ horsepower, data = Auto)
summary(a)
predict(a, data.frame(horsepower = c(98)), interval = "confidence")
predict(a, data.frame(horsepower = c(98)), interval = "prediction")
plot(horsepower, mpg)
abline(a)
par(mfrow = c(2, 2))
plot(a)







pairs(Auto)
View(Auto)
s <- Auto[,-ncol(Auto) ]
cor(s)
p2 <- lm(mpg~., data = s)
summary(p2)
plot(p2)

p3 <- lm(mpg~(cylinders + displacement + horsepower + weight + acceleration + year + origin)^2, data = s)
summary(p3)


s2 <- sapply(s[2:ncol(s)],log)
s2 <- as.data.frame(s2)
p4 <- lm(mpg~., data = s2 )
summary(p4)


s3 <- sapply(s[2:ncol(s)],sqrt)
s3 <- as.data.frame(s3)
p5 <- lm(mpg~., data = s3 )
summary(p5)

func = function(x){
  a <- x^2
  return(a)
}
s4 <- sapply(s[2:ncol(s)], func)
s4 <- as.data.frame(s4)
p6 <- lm(mpg~., data = s4 )
summary(p6)




fit1 <- lm(Sales ~ Price + Urban + US, data = Carseats)
summary(fit1)


fit2 <- lm(Sales ~ Price + US, data = Carseats)
summary(fit2)
attach(Carseats)
confint(fit2)
8.
(a) i mpg和horse是相关的。
ii两者之间有较强的相关性。
iii两者是负相关。
iv 预测值为24.46708，95%置信区间为【23.97308，24.96108】，预测区间为【14.8094，34.12476】
(c)由四个诊断图可以看出，第一个图表明残差散点图并非线性的，这说明我们应该拟合一个非线性模型更符合；第二个图QQ图说明残差大部分接近直线，说明残差接近正态分布；第三个图说明有些马力的数值会独立于变量mpg；第四个图看出指数为117和84的两个样本点对回归模型中的系数影响最大。
9.
(c) 
i 各个预测变量都与响应变量有一定的关系。
Ii  weight, year, origin三个预测变量。
Iii  year对mpg具有正相关关系，并且对mpg有比较大的影响。说明时间过得越久，随着年份增加，汽车的马力会越来越猛。
(d) 在样本点中第323和第327个数据具有非常大的残差，是离群点，第14个样本点是杠杆点。
(e) 相互作用显著的组合是马力：质量，加速度:马力和加速度:重量。添加交互项后，模型拟合度提高。
(f) 都很好，但是使用平方项作为预测变量时，对响应变量的拟合程度最好 
10.
(b) 这表明价格系数是负的，并且是显著的。UrbanYes系数为负，但并不显著。US的系数为正，也较为显著。
(c) Y=13.043469-0.054459*Price-0.021916*UrbanYes+1.200573*USYes
(注意在不在城市取UrbanYes 1或0；在不在美国则USYes取1或0)
(d) Price 和 USYes 
(f) (a)中模型的R^2= 0.2393，同时方差= 2.472；(e) 中模型 R^2= 0.2393，方差(g) Price 系数的置信区间为【-0.06475984， -0.04419543】 USYes 系数的置信区间为【0.69151957， 1.70776632】
(h)样本点69、377和51是离群点；样本点26、50和368是高杠杆点。
