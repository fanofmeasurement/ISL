# ---
# title: "exercise"
# author: "XWL"
# date: "2021/4/7"
# ---
  


library(ISLR)
names(Auto)

# 8.
## a.
lm.fit = lm(mpg ~ horsepower, data = Auto)
summary(lm.fit)
predict(lm.fit, data.frame(horsepower = c(98)), interval = "confidence")
predict(lm.fit, data.frame(horsepower = c(98)), interval = "prediction")


# # a.
# 
# 1.模型中系数的p值都是显著的，说明响应变量mpg和预测变量horsepower存在相关关系。
# 
# 2.R方的值为0.6059，说明两者之间有较强的相关性。
# 
# 3.horsepower的系数为-0.157845，说明两者之间是负相关。
# 
# 4.当马力为98时，mpg(油耗)的预测值为24.46708，相应的95%置信区间为(23.97308，24.96108)，
# 预测区间为(14.8094，34.12476)


## b.
attach(Auto)
plot(horsepower, mpg)
abline(lm.fit)



## c.
par(mfrow = c(2, 2))
plot(lm.fit)


# # c.
# 
# 
# 1.第一个图，残差图呈现为U形，表明数据之间存在非线性关系，说明用非线性模型进行拟合更符合；
# 
# 2.第二个图，QQ图说明残差大部分接近直线，说明残差接近正态分布；
# 
# 3.第三个图，可以更方便地看出误差分布的范围,不存在异方差；
# 
# 4.第四个图,117和94两个样本点对回归模型中的系数影响最大。



# 9.
## a.
pairs(Auto)
## b.
data1 <- Auto[,-ncol(Auto) ]
cor(data1)
## c.
lm.fit2 <- lm(mpg~., data = data1)
summary(lm.fit2)



# c.
# 
# 1.各个预测变量与响应变量具有一定的关系。
# 
# 2.displacement、weight、year、origin三个预测变量。
# 
# 3.year对mpg具有正相关关系，并且对mpg有比较大的影响。
# 系数为0.750773，说明年份与马力呈正相关，随着年份增加，汽车的马力会越来越足。



## d.
par(mfrow = c(2, 2))
plot(lm.fit2)



# d.

# 在样本点中第323、第326、第327个数据具有非常大的残差，是离群点，第14个样本点是杠杆点。



## e.
lm.fit3 <- lm(mpg ~ .+ horsepower:weight , data = data1)
summary(lm.fit3)
anova(lm.fit2, lm.fit3)


# e.

# 通过添加horsepower与weight的交互项，系数是显著的。
# 
# 再次通过anova()函数判断两个回归模型，F = 111.88，远远优于交互项的模型。


## f.
data2 <- sapply(data1[2:ncol(data1)],log)
data2 <- as.data.frame(data2)
lm.fit4 <- lm(mpg~., data = data2 )
summary(lm.fit4)


data3 <- sapply(data1[2:ncol(data1)],sqrt)
data3 <- as.data.frame(data3)
lm.fit5 <- lm(mpg~., data = data3 )
summary(lm.fit5)

square = function(x){
  return(x^2)
}

data4 <- sapply(data1[2:ncol(data1)], square)
data4 <- as.data.frame(data4)

lm.fit6 <- lm(mpg~., data = data4 )
summary(lm.fit6)
anova(lm.fit2, lm.fit4)
anova(lm.fit2, lm.fit5)
anova(lm.fit2, lm.fit6)


# f.

# 对预测变量经过不同的非线性变换，如取对数、开方、平方，得到相应的新的回归模型，通过anova()函数进行
# 相应的比较，发现取对数模型是更加拟合的。



#第10题
###a
fit1 <- lm(Sales ~ Price + Urban + US, data = Carseats)
summary(fit1)


# b.
# Price的系数是-0.054459,负相关，是显著的；
# 
# UrbanYes的系数是-0.021916，负相关，系数不显著；
# 
# USYes的系数是1.200573，正相关，系数显著。


## c.
attach(Carseats)
contrasts(US)
contrasts(Urban)


# c.
# Y=13.043469-0.054459 * Price-0.021916 \times UrbanYes+1.200573 * USYes
   
# d.
# 通过p值的大小，判断出Price和UrbanYes可以拒绝零假设


## e.
summary(fit1)
fit2 <- lm(Sales ~ Price + US, data = Carseats)
summary(fit2)


# f.

# (a)中模型的调整后的R方为0.2335，(e)中模型调整后的R方为 0.2354,拟合度略有提高。
 
# (a)中模型的方差= 2.472,(e) 中模型方差= 2.469,e中的模型有更小的估计误差。


##g.
attach(Carseats)
confint(fit2)

# g.

# Price 系数的95%置信区间为[-0.06475984， -0.04419543]
# USYes 系数的95%置信区间为[0.69151957， 1.70776632]

## h.
par(mfrow = c(2, 2))
plot(fit2)


# h.
# 通过图像，发现样本点69、377和51是离群点；样本点26、50和368是高杠杆点。

  
  
  
  
