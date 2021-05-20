#10、
library(ISLR)
View(Weekly)
summary(Weekly)
#把第九列删除
temp <- Weekly[,-9]
#绘制散点矩阵图
pairs(temp)
#计算相关系数
cor(temp)
#  (a)
#从散点矩阵图和相关系数矩阵可以看出：滞后时间变量Lag1~Lag5之间没有显著性关系，
#但交易量Volume随时间Year相关系数比较大，相关性比较强。

#  (b)
#建立逻辑斯蒂回归模型
attach(Weekly)
huigui_1 = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Weekly,
               family=binomial)
summary(huigui_1)
#从P值来看，Lag2在统计上显著，P值=0.0296<0.05,
#Intercept在统计上也显著，P值=0.0019 < 0.05。

#  (c)
glm.probs_1 = predict(huigui_1, type="response")
glm.pred=rep("Down", length(glm.probs_1))
glm.pred[glm.probs_1 > 0.5]="Up"
table(glm.pred, Direction)
mean(glm.pred == Direction)
#整体预测准确率为：0.56
#当预测Direction增加(up)时正确率为:
#557/(48 + 557) = 0.92
#当预测Direction减少(down)时正确率为：
#54/(54 + 430) = 0.11


#  (d)
#选择1990-2008年为训练集
train = (Year < 2009)
#选择剩下年份为训练集
test = Weekly[!train,]
#建立逻辑斯蒂回归模型
huigui_2 = glm(Direction~Lag2, data=Weekly, family=binomial, subset=train)
glm.probs_2 = predict(huigui_2 ,test, type="response")
glm.pred = rep("Down", length(glm.probs_2))
glm.pred[glm.probs_2 > 0.5] <- "Up"
Direction_test = Direction[!train]
table(glm.pred, Direction_test)
mean(glm.pred == Direction_test)
#测试集中总体预测准确率为：0.625

#   (e)
library(MASS)
#建立LDA模型
huigui_lda = lda(Direction ~ Lag2, data=Weekly, subset=train)
lda.pred = predict(huigui_lda, test)
table(lda.pred$class,Direction_test)
mean(lda.pred$class == Direction_test)
  
#   (f)
#建立QDA模型
huigui_qda = qda(Direction~Lag2, data=Weekly, subset=train)
qda_class = predict(huigui_qda, test)$class
table(qda_class, Direction_test)
mean(qda_class==Direction_test)

#   (g)
library(class)
train_X = as.matrix(Lag2[train])
test_X = as.matrix(Lag2[!train])
train_Direction = Direction[train]
set.seed(1314)
knn_pred = knn(train_X, test_X, train_Direction, k=1)
table(knn_pred, Direction_test)
mean(knn_pred == Direction_test)

#从预测准确率来看，逻辑斯蒂回归和LDA具有最高的准确率,都为62.5%。


#  (i)
### 逻辑斯蒂回归 Lag2与Lag1相关：
glm.fit = glm(Direction ~ Lag2:Lag1, data=Weekly, family=binomial, subset=train)
glm.probs = predict(glm.fit, test, type="response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs>0.5]="Up"
Direction_test = Direction[!train]
table(glm.pred, Direction_test)
mean(glm.pred == Direction_test)

### LDA Lag2与Lag1相关：
lda.fit =lda(Direction ~ Lag2:Lag1,data = Weekly, subset=train)
lda.pred = predict(lda.fit, test)
table(lda.pred$class, Direction_test)
mean(lda.pred$class == Direction_test)


### QDA Lag2与sqrt(abs(Lag2))：
qda.fit = qda(Direction~Lag2+sqrt(abs(Lag2)), data=Weekly, subset=train)
qda_class = predict(qda.fit, test)$class
table(qda_class,Direction_test)
mean(qda_class==Direction_test)


### K=10：
knn_pred = knn(train_X,test_X,train_Direction,k=10)
table(knn_pred,Direction_test)
mean(knn_pred==Direction_test)
#综合比较预测准确率，依旧是LDA和逻辑斯蒂回归正确率更高。


#    (11)
## (a)
#按照mpg中位数构建好0，1数据集。
library(ISLR)
attach(Auto)
View(Auto)
temp = rep(0,length(mpg))
temp[mpg > median(mpg)] <- 1
#1为中位数以上
Auto = data.frame(Auto, temp)

## (b)
cor(Auto[,-9])
pairs(Auto[,-9])
boxplot(Auto[,-9])
#可以看出cylinders, displacement, horsepower, weight与mpg01有很大相关系数。 

## (c)
train=(year%%2 == 0)
#按照年份进行分组，
#我们简单地把数据一分为二作为训练集和测试集,（通过年份奇数偶数）
test=!train
Auto.train = Auto[train,]
Auto.test = Auto[test,]
temp.test = temp[test]

## (d)
library(MASS)
lda.fit=lda(mpg01~ cylinders + displacement + horsepower + weight, data=Auto, subset=train)
#建立LDA模型
lda.pred=predict(lda.fit, Auto.test)
mean(lda.pred$class != temp.test)
#LDA模型得到的测试误差为0.126.


## (e)
#建立QDA模型
qda.fit=qda(mpg01~cylinders+displacement+horsepower+weight,data=Auto,subset=train)
qda.pred=predict(qda.fit,Auto.test)
mean(qda.pred$class!=mpg01.test)
#QDA模型得到的测试误差为0.132.

## (f)
glm.fit=glm(mpg01~cylinders+displacement+horsepower+weight,data=Auto,family=binomial,subset=train)
#建立逻辑斯蒂回归模型
glm.probs=predict(glm.fit,Auto.test,type="response")
glm.pred=rep(0,length(glm.probs))
glm.pred[glm.probs>0.5]=1
mean(glm.pred!=mpg01.test)
#逻辑斯蒂回归模型得到的测试误差为0.121.


## (g)
library(class)
train.X=cbind(cylinders,displacement,horsepower,weight)[train,]
test.X=cbind(cylinders,displacement,horsepower,weight)[test,]
train.mpg01=mpg01[train]
set.seed(1)
#k=1
knn.pred=knn(train.X,test.X,train.mpg01,k=1) 
mean(knn.pred != mpg01.test)
#k=5
knn.pred=knn(train.X,test.X,train.mpg01,k=5) 
mean(knn.pred != mpg01.test) 
#k=10
knn.pred=knn(train.X,test.X,train.mpg01,k=10) 
mean(knn.pred != mpg01.test) 
#k=1时，模型得到的测试误差为0.154;
#k=5时，模型得到的测试误差为0.148;
#k=10时，模型得到的测试误差为0.154;
#比较测试误差发现k=5时，模型测试误差最小,KNN效果最好。







