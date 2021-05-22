#10.
library(ISLR)
summary(Weekly)
#绘制散点矩阵图
pairs(Weekly[,-9])
#相关系数
cor(Weekly[,-9])
## (a)
#滞后时间变量Lag1~Lag5之间没有显著性关系

attach(Weekly)
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Weekly,family=binomial)
#建立logistics回归模型
summary(glm.fit)
## (b)
#Lag2有统计显著性，p值为0.0296<0.05.

glm.probs=predict(glm.fit,type="response")
glm.pred=rep("Down",length(glm.probs))
glm.pred[glm.probs>0.5]="Up"
table(glm.pred,Direction)
mean(glm.pred==Direction)

## (c)
# 整体预测准确率为:0.56
# 增加时预测的正确率为:0.921
# 减少时预测正确率为：0.112

train=(Year<2009)
Weekly.t=Weekly[!train,]
glm.fit=glm(Direction~Lag2,data=Weekly,family=binomial,subset=train)
#建立logistics回归模型
glm.probs=predict(glm.fit,Weekly.t,type="response")
glm.pred=rep("Down",length(glm.probs))
glm.pred[glm.probs>0.5]="Up"
Direction.t=Direction[!train]
table(glm.pred,Direction.t)
mean(glm.pred==Direction.t)
## (d)
#测试集中总体预测准确率为：0.625
library(MASS)
lda.fit=lda(Direction~Lag2,data=Weekly,subset=train)
#LDA模型
lda.pred=predict(lda.fit,Weekly.t)
table(lda.pred$class,Direction.t)
mean(lda.pred$class == Direction.t)
## (e)
#QDA模型
qda.fit=qda(Direction~Lag2,data=Weekly,subset=train)
qda.class=predict(qda.fit,Weekly.t)$class
table(qda.class,Direction.t)
mean(qda.class==Direction.t)

## (f)
library(class)
train.X=as.matrix(Lag2[train])
test.X=as.matrix(Lag2[!train])
train.Direction=Direction[train]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.t)
mean(knn.pred==Direction.t)

## (g)
#K=1的KNN过程如上。

## (h)
#从预测准确率来看，logistics回归和LDA具有最高的正确率0.625.

## (i)
### logistics回归 Lag2与Lag1相关：
glm.fit=glm(Direction~Lag2:Lag1,data=Weekly,family=binomial,subset=train)
glm.probs=predict(glm.fit,Weekly.t,type="response")
glm.pred=rep("Down",length(glm.probs))
glm.pred[glm.probs>0.5]="Up"
Direction.t=Direction[!train]
table(glm.pred,Direction.t)
mean(glm.pred==Direction.t)

### LDA Lag2与Lag1相关：
lda.fit=lda(Direction~Lag2:Lag1,data=Weekly,subset=train)
lda.pred=predict(lda.fit,Weekly.t)
table(lda.pred$class,Direction.t)
mean(lda.pred$class==Direction.t)


### QDA Lag2与sqrt(abs(Lag2))：
qda.fit=qda(Direction~Lag2+sqrt(abs(Lag2)),data=Weekly,subset=train)
qda.class=predict(qda.fit,Weekly.t)$class
table(qda.class,Direction.t)
mean(qda.class==Direction.t)


### K=10：
knn.pred=knn(train.X,test.X,train.Direction,k=10)
table(knn.pred,Direction.t)
mean(knn.pred==Direction.t)


### K=100：
knn.pred=knn(train.X,test.X,train.Direction,k=100)
table(knn.pred,Direction.t)
mean(knn.pred==Direction.t)
#logistics回归的预测正确率最高,效果最好。

detach(Weekly)
# 11.
library(ISLR)
summary(Auto)
attach(Auto)
mpg01=rep(0,length(mpg))
mpg01[mpg>median(mpg)]=1
#1为中位数以上
Auto=data.frame(Auto,mpg01)

## (a)
cor(Auto[,-9])
pairs(Auto[,-9])
boxplot(Auto[,-9])

## (b)
#可以看出cylinders, displacement, horsepower, weight与mpg01有很大的可能是相关的。 

train=(year%%2==0)
#按照年份进行分组
test=!train
Auto.train=Auto[train,]
Auto.test=Auto[test,]
mpg01.test=mpg01[test]

## (c)
#把数据一分为二作为训练集和测试集，通过年份奇数偶数很容易做到。

library(MASS)
lda.fit=lda(mpg01~cylinders+displacement+horsepower+weight,data=Auto,subset=train)
#建立LDA模型
lda.pred=predict(lda.fit,Auto.test)
mean(lda.pred$class!=mpg01.test)

## (d)
#LDA模型得到的测试误差为0.126.
qda.fit=qda(mpg01~cylinders+displacement+horsepower+weight,data=Auto,subset=train)
#建立QDA模型
qda.pred=predict(qda.fit,Auto.test)
mean(qda.pred$class!=mpg01.test)
## (e)
#QDA模型得到的测试误差为0.132.

#建立Logistics回归模型
glm.fit=glm(mpg01~cylinders+displacement+horsepower+weight,data=Auto,family=binomial,subset=train)
glm.probs=predict(glm.fit,Auto.test,type="response")
glm.pred=rep(0,length(glm.probs))
glm.pred[glm.probs>0.5]=1
mean(glm.pred!=mpg01.test)

## (f)
#logistics回归模型得到的测试误差为0.121.

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
# k=1时，模型得到的测试误差为0.154;
# k=5时，模型得到的测试误差为0.148;
# k=10时，模型得到的测试误差为0.154;
# k=5时，模型测试误差最小,KNN效果最好。
