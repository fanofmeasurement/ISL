# Q10.

library(ISLR)
#计算相关系数矩阵，绘制散点矩阵图
summary(Weekly)
pairs(Weekly[,-9])
cor(Weekly[,-9])

# (a)
#由相关系数矩阵和散点矩阵图可以看出：滞后时间变量Lag1~Lag5之间没有显著性关系，交易量Volume随时间Year不断有明显的增加。

#建立逻辑斯蒂回归模型
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Weekly,family=binomial)

summary(glm.fit)

# (b)
#变量Lag2在统计上显著,p-value=0.0296<0.05.


glm.probs=predict(glm.fit,type="response")
glm.pred=rep("Down",length(glm.probs))
glm.pred[glm.probs>0.5]="Up"
table(glm.pred,Direction)

# (c)
#整体预测准确率为：
#  {54+557}/{54+48+430+557}=56% 
#  当预测weak增加时正确率为:
#  {557}/{557+48}=92.1% 
#  当预测weak减少时正确率为：
#  {54}/{430+54}=11.2%
 
  

train=(Year<2009)
Weekly.0910=Weekly[!train,]
glm.fit=glm(Direction~Lag2,data=Weekly,family=binomial,subset=train)
glm.probs=predict(glm.fit,Weekly.0910,type="response")
glm.pred=rep("Down",length(glm.probs))
glm.pred[glm.probs>0.5]="Up"
Direction.0910=Direction[!train]
table(glm.pred,Direction.0910)
mean(glm.pred==Direction.0910)

# (d)
#测试集中总体预测准确率为：
# {9+56}/{9+5+34+56}=62.5%
  
## (e)  
#建立LDA模型
library(MASS)
lda.fit=lda(Direction~Lag2,data=Weekly,subset=train)
lda.pred=predict(lda.fit,Weekly.0910)
table(lda.pred$class,Direction.0910)
mean(lda.pred$class == Direction.0910)

## (f)
#建立QDA模型
qda.fit=qda(Direction~Lag2,data=Weekly,subset=train)
qda.class=predict(qda.fit,Weekly.0910)$class
table(qda.class,Direction.0910)
mean(qda.class==Direction.0910)

# (g)
library(class)
train.X=as.matrix(Lag2[train])
test.X=as.matrix(Lag2[!train])
train.Direction=Direction[train]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.0910)
mean(knn.pred==Direction.0910)

# (h)
#从预测准确率来看，逻辑斯蒂回归和LDA具有最高的正确率0.625.

# (i)
# 逻辑斯蒂回归中 Lag2与Lag1相关

glm.fit=glm(Direction~Lag2:Lag1,data=Weekly,family=binomial,subset=train)
glm.probs=predict(glm.fit,Weekly.0910,type="response")
glm.pred=rep("Down",length(glm.probs))
glm.pred[glm.probs>0.5]="Up"
Direction.0910=Direction[!train]
table(glm.pred,Direction.0910)
mean(glm.pred==Direction.0910)

# LDA中 Lag2与Lag1相关

lda.fit=lda(Direction~Lag2:Lag1,data=Weekly,subset=train)
lda.pred=predict(lda.fit,Weekly.0910)
table(lda.pred$class,Direction.0910)
mean(lda.pred$class==Direction.0910)



# QDA中 Lag2与sqrt(abs(Lag2))相关

qda.fit=qda(Direction~Lag2+sqrt(abs(Lag2)),data=Weekly,subset=train)
qda.class=predict(qda.fit,Weekly.0910)$class
table(qda.class,Direction.0910)
mean(qda.class==Direction.0910)

#分类器中K值分别取10,100

# K=10：

knn.pred=knn(train.X,test.X,train.Direction,k=10)
table(knn.pred,Direction.0910)
mean(knn.pred==Direction.0910)



# K=100：

knn.pred=knn(train.X,test.X,train.Direction,k=100)
table(knn.pred,Direction.0910)
mean(knn.pred==Direction.0910)

#根据预测准确率，（d)和(e)中的LDA和逻辑斯蒂回归正确率最高。




# Q11.

library(ISLR)
summary(Auto)
attach(Auto)
mpg01=rep(0,length(mpg))
mpg01[mpg>median(mpg)]=1
Auto=data.frame(Auto,mpg01)

# (a)
#已按照mpg中位数构建好0，1数据集。


cor(Auto[,-9])
pairs(Auto[,-9])
boxplot(Auto[,-9])


# (b)
#根据散点图和箱线图，cylinders, displacement, horsepower, weight与mpg01有很大的可能是相关的。 


train=(year%%2==0)
test=!train
Auto.train=Auto[train,]
Auto.test=Auto[test,]
mpg01.test=mpg01[test]

# (c)
#运用二分法，以年份偶数和奇数，分别作为训练集和测试集。


library(MASS)
lda.fit=lda(mpg01~cylinders+displacement+horsepower+weight,data=Auto,subset=train)
lda.pred=predict(lda.fit,Auto.test)
mean(lda.pred$class!=mpg01.test)

# (d)
#LDA模型的测试误差为0.1263736.


qda.fit=qda(mpg01~cylinders+displacement+horsepower+weight,data=Auto,subset=train)
qda.pred=predict(qda.fit,Auto.test)
mean(qda.pred$class!=mpg01.test)

# (e)
# QDA模型的测试误差为0.1318681.


glm.fit=glm(mpg01~cylinders+displacement+horsepower+weight,data=Auto,family=binomial,subset=train)
glm.probs=predict(glm.fit,Auto.test,type="response")
glm.pred=rep(0,length(glm.probs))
glm.pred[glm.probs>0.5]=1
mean(glm.pred!=mpg01.test)

# (f)
#逻辑斯蒂回归的测试误差为0.1208791.


# (g)

library(class)
train.X=cbind(cylinders,displacement,horsepower,weight)[train,]
test.X=cbind(cylinders,displacement,horsepower,weight)[test,]
train.mpg01=mpg01[train]
set.seed(1)

#k=1时
knn.pred=knn(train.X,test.X,train.mpg01,k=1) 
mean(knn.pred != mpg01.test)

#k=5时
knn.pred=knn(train.X,test.X,train.mpg01,k=5) 
mean(knn.pred != mpg01.test) 

#k=10时
knn.pred=knn(train.X,test.X,train.mpg01,k=10) 
mean(knn.pred != mpg01.test) 

#k=100时
knn.pred=knn(train.X,test.X,train.mpg01,k=100) 
mean(knn.pred != mpg01.test) 

#k=1时，模型的测试误差为0.0.1538462;
#k=5时，模型的测试误差为0.1483516;
#k=10时，模型的测试误差为0.1593407;
#k=100时，模型的测试误差为0.1428571;
#k=5时，模型测试误差最小,KNN效果最好。
