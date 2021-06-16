library(ISLR)
library(class)
library(MASS)
#10.
View(Weekly)
summary(Weekly)
t <- Weekly[,-9]
pairs(t)
cor(t) #在数据框中删除第九列，绘制散点图并计算相关系数。
#(a)由上，关系并不显著，此外发现Volume与Year相关性较强。

#(b)
attach(Weekly)
reg_1b = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Weekly,
             family=binomial)
summary(reg_1b)
#Lag2的P值小于给定的显著性水平，故在统计意义上显著；同时常数的p<0.05，同样显著。

# (c)
glm.probs_1 = predict(reg_1b, type="response")
glm.pred=rep("Down", length(glm.probs_1))
glm.pred[glm.probs_1 > 0.5]="Up"
table(glm.pred, Direction)
mean(glm.pred == Direction)
#由以上可知，整体预测准确率为：56.11%；
#up正确率为557/(48 + 557) = 92.01%，down正确率为54/(54 + 430) =11.16%。

# (d)
train = (Year < 2009)
test = Weekly[!train,]  
reg_1d = glm(Direction~Lag2, data=Weekly, family=binomial, subset=train)
glm.probs_2 = predict(reg_1d ,test, type="response")
glm.pred = rep("Down", length(glm.probs_2))
glm.pred[glm.probs_2 > 0.5] <- "Up"
Direction_test = Direction[!train]
table(glm.pred, Direction_test)
mean(glm.pred == Direction_test)
#测试集预测准确率为62.5%。

# (e)
lda_1e = lda(Direction ~ Lag2, data=Weekly, subset=train)
lda.pred = predict(lda_1e, test)
table(lda.pred$class,Direction_test)
mean(lda.pred$class == Direction_test) #LDA

#(f)
qda_1f = qda(Direction~Lag2, data=Weekly, subset=train)
qda_class = predict(qda_1f, test)$class
table(qda_class, Direction_test)
mean(qda_class==Direction_test) #QDA

# (g)
train_X = as.matrix(Lag2[train])
test_X = as.matrix(Lag2[!train])
train_Direction = Direction[train]
set.seed(1314)
knn_pred = knn(train_X, test_X, train_Direction, k=1)
table(knn_pred, Direction_test)
mean(knn_pred == Direction_test)
#逻辑斯蒂回归和LDA具有最高的准确率,都为62.5%,而QDA的准确率稍低。


# (i)
glm.fit = glm(Direction ~ Lag2:Lag1, data=Weekly, family=binomial, subset=train)
glm.probs = predict(glm.fit, test, type="response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs>0.5]="Up"
Direction_test = Direction[!train]
table(glm.pred, Direction_test)
mean(glm.pred == Direction_test) #Logstic

lda.fit =lda(Direction ~ Lag2:Lag1,data = Weekly, subset=train)
lda.pred = predict(lda.fit, test)
table(lda.pred$class, Direction_test)
mean(lda.pred$class == Direction_test) #LDA

qda.fit = qda(Direction~Lag2+sqrt(abs(Lag2)), data=Weekly, subset=train)
qda_class = predict(qda.fit, test)$class
table(qda_class,Direction_test)
mean(qda_class==Direction_test) #QDA，含sqrt(abs(Lag2))


knn_pred = knn(train_X,test_X,train_Direction,k=10)
table(knn_pred,Direction_test)
mean(knn_pred==Direction_test)  #K=10时
# LDA和逻辑斯蒂回归仍然预测准确率更高。


# 11
#(a)
attach(Auto)
View(Auto)
mpg01 = rep(0,length(mpg))
mpg01[mpg > median(mpg)] <- 1
mpg01[mpg < median(mpg)] <-0
Auto_cal = cbind(Auto, mpg01) #构建{0,1}集合

# (b)
cor(Auto_cal[,-9])
pairs(Auto_cal[,-9])
boxplot(Auto_cal[,-9])
# cylinders, displacement, horsepower, weight与mpg01相关性较强。 

# (c)
train=(year%%2 == 0)
test=!train
Auto.train = Auto[train,]
Auto.test = Auto[test,]
a.test = mpg01[test] #构建数据集

# (d)
lda.fit=lda(mpg01~ cylinders + displacement + horsepower + weight, data=Auto, subset=train) #LDA
lda.pred=predict(lda.fit, Auto.test)
mean(lda.pred$class != a.test)
#LDA测试误差为12.64%。


# (e)
qda.fit=qda(mpg01~cylinders+displacement+horsepower+weight,data=Auto,subset=train)
qda.pred=predict(qda.fit,Auto.test) #QDA
mean(qda.pred$class!=a.test)
#QDA测试误差为13.19%。.

# (f)
glm.fit=glm(mpg01~cylinders+displacement+horsepower+weight,data=Auto,family=binomial,subset=train)#建立Logstic
glm.probs=predict(glm.fit,Auto.test,type="response")
glm.pred=rep(0,length(glm.probs))
glm.pred[glm.probs>0.5]=1
mean(glm.pred!=a.test)
#逻辑斯蒂回归模型得到的测试误差为12.09%。


# (g)
library(class)
train.X=cbind(cylinders,displacement,horsepower,weight)[train,]
test.X=cbind(cylinders,displacement,horsepower,weight)[test,]
train.mpg01=mpg01[train]
set.seed(1)
#k=1
knn.pred=knn(train.X,test.X,train.mpg01,k=1) 
mean(knn.pred != a.test)
#k=5
knn.pred=knn(train.X,test.X,train.mpg01,k=5) 
mean(knn.pred != a.test) 
#k=10
knn.pred=knn(train.X,test.X,train.mpg01,k=10) 
mean(knn.pred != a.test)
#误差分别为15.38%，14.84%，15.38%，故K=5时在测试集上误差最小。

