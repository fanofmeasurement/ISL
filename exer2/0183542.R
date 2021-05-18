library(ISLR)
library(caret)
library(MASS)
library(class)
head(Weekly)

# 10

# (a)
# 对Weekly数据进行数值和图像的描述性统计
plot(Weekly)
# 从图中可以看出在1990到2010年间，每次记录的today数据都比较平稳，在0左右徘徊
# 同时在1990到2010年间，五个滞后时间变量也较为平稳，在0附近左右徘徊
# 在1990到2010年间，Volume呈逐年上升状态，且上升得越来越快
summary(Weekly)
# 可以得到每个变量的中位数，均值和分位数
cor(Weekly[, -9])
# 证实上述图像描述，时间和volume具有相关性


# (b)
# 预测logsitic模型
fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
           data = Weekly, family = binomial())
summary(fit)
# 根据汇总结果可以看出，存在显著变量，只有Lag2显著


# (c)
probs <- predict(fit, type = "response")
predict <- rep("Down", length(probs))
predict[probs > 0.5] <- "Up"
table(predict, Weekly$Direction)
mean(predict == Weekly$Direction)
# 可知整体的预测准确率为0.56


# (d)
# 划分数据集
head(Weekly)
Weekly[Weekly$Year == 2008, ]
w.train <- Weekly[c(1:985), ]
w.test <- Weekly[-c(1:985), ]

# 拟合模型
fit.rep <- glm(Direction~Lag2, data = w.train, 
               family = binomial())

probs2 <- predict(fit.rep, w.test, type="response")
pred2 <- rep("Down", length(probs2))
pred2[probs2 > 0.5] <- "Up"
Direction2 <- w.test$Direction
table(pred2, Direction2)
mean(pred2 == Direction2)
# 可知整体预测准确率为0.625


# (e)
# LDA
fit_lda <- lda(Direction ~ Lag2, data = w.train)
pred_lda <- predict(fit_lda, w.test)
table(pred_lda$class, Direction2)
mean(pred_lda$class == Direction2)


# (f)
# QDA
fit_qda <- qda(Direction ~ Lag2, data = w.train)
pred_qda <- predict(fit_qda, w.test)
table(pred_qda$class, Direction2)
mean(pred_qda$class == Direction2)


# (g)
# K = 1的KNN
train <- as.matrix(w.train$Lag2)
test <- as.matrix(w.test$Lag2)
train_Direction <-w.train$Direction
set.seed(1)
pred_knn <- knn(train, test, train_Direction, k=1)
table(pred_knn, Direction2)
mean(pred_knn == Direction2)


# (h)
# 根据最终的预测准确率判断，logistic回归以及LDA的结果最好


# (i)
# 逻辑回归加lag2和lag1的交叉项
fit_glm2 <- glm(Direction ~ Lag2 + Lag2:Lag1, 
                data = w.train, family=binomial())
probs_glm2 <- predict(fit_glm2, w.test, type="response")
pred_glm2 <- rep("Down", length(probs_glm2))
pred_glm2[probs_glm2 > 0.5] = "Up"
table(pred_glm2, Direction2)
mean(pred_glm2 == Direction2)

# LDA关于lag2和lag3的交叉项
fit_lda2 <- lda(Direction ~ Lag2:Lag3, data = w.train)
pred_lda2 <- predict(fit_lda2, w.test)
table(pred_lda2$class, Direction2)
mean(pred_lda2$class == Direction2)

# QDA对lag2进行变换
fit_qda2 <- qda(Direction ~ sqrt(abs(Lag2)), data = w.train)
pred_qda2 <- predict(fit_qda2, w.test)
table(pred_qda2$class, Direction2)
mean(pred_qda2$class == Direction2)

# KNN中K = 100
pred_knn <- knn(train, test, train_Direction, k=100)
table(pred_knn, Direction2)
mean(pred_knn == Direction2)

# 从上述试验中可以得出，最初的逻辑回归和LDA结果最好

###################################

# 11

# (a)
# 加入二分变量
Auto$mpg01[Auto$mpg > median(Auto$mpg)] <- 1
Auto$mpg01[Auto$mpg < median(Auto$mpg)] <- 0
Auto <- Auto[, -1]
head(Auto)


# (b)
plot(Auto)
cor(Auto[, -8])
# 从散点图以及协方差矩阵可以看出cylinders, displacement, horsepower以及
# weight与mpg01的相关性较高


# (c)
# 划分数据集
set.seed(1234)
train <- sample(nrow(Auto), 0.7*nrow(Auto))
a.train <- Auto[train, ]
a.test <- Auto[-train, ]


# (d)
fit.lda <- lda(mpg01 ~ cylinders + displacement + horsepower + weight, 
               data = a.train)
pred.lda <- predict(fit.lda, a.test)
mean(pred.lda$class != a.test$mpg01)
# 测试误差为0.14


# (e)
fit.qda <- qda(mpg01 ~ cylinders + displacement + horsepower + weight, 
               data = a.train)
pred.qda <- predict(fit.qda, a.test)
mean(pred.qda$class != a.test$mpg01)
# 测试误差为0.12


# (f)
fit.glm <- glm(mpg01 ~ cylinders + displacement + horsepower + weight, 
               data = a.train, family=binomial())
probs.glm <- predict(fit.glm, a.test, type = "response")
pred.glm <- rep(0, length(probs.glm))
pred.glm[probs.glm > 0.5] = 1
mean(pred.glm != a.test$mpg01)
# 测试误差为0.46


# (g)
attach(a.train)
train_knn <- cbind(cylinders, weight, displacement, horsepower)
detach(a.train)

attach(a.test)
test_knn <- cbind(cylinders, weight, displacement, horsepower)
detach(a.test)

pred.knn <- knn(train_knn, test_knn, a.train$mpg01, k = 1) 
mean(pred.knn != a.test$mpg01)   # 0.16

pred.knn <- knn(train_knn, test_knn, a.train$mpg01, k = 10) 
mean(pred.knn != a.test$mpg01)   # 0.11

pred.knn <- knn(train_knn, test_knn, a.train$mpg01, k = 20) 
mean(pred.knn != a.test$mpg01)   # 0.16

pred.knn <- knn(train_knn, test_knn, a.train$mpg01, k = 100) 
mean(pred.knn != a.test$mpg01)   # 0.16

# k = 10的时候效果最好







