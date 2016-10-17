
library(MASS)
Boston
?Boston
pairs(Boston)
?Boston
cor(Boston)

par(mfrow=c(1,1))
boxplot(Boston$crim)
summary(Boston$crim)

boxplot(Boston$tax)
summary(Boston$tax)

boxplot(Boston$ptratio)
summary(Boston$ptratio)

nrow(Boston[Boston$chas==1,])
median(Boston$ptratio)
Boston[Boston$medv == range(Boston$medv)[1],]
summary(Boston)

nrow(Boston[Boston$rm > 7,])
nrow(Boston[Boston$rm > 8,])
Boston[Boston$rm > 8,]

#3.15
library(MASS)
library(ISLR)

names(Boston)
attach(Boston)
summary(lm(crim~zn)) #Significant
summary(lm(crim~indus)) #Significant
summary(lm(crim~chas)) #Not Significant
summary(lm(crim~nox)) #Significant
summary(lm(crim~rm)) #Significant
summary(lm(crim~age)) #Significant
summary(lm(crim~dis)) #Significant
summary(lm(crim~rad)) #Significant
summary(lm(crim~tax)) #Significant
summary(lm(crim~ptratio)) #Significant
summary(lm(crim~black)) #Significant
summary(lm(crim~lstat)) #Significant
summary(lm(crim~medv)) #Significant

pairs(crim~zn+indus+nox+rm+age)
pairs(crim~dis+rad+tax+ptratio+black+lstat+medv)

all <- lm(crim ~ ., data=Boston)
summary(all)

summary(lm(crim~zn)) #Significant
summary(lm(crim~indus)) #Significant
summary(lm(crim~chas)) #Not Significant
summary(lm(crim~nox)) #Significant
summary(lm(crim~rm)) #Significant
summary(lm(crim~age)) #Significant
summary(lm(crim~dis)) #Significant
summary(lm(crim~rad)) #Significant
summary(lm(crim~tax)) #Significant
summary(lm(crim~ptratio)) #Significant
summary(lm(crim~black)) #Significant
summary(lm(crim~lstat)) #Significant
summary(lm(crim~medv)) #Significant

ac <- c(coefficients(lm(crim~zn))[2], coefficients(lm(crim~indus))[2], coefficients(lm(crim~chas))[2], coefficients(lm(crim~nox))[2],
        coefficients(lm(crim~rm))[2],coefficients(lm(crim~age))[2],coefficients(lm(crim~dis))[2],coefficients(lm(crim~rad))[2],
        coefficients(lm(crim~tax))[2],coefficients(lm(crim~ptratio))[2],coefficients(lm(crim~black))[2],coefficients(lm(crim~lstat))[2],
        coefficients(lm(crim~medv))[2])
bc <- coefficients(all)[-1]

plot(ac, bc, main="Plot of Coefficient Estimate for Simple and Multiple Regression", xlab="Simple Regression Estimate", 
     ylab="Multiple Regression Estimate")

summary(lm(crim ~ poly(zn, 3)))
summary(lm(crim ~ poly(indus, 3)))
summary(lm(crim ~ poly(chas, 3)))
summary(lm(crim ~ poly(nox, 3)))
summary(lm(crim ~ poly(rm, 3)))
summary(lm(crim ~ poly(age, 3)))
summary(lm(crim ~ poly(dis, 3)))
summary(lm(crim ~ poly(rad, 3)))
summary(lm(crim ~ poly(tax, 3)))
summary(lm(crim ~ poly(ptratio, 3)))
summary(lm(crim ~ poly(black, 3)))
summary(lm(crim ~ poly(lstat, 3)))
summary(lm(crim ~ poly(medv, 3)))

##########################

#4.10
library(ISLR)
head(Weekly)
summary(Weekly)
pairs(Weekly)
cor(Weekly[,-9])
names(Weekly)
log_week <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Weekly, family=binomial)
summary(log_weekly)

#4.10c
log_week_prob <- predict(log_week, data=Weekly, type="response")
glm.pred = rep("Down", nrow(Weekly))
glm.pred[log_week_prob > .5] <- "Up"
table(glm.pred, Weekly$Direction)
mean(glm.pred == Weekly$Direction)
mean(glm.pred != Weekly$Direction)

#4.10d

train <- Weekly[Weekly$Year <= 2008,]
test <- Weekly[Weekly$Year > 2008,]

log_train <- glm(Direction ~ Lag2, data=train, family=binomial)
log_prob <- predict(log_train, test, type="response")
log_pred <- rep("Down", nrow(test))
log_pred[log_prob > .5] <- "Up"
table(log_pred, test$Direction)
mean(log_pred == test$Direction)

#4.10e

lda_train <- lda(Direction ~ Lag2, data=train)
lda_pred <- predict(lda_train, test)
table(lda_pred$class, test$Direction)
mean(lda_pred$class == test$Direction)

#4.10f

qda_train <- qda(Direction ~ Lag2, data=train)
qda_pred <- predict(qda_train, test)
table(qda_pred$class, test$Direction)
mean(qda_pred$class == test$Direction)

#4.10g

library("class")
train.X <- data.frame(train$Lag2)
test.X <- data.frame(test$Lag2)
train.Direction <- train$Direction

knn.pred <- knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, test$Direction)
(21+31) / 104

#4.10i

#logistic regression

log_train <- glm(Direction ~ Lag1+Lag2, data=train, family=binomial)
log_prob <- predict(log_train, test, type="response")
log_pred <- rep("Down", nrow(test))
log_pred[log_prob > .5] <- "Up"
table(log_pred, test$Direction)
mean(log_pred == test$Direction)

log_train <- glm(Direction ~ Lag1*Lag2, data=train, family=binomial)
log_prob <- predict(log_train, test, type="response")
log_pred <- rep("Down", nrow(test))
log_pred[log_prob > .5] <- "Up"
table(log_pred, test$Direction)
mean(log_pred == test$Direction)

#LDA

lda_train <- lda(Direction ~ Lag1+Lag2, data=train)
lda_pred <- predict(lda_train, test)
table(lda_pred$class, test$Direction)
mean(lda_pred$class == test$Direction)

lda_train <- lda(Direction ~ Lag1+Lag2+I(Lag2^2), data=train)
lda_pred <- predict(lda_train, test)
table(lda_pred$class, test$Direction)
mean(lda_pred$class == test$Direction)

#QDA

qda_train <- qda(Direction ~ Lag1+Lag2, data=train)
qda_pred <- predict(qda_train, test)
table(qda_pred$class, test$Direction)
mean(qda_pred$class == test$Direction)

qda_train <- qda(Direction ~ Lag1*Lag2, data=train)
qda_pred <- predict(qda_train, test)
table(qda_pred$class, test$Direction)
mean(qda_pred$class == test$Direction)

qda_train <- qda(Direction ~ Lag2+I(Lag2^2), data=train)
qda_pred <- predict(qda_train, test)
table(qda_pred$class, test$Direction)
mean(qda_pred$class == test$Direction)

#KNN

library("class")
train.X <- data.frame(train$Lag2)
test.X <- data.frame(test$Lag2)
train.Direction <- train$Direction

knn.pred <- knn(train.X, test.X, train.Direction, k=50)
table(knn.pred, test$Direction)
(20+38) / 104

train.X <- data.frame(train$Lag1, train$Lag2)
test.X <- data.frame(test$Lag1, test$Lag2)
train.Direction <- train$Direction

knn.pred <- knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, test$Direction)
(18+32) / 104

#4.13

summary(Boston$crim) #Median is 0.25650
Boston$median <- "Above"
Boston$median[Boston$crim < 0.25650] <- "Below" 
train.sample <- sample(1:nrow(Boston), nrow(Boston)/2)
Boston.train <- Boston[train.sample,] 
Boston.test <- Boston[-train.sample,]

Boston.train$median <- as.factor(Boston.train$median)
Boston.test$median <- as.factor(Boston.test$median)

head(Boston.train)
head(Boston.test)

names(Boston.train)

#logistic regression

log_train <- glm(median ~ zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+medv, data=Boston.train, family=binomial)
log_prob <- predict(log_train, Boston.test, type="response")
contrasts(Boston.test$median)
log_pred <- rep("Above", nrow(Boston.test))
log_pred[log_prob > .5] <- "Below"
table(log_pred, Boston.test$median)
mean(log_pred == Boston.test$median)

summary(log_train) #Only keep nox, dis rad, ptratio, medv
log_train <- glm(median ~ nox+dis+rad+ptratio+medv, data=Boston.train, family=binomial)
log_prob <- predict(log_train, Boston.test, type="response")
contrasts(Boston.test$median)
log_pred <- rep("Above", nrow(Boston.test))
log_pred[log_prob > .5] <- "Below"
table(log_pred, Boston.test$median)
mean(log_pred == Boston.test$median)

#LDA

lda_train <- lda(median ~ zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+medv, data=Boston.train)
lda_pred <- predict(lda_train, Boston.test)
table(lda_pred$class, Boston.test$median)
mean(lda_pred$class == Boston.test$median)

lda_train <- lda(median ~ nox+dis+rad+ptratio+medv, data=Boston.train)
lda_pred <- predict(lda_train, Boston.test)
table(lda_pred$class, Boston.test$median)
mean(lda_pred$class == Boston.test$median)

lda_train <- lda(median ~ nox+I(nox^2)+dis+rad+ptratio+medv, data=Boston.train)
lda_pred <- predict(lda_train, Boston.test)
table(lda_pred$class, Boston.test$median)
mean(lda_pred$class == Boston.test$median)

lda_train <- lda(median ~ nox+I(nox^2)+dis+rad+ptratio+medv, data=Boston.train)
lda_pred <- predict(lda_train, Boston.test)
table(lda_pred$class, Boston.test$median)
mean(lda_pred$class == Boston.test$median)

#KNN

library("class")
train.X <- data.frame(Boston.train[,2:14])
test.X <- data.frame(Boston.test[,2:14])
train.Direction <- Boston.train$median

knn.pred <- knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Boston.test$median)
(115+113) / 253

knn.pred <- knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Boston.test$median)
(112+117) / 253

knn.pred <- knn(train.X, test.X, train.Direction, k=50)
table(knn.pred, Boston.test$median)
(81+119) / 253

head(Boston.train)
train.X <- data.frame(Boston.train[,c(5,8,9,11,14)])
test.X <- data.frame(Boston.test[,c(5,8,9,11,14)])
train.Direction <- Boston.train$median

knn.pred <- knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Boston.test$median)
(110+114)/253

