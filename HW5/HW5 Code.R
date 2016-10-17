#Problem 1

class_error <- Vectorize(function(x) 1-max(x,(1-x)))
gini_index <- function(x) x*(1-x)+x*(1-x)
cross_entropy <- Vectorize(function(x) -(x*log(x)+(1-x)*log(1-x)))

curve(cross_entropy, 0, 1, xlab="pm1", ylab="value", main="Value of Gini Index, Classification Error, and Entropy")
curve(gini_index, 0, 1, add=T, col="red")
curve(class_error, 0, 1, add=T, col="blue")

legend("topright", c("Class Error","Gini Index","Cross Entropy"), lty=c(1,1),
       lwd=c(2.5,2.5),col=c("Blue","Red","Black"))

#Problem 2 

library(ISLR)
attach(Carseats)
set.seed(1)
train <- sample(1:nrow(Carseats), 200)

#Part a
library(tree)
carseats.tree = tree(Sales~., Carseats, subset=train)
plot(carseats.tree)
text(p2a, cex=.8, pretty=0)
summary(carseats.tree)

carseats.test <- Carseats[-train,]$Sales
yhat <- predict(carseats.tree, newdata=Carseats[-train,])
mean((yhat-carseats.test)^2)
#MSE 2.793222

#Part b
#Prune the Tree
cv.carseats=cv.tree(carseats.tree)
plot(cv.carseats$size, cv.carseats$dev, type='b')

#Prune tree using prune.tree() function
carseats.prune=prune.tree(carseats.tree, best=2)
plot(carseats.prune)
text(carseats.prune, pretty=0)
yhat <- predict(carseats.prune, newdata=Carseats[-train,])
mean((yhat-carseats.test)^2)
#MSE 2.8444887, no pruning did not decrease the test MSE

#Part c
library(randomForest)
set.seed(1)
bag.carseats = randomForest(Sales~., data=Carseats, subset=train, mtry=11,
                            importance=TRUE)
yhat <- predict(bag.carseats, newdata=Carseats[-train,])
mean((yhat-carseats.test)^2)
#MSE 1.944736
importance(bag.carseats)
varImpPlot(bag.carseats)
  
#Part d
library(gbm)

set.seed(1)
store <- data.frame(depth=seq(1:30),n.trees=rep(NA,30), cv.error=rep(NA,30))
for (i in 1:30){
  boost.carseats=gbm(Sales~., data=Carseats[train,], distribution="gaussian",
                     n.trees=100000,interaction.depth=i, cv.folds=5)
  store[i,2] <- gbm.perf(boost.carseats)
  store[i,3] <- min(boost.carseats$cv.error)
}
store #Lowest cv is depth=1, n.trees=31607, and cv.error=1.471904

boost.carseats=gbm(Sales~., data=Carseats[train,], distribution="gaussian",
                   n.trees=50000,interaction.depth=1, cv.folds=5)
gbm.perf(boost.carseats)
min(boost.carseats$cv.error)
#Even with a new random seed, the n.trees is similar and the cv.error is low. 

boost.carseats=gbm(Sales~., data=Carseats[train,], distribution="gaussian",
                   n.trees=31607,interaction.depth=1, cv.folds=5)
yhat = predict(boost.carseats, n.trees=31607, newdata=Carseats[-train,])
mean((yhat-carseats.test)^2)
#MSE 1.269095

#Part e
library(randomForest)
set.seed(1)
error <- data.frame(mtry=1:11, test.error=rep(NA,11))
for (i in 1:11){
  random.carseats=randomForest(Sales~., data=Carseats, subset=train, mtry=i, importance=TRUE)
  yhat = predict(random.carseats, newdata=Carseats[-train,])
  error[i,2] <- mean((yhat-carseats.test)^2)
}
#Lowest MSE is when mtry = 6

random.carseats=randomForest(Sales~., data=Carseats, subset=train, mtry=6, importance=TRUE)
importance(random.carseats)
varImpPlot(random.carseats)

#Part f
install.packages("earth")

library(earth)
mars.carseats <- earth(Sales~., data=Carseats, subset=train)
yhat = predict(mars.carseats, newdata=Carseats[-train,])
mean((yhat-carseats.test)^2)
#MSE 1.003613

#Problem 3
library(MASS)
head(Pima.tr)
head(Pima.te)

#CART with bagging
library(randomForest)
set.seed(1)
bag.pima=randomForest(type~., data=Pima.tr, mtry=7, importance=TRUE)
yhat = predict(bag.pima, newdata=Pima.te)

test <- table(yhat, Pima.te$type)
(43+39)/332
#Misclassification 0.246988

#Random Forest

error.pima = data.frame(mtry=1:7, test.error=rep(NA, 7))
for (i in 1:7){
  set.seed(1)
  bag.pima=randomForest(type~., data=Pima.tr, mtry=i, importance=TRUE)
  yhat = predict(bag.pima, newdata=Pima.te)
  tmp <- table(yhat, Pima.te$type)
  error.pima[i,2] <- (tmp[2]+tmp[3])/sum(tmp)
}
#Lowest is mtry=2 with Misclassification of 0.2319277

#CART with Boosting
Pima.train <- Pima.tr
Pima.test <- Pima.te
Pima.train$response <- ifelse(Pima.train$type=="Yes",1,0)
Pima.test$response <- ifelse(Pima.test$type=="Yes",1,0)
Pima.train <- Pima.train[,-8]
Pima.test <- Pima.test[,-8]

set.seed(1)
boost.pima = gbm(response~., data=Pima.train, distribution="bernoulli", n.trees=30000, 
                 interaction.depth=1, cv.folds=5)
gbm.perf(boost.pima)
yhat = predict(boost.pima, n.trees=5314,newdata=Pima.test, type="response")
yhat = ifelse(yhat>=.5, 1, 0)
t <- table(yhat, Pima.test$response)
misclassification <- (t[2]+t[3])/sum(t)

store <- data.frame(depth=seq(1:10),n.trees=rep(NA,10), cv.error=rep(NA,10))
for (i in 1:10){
  set.seed(1)
  boost.pima = gbm(response~., data=Pima.train, distribution="bernoulli", n.trees=25000, 
                   interaction.depth=i, cv.folds=5)
  n.trees <- gbm.perf(boost.pima)
  store[i,2] <- n.trees
  
  yhat = predict(boost.pima, n.trees=n.trees,newdata=Pima.test, type="response")
  yhat = ifelse(yhat>=.5, 1, 0)
  t <- table(yhat, Pima.test$response)
  misclassification <- (t[2]+t[3])/sum(t)
  store[i,3] <- misclassification
}
store #Lowest cv is depth=1, n.trees=4452, and misclassification=0.2078313

#Misclassification 0.2078313

#MARS Model 

library(earth)

mars.pima <- earth(response~., data=Pima.train, glm=list(family=binomial))
yhat = predict(mars.pima, newdata=Pima.test, type="response")
yhat = ifelse(yhat>=.5, 1, 0)
t <- table(yhat, Pima.test$response)
misclassification <- (t[2]+t[3])/sum(t)
misclassification
#MSE 0.253012







