#Problem 9 

set.seed(1) 
train <- sample(1:nrow(College), nrow(College)/2) 
test <- (-train)

library(boot)
lr_model <- glm(Apps~., data=College, subset=train)
summary(lr_model)
mean(( predict(lr_model, College[test,])-College[test,]$Apps )^2)
mean((predict(lr_model, College[train,]) - College[train,]$Apps)^2)
#Test Error 1108531

install.packages("glmnet")
library(glmnet)

#ridge regression with glmnet

x <- model.matrix(Apps~., College)[,-1]
y <- College$Apps

ridge_mod = glmnet(x[train,], y[train], alpha =0)
set.seed(1)
cv.out = cv.glmnet(x[train ,],y[train],alpha =0)
bestlam = cv.out$lambda.min

ridge_pred = predict(ridge_mod, s=bestlam, newx=x[test,])
mean( (ridge_pred - y[test])^2 ) 
#Test Error 1037616
#best lambda is 450.7435

#Lasso Model 
lasso_mod = glmnet(x[train,], y[train], alpha=1)
#lasso_mod = glmnet(x[train,], y[train], alpha=1, lambda=bestlam)
plot(lasso_mod)
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
lasso_pred = predict(lasso_mod, s=bestlam, newx=x[test,])
lasso_mod1 = glmnet(x[train,], y[train], alpha=1, lambda=bestlam)
mean((lasso_pred-y[test])^2) 
#Test Error 1030941
#Lambda was 24.62086

#PCR Model, chose M with cross validation
set.seed(1)
pcr_model = pcr(Apps~., data=College, subset=train, scale=TRUE, validation="CV")
validationplot(pcr_model, val.type="MSEP")
summary(pcr_model) #16 Comps the model
pcr_pred = predict(pcr_model ,x[test ,], ncomp =16)
mean((pcr_pred -y[test])^2) #Test Error 1166897

#PLS Model, chose M with cross validation

install.packages("pls")
library(pls)
set.seed(1)
pls_model = plsr(Apps~., data=College, subset=train, scale=TRUE, validation="CV")
validationplot(pls_model, val.type="MSEP") #10 comps is lowest and stabilizes after. 
summary(pls_model)
pls_pred = predict(pls_model ,x[test ,], ncomp=10)
mean((pls_pred-y[test])^2) #Test Error 1134531

head(College)
mean(College$Apps)
range(College$Apps)
sqrt(1166897)
sqrt(1030941)

###########################################################################
#Problem 2

library(MASS)
head(Boston)

set.seed(1)
train = sample(c(TRUE ,FALSE), nrow(Boston),rep=TRUE)
test = (!train)

#Linear Regression Model
head(Boston)
lm_model <- lm(crim~., data=Boston, subset=train)
lm_predict <- predict(lm_model, Boston[test,])
mean((lm_predict-Boston[test,]$crim)^2) #59.36511

#Best Subsets Model

k=10
set.seed(1)
folds = sample(1:k, nrow(Boston), replace=TRUE)
cv.errors = matrix(NA,k,13, dimnames=list(NULL, paste(1:13)))

predict.regsubsets = function(object, newdata, id,...){
        form=as.formula(object$call[[2]])
        mat=model.matrix(form,newdata)
        coefi=coef(object, id=id)
        xvars=names(coefi)
        mat[,xvars]%*%coefi
}

for(j in 1:k){
        best.fit = regsubsets(crim~., data=Boston[folds!=j,], nvmax=13)
        for (i in 1:13){
                pred = predict(best.fit, Boston[folds==j,], id=i)
                cv.errors[j,i] = mean((pred-Boston[folds==j,]$crim)^2)
        }
}

mean.cv.errors = apply(cv.errors, 2, mean)
sd.cv.errors = apply(cv.errors, 2, sd) #Can't use standard deviation rule here. 
which.min(mean.cv.errors) #12 Variables is best method

train.best.fit = regsubsets(crim~., data=Boston[train,], nvmax=13)
coef(train.best.fit, 12) #12 Variables is best method
test.pred = predict(train.best.fit, Boston[test,], id=12)
mean((test.pred - Boston[test,]$crim)^2) #59.3044 Test MSE

#Ridge Regression

x= model.matrix(crim~., Boston)[,-1]
y= Boston$crim

ridge.mod = glmnet(x[train,], y[train],alpha=0)
set.seed(1)
cv.out=cv.glmnet(x[train,], y[train], alpha=0)
bestlam = cv.out$lambda.min
bestlam
ridge.pred = predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred - y[test])^2) #Test MSE is 58.26644

#Lasso Model

lasso.mod = glmnet(x[train,], y[train], alpha=1)
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha=1)
bestlam = cv.out$lambda.min
bestlam
lasso.pred = predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred - y[test])^2) #Test MSE is 56.26661
lasso.mod1 = glmnet(x[train,], y[train], alpha=1, lambda=bestlam)
coefficients(lasso.mod1)

#PCR Model

set.seed(1)
pcr.fit = pcr(crim~., data=Boston, subset=train, scale=TRUE, validation="CV")
validationplot(pcr.fit)
summary(pcr.fit) #13 Comps has lowest test MSE
pcr.pred=predict(pcr.fit, x[test,], ncomp=13)
mean((pcr.pred - y[test])^2) #59.36511

pcr.pred=predict(pcr.fit, x[test,], ncomp=8)
mean((pcr.pred - y[test])^2) #56.59548

pcr.pred=predict(pcr.fit, x[test,], ncomp=9)
mean((pcr.pred - y[test])^2) #56.2251

pcr.pred=predict(pcr.fit, x[test,], ncomp=10)
mean((pcr.pred - y[test])^2) #56.50196

pcr.pred=predict(pcr.fit, x[test,], ncomp=11)
mean((pcr.pred - y[test])^2) #60.77806

#PLS Model

set.seed(1)
pls.fit = plsr(crim~., data=Boston, subset=train, scale=TRUE, validation="CV")
summary(pls.fit) #9 Comps has lowest 
validationplot(pls.fit)
pls.pred=predict(pls.fit, x[test,], ncomp=9)
mean((pls.pred - y[test])^2) #59.28428

pls.pred=predict(pls.fit, x[test,], ncomp=4)
mean((pls.pred - y[test])^2) #59.24453

pls.pred=predict(pls.fit, x[test,], ncomp=5)
mean((pls.pred - y[test])^2) #59.20932

pls.pred=predict(pls.fit, x[test,], ncomp=11)
mean((pls.pred - y[test])^2) #59.20932

##################################################################################
#Problem 3

setwd("/Users/ahillard/Documents/2016 Fall Semester/Data 3")
p3 = read.table("student-mat.csv", sep=";", header=TRUE)
head(p3)

#Ridge Regression

set.seed(1)
train = sample(c(TRUE ,FALSE), nrow(p3),rep=TRUE)
test = (!train)

x <- model.matrix(G3~., data=p3)[,-c(1,41:42)]
y <- p3$G3       

set.seed(1)
ridge.mod = glmnet(x[train,], y[train], alpha=0)
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha=0, nfolds=5)
bestlam=cv.out$lambda.min
bestlam
ridge.pred = predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred-y[test])^2) #18.35763 Test MSE

#Lasso Regression

set.seed(1)
lasso.mod = glmnet(x[train,], y[train], alpha=1)
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha=1, nfolds=5)
bestlam=cv.out$lambda.min
bestlam
lasso.pred = predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred-y[test])^2) #18.51826 Test MSE

lasso.mod1 = glmnet(x[train,], y[train], alpha=1, lambda=bestlam)
coefficients(lasso.mod1)

#PC Regression

p3a <- as.data.frame(cbind(y,x))
names(p3a)[1] <- "G3"
set.seed(1)
pcr.fit = pcr(G3~., data=p3a, scale=TRUE, validation="CV", segments=5)
validationplot(pcr.fit, val.type="MSEP") #25 comps, 4.335
summary(pcr.fit)
4.335^2 #18.7922 CV Test MSE
pcr.pred=predict(pcr.fit, x[test,], ncomp=25)
mean((pcr.pred - y[test])^2) #16.60168 Test MSE

#PLS Regression

p3a <- as.data.frame(cbind(y,x))
names(p3a)[1] <- "G3"
set.seed(1)
pls.fit = plsr(G3~., data=p3a, scale=TRUE, validation="CV", segments=5)
validationplot(pls.fit, val.type="MSEP") #1 Comp, 4.333
summary(pls.fit)
4.333^2 #18.77489 CV Test MSE
pcr.pred=predict(pcr.fit, x[test,], ncomp=1)
mean((pcr.pred - y[test])^2) #18.78685 Test MSE


######Part b Problem 3#########

#Ridge Regression

set.seed(1)
train = sample(c(TRUE ,FALSE), nrow(p3),rep=TRUE)
test = (!train)

x <- model.matrix(G3~., data=p3)[,-1]
y <- p3$G3       

#Ridge Regression
ridge.mod = glmnet(x[train,], y[train], alpha=0)
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha=0, nfolds=5)
bestlam=cv.out$lambda.min
bestlam
ridge.pred = predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred-y[test])^2) #4.392806 Test MSE

#Lasso Regression

set.seed(1)
lasso.mod = glmnet(x[train,], y[train], alpha=1)
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha=1, nfolds=5)
bestlam=cv.out$lambda.min
bestlam
lasso.pred = predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred-y[test])^2) #3.466583 Test MSE
lasso.mod1 = glmnet(x[train,], y[train], alpha=1, lambda=bestlam)
coefficients(lasso.mod1)

#PC Regression

p3a <- as.data.frame(cbind(y,x))
names(p3a)[1] <- "G3"
set.seed(1)
pcr.fit = pcr(G3~., data=p3a, scale=TRUE, validation="CV", segments=5)
validationplot(pcr.fit, val.type="MSEP") #Levels out at 39 comps, 2.061
summary(pcr.fit)
2.077^2 #4.313929 CV Test MSE
pcr.pred=predict(pcr.fit, x[test,], ncomp=39)
mean((pcr.pred - y[test])^2) #3.265469 Test MSE

#PLS Regression

p3a <- as.data.frame(cbind(y,x))
names(p3a)[1] <- "G3"
set.seed(1)
pls.fit = plsr(G3~., data=p3a, scale=TRUE, validation="CV", segments=5)
validationplot(pls.fit, val.type="MSEP") #5 Comp levels out 2.0...
summary(pls.fit)
2.090^2 #4.3681 Test MSE
pls.pred=predict(pls.fit, x[test,], ncomp=5)
mean((pls.pred - y[test])^2)  #Test MSE

#Should use standard deviation rule?
