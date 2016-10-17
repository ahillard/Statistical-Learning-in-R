#########################################
#Problem 2
library(MASS)
data(Boston)
attach(Boston)

#Part a
model_p2a <- glm(nox~poly(dis, 3), data=Boston)
summary(model_p2a)
coef(summary(model_p2a))

dislims = range(dis)
dis_grid = seq(from=dislims[1], to=dislims[2], by=.1)
preds = predict(model_p2a, newdata=list(dis=dis_grid), se=T)
se.bands = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)

par(mfrow=c(1,1))
plot(dis, nox, xlim=dislims, cex=.5, col="darkgrey")
title("Degree-3 Polynomial to Predict Nox", outer=T)
lines(dis_grid, preds$fit, lwd=2, col="blue")
matlines(dis_grid, se.bands, lwd=1, col="blue", lty=3)

#Part b

poly_RSS <- data.frame(degree=1:10, RSS=rep(NA,10))
poly_preds <- list()
for (i in 1:10) {
        output <- lm(nox~poly(dis, i), data=Boston)
        poly_RSS[i,2] <- sum(output$residuals^2)
        poly_preds[[i]] = predict(output, newdata=list(dis=dis_grid), se=T)$fit
}

poly_RSS

par(mfrow=c(1,1))
plot(dis, nox, xlim=dislims, cex=.5, col="darkgrey")
title("Polynomials of Degree 1 to 10 to Predict Nox", outer=T)
lines(dis_grid, poly_preds[[1]], lwd=2)
lines(dis_grid, poly_preds[[2]], lwd=2, col="blue")
lines(dis_grid, poly_preds[[3]], lwd=2, col="red")
lines(dis_grid, poly_preds[[4]], lwd=2, col="yellow")
lines(dis_grid, poly_preds[[5]], lwd=2, col="green")
lines(dis_grid, poly_preds[[6]], lwd=2, col="purple")
lines(dis_grid, poly_preds[[7]], lwd=2, col="pink")
lines(dis_grid, poly_preds[[8]], lwd=2, col="brown")
lines(dis_grid, poly_preds[[9]], lwd=2, col="orange")
lines(dis_grid, poly_preds[[10]], lwd=2, col="cyan")
legend("topright", legend=c("1", "2", "3","4","5","6","7","8","9","10"),
       col=c("black","blue","red","yellow","green","purple","pink","brown",
             "orange","cyan"), lty=1, lwd=2, cex=.5)

#Part c

library(boot)
poly_CV <- data.frame(degree=1:10, CV=rep(NA,10))
for (i in 1:10) {
        set.seed(1)
        output <- glm(nox~poly(dis, i), data=Boston)
        poly_CV[i,2] <- cv.glm(Boston, output)$delta[1]
}
poly_CV

#Part d
library(splines)

bs_df <- data.frame(df=3:6, RSS=rep(NA, 4), CV=rep(NA,4))
for (i in 3:6){
        output <- glm(nox~bs(dis, df=i), data=Boston)
        bs_df[i-2,2] <- sum(output$residuals^2)
        set.seed(1)
        bs_df[i-2,3] <- cv.glm(Boston, output)$delta[1]
}

output <- lm(nox~bs(dis, df=5), data=Boston)
preds <- predict(output, newdata=list(dis=dis_grid), se=T)
par(mfrow=c(1,1))
plot(dis, nox, xlim=dislims, cex=.5, col="darkgrey")
title("Regression Spline with 5 DF", outer=T)
lines(dis_grid, preds$fit, lwd=2)

#Part e

ns_df <- data.frame(df=3:6, RSS=rep(NA, 4), CV=rep(NA,4))
for (i in 3:6){
        output <- glm(nox~ns(dis, df=i), data=Boston)
        ns_df[i-2,2] <- sum(output$residuals^2)
        set.seed(1)
        ns_df[i-2,3] <- cv.glm(Boston, output)$delta[1]
}

output <- lm(nox~ns(dis, df=5), data=Boston)
preds <- predict(output, newdata=list(dis=dis_grid), se=T)
par(mfrow=c(1,1))
plot(dis, nox, xlim=dislims, cex=.5, col="darkgrey")
title("Natural Spline with 5 DF", outer=T)
lines(dis_grid, preds$fit, lwd=2)

#Part f

fit=smooth.spline(dis, nox, cv=TRUE)
fit$df

plot(dis, nox, xlim=dislims, cex=.5, col="darkgrey")
title("Smoothing Spline", outer=T)
lines(fit, col="blue", lwd=2)

#Part g

install.packages("bisoreg")
library(bisoreg)

set.seed(1)
fit <- loess.wrapper(dis, nox, span.vals = seq(0.1, 1, by=0.05), folds=10)
names(fit)
s <- fit$s
fit=loess(nox~dis, span=s, data=Boston)
preds <- predict(fit, data.frame(dis=dis_grid))

plot(dis, nox, xlim=dislims, cex=.5, col="darkgrey")
title("Loess Fit", outer=T)
lines(dis_grid, preds, col="blue", lwd=2) 

detach(Boston)

###############################################
#Problem 3

setwd("/Users/ahillard/Documents/2016 Fall Semester/Data 3")
pages <- read.table("pages.dat")
names(pages) <- c("time","pages","count_liners","inches_liners","lines_display")
attach(pages)

#Model Response: pages
#Model Predictors: count_liners, inches_liners, lines_display

#install.packages("mgcv") Thought this was needed for CVgam
library(gam)
library(gamclass)
library(mgcv)

pairs(pages)
set.seed(100)
names(pages)
#CVgam sets seed everytime 

#Do gam, then gamclass, then mgcv. I think all you need is gam class
CVgam(pages~count_liners+inches_liners+lines_display, data=pages, nfold=5) #CV=60.8352
CVgam(pages~inches_liners+lines_display, data=pages, nfold=5) #CV=43.4543
CVgam(pages~count_liners+inches_liners, data=pages, nfold=5) #CV=265.4901
CVgam(pages~count_liners+lines_display, data=pages, nfold=5) #CV=107.1172

#Do gamclass and make sure you specify the k value 
CVgam(pages~s(inches_liners, k=4)+lines_display, data=pages, nfold=5) #CV=40.1059
CVgam(pages~inches_liners+s(lines_display, k=3), data=pages, nfold=5) #CV=46.4255
CVgam(pages~s(inches_liners, k=3)+s(lines_display, k=3), data=pages, nfold=5) #CV=46.4255

#Do gam, then gamclass, then mgcv 
CVgam(pages~ns(inches_liners,1)+lines_display, data=pages, nfold=5) #CV=43.4543. Same as regression
CVgam(pages~inches_liners+ns(lines_display,2), data=pages, nfold=5) #CV=41.2489

#Best model appears to be regular linear regression model
library(gam)
#Need gam for lo function

CVgam(pages~count_liners, data=pages, nfold=5) #CV=150.3159
CVgam(pages~ns(count_liners, 1), data=pages, nfold=5) #CV=150.3159
CVgam(pages~s(count_liners, k=10), data=pages, nfold=5) #CV=146.0387
CVgam(pages~lo(count_liners, span=.5), data=pages, nfold=5) #CV=150.3159. Weird, span isn't effecting it. 

CVgam(pages~inches_liners, data=pages, nfold=5) #CV=143.9255
CVgam(pages~ns(inches_liners, 3), data=pages, nfold=5) #CV=75.1079
CVgam(pages~s(inches_liners, k=3), data=pages, nfold=5) #CV=79.4221
CVgam(pages~lo(inches_liners, span=.5), data=pages, nfold=5) #CV=143.9255. Span isn't making a difference. 

CVgam(pages~lines_display, data=pages, nfold=5) #CV=128.6296
CVgam(pages~ns(lines_display, 4), data=pages, nfold=5) #CV=49.1276
CVgam(pages~s(lines_display, k=5), data=pages, nfold=5) #CV=36.1977
CVgam(pages~lo(lines_display, span=.5), data=pages, nfold=5) #CV=128.6296. Span isn't working. 

#Plot GAM
library(gam)
pairs(pages)
gam_mod1 = gam(pages~s(inches_liners, k=4)+lines_display, data=pages)
plot.gam(gam_mod1, se=TRUE, col="blue") #Already have linear plotted.
summary(gam_mod1)

#######################################################
#Problem 4 

setwd("C:/Users/ajhkt6/Desktop/Data 3")
levee <- read.table("mmr_levee.dat")
range(levee$V1)
pairs(levee)

install.packages("glmnet")
library(glmnet)
x = model.matrix(V1~., levee)[,-1]
y = levee$V1
lasso_log <- glmnet(x, y, alpha=1, family='binomial')
set.seed(1)
cv.out = cv.glmnet(x,y,alpha=1)
bestlam = cv.out$lambda.min
predict(lasso_log, s=bestlam, type="coefficients") #V4, V6, V7, V13
#only V7 and V13 are continuous 
#V4 and V6 are categorical 

#install.packages("gam")
#install.packages("mgcv")
library(gam)
library(mgcv)

#Let's do cross validation on all the models. V4, V6, V7, V13
#Only V7 and V13 are continuous 

model <- gam(V1~V4+V6+V7+V13, data=levee, family=binomial)
names(model)
model$gcv.ubre
#Need GAM and mgcv for UBRE
#Use UBRE score and write for loop for all ns, s, and lo for each predictor

V7_df <- data.frame(df=1:10, ns=rep(NA,10), s=rep(NA,10), lo=rep(NA,10))
for (i in 1:10){
        model = gam(V1~V4+V6+ns(V7,i)+V13, data=levee, family=binomial)
        V7_df[i,2] <- model$aic
        model = gam(V1~V4+V6+s(V7,i)+V13, data=levee, family=binomial)
        V7_df[i,3] <- model$aic
        model = gam(V1~V4+V6+lo(V7,span=i/10)+V13, data=levee, family=binomial)
        V7_df[i,4] <- model$aic
}



CVgam(V1~V4+V6+V7+V13, data=levee, nfold=10) #0.2223
CVgam(V1~V4+V6+ns(V7,1)+V13, data=levee, nfold=10) #0.2223
CVgam(V1~V4+V6+s(V7,k=3)+V13, data=levee, nfold=10) #0.2223
CVgam(V1~V4+V6+lo(V7,span=.1)+V13, data=levee, nfold=10) #0.2223

CVgam(V1~V4+V6+V7+V13, data=levee, nfold=10) #0.2223
CVgam(V1~V4+V6+V7+ns(V13,3), data=levee, nfold=10) #0.1995. 
CVgam(V1~V4+V6+ns(V13,3), data=levee, nfold=10) #0.1980 . This is the lowest. 
CVgam(V1~V4+V6+V7+s(V13,k=3), data=levee, nfold=10) #0.2223

gam_mod <- gam(V1~V4+V7+ns(V13,3), data=levee)
plot.gam(gam_mod) #Why isn't this working? 

####



mod1 <- gam(V1~V4+V6+V7+V13, data=levee)

mod2 <- gam(V1~V4+V6+ns(V7,2)+V13, data=levee)
mod2a <- gam(V1~V4+V6+ns(V7,3)+V13, data=levee)
mod2b <- gam(V1~V4+V6+ns(V7,4)+V13, data=levee)
mod2c <- gam(V1~V4+V6+ns(V7,5)+V13, data=levee)
mod2d <- gam(V1~V4+V6+ns(V7,6)+V13, data=levee)
anova(mod1, mod2, test="F")
anova(mod1, mod2a, test="F")
anova(mod1, mod2b, test="F") #Lowest P-Value
anova(mod1, mod2c, test="F")
anova(mod1, mod2d, test="F")

mod3 <- gam(V1~V4+V6+s(V7, 1)+V13, data=levee)
mod3a <- gam(V1~V4+V6+s(V7, 2)+V13, data=levee)
mod3b <- gam(V1~V4+V6+s(V7, 3)+V13, data=levee)
mod3c <- gam(V1~V4+V6+s(V7, 4)+V13, data=levee)
mod3d <- gam(V1~V4+V6+s(V7, 5)+V13, data=levee)
mod3e <- gam(V1~V4+V6+s(V7, 6)+V13, data=levee)
mod3f <- gam(V1~V4+V6+s(V7, 7)+V13, data=levee)
anova(mod1, mod3a, test="F")
anova(mod1, mod3b, test="F")
anova(mod1, mod3c, test="F") #Lowest p-value 
anova(mod1, mod3d, test="F")
anova(mod1, mod3e, test="F")

mod4 <- gam(V1~V4+V6+V7+V13, data=levee)
mod4a <- gam(V1~V4+V6+lo(V7, span=.3)+V13, data=levee)
mod4b <- gam(V1~V4+V6+lo(V7, span=.4)+V13, data=levee)
mod4c <- gam(V1~V4+V6+lo(V7, span=.5)+V13, data=levee)
mod4d <- gam(V1~V4+V6+lo(V7, span=.6)+V13, data=levee)
mod4e <- gam(V1~V4+V6+lo(V7, span=.7)+V13, data=levee)
mod4f <- gam(V1~V4+V6+lo(V7, span=.8)+V13, data=levee)
mod4g <- gam(V1~V4+V6+lo(V7, span=.9)+V13, data=levee)
summary(mod4a)
summary(mod4b)
summary(mod4c)
summary(mod4d)
summary(mod4e)
summary(mod4f)
summary(mod4g)
anova(mod1, mod4a, test="F")
anova(mod1, mod4b, test="F")
anova(mod1, mod4c, test="F")
anova(mod1, mod4d, test="F")
anova(mod1, mod4e, test="F")
anova(mod1, mod4f, test="F") 
anova(mod1, mod4g, test="F") #Lowest p-value 

library(gam)
library(mgcv)
library(gamclass)

summary(mod2b) #AIC=89.753
summary(mod3c) #AIC=93.215
summary(mod4g) #AIC=94.2109

#mod2b, mod3c, mod4f
CVgam(V1~V4+V6+ns(V7,4)+V13, data=levee, nfold=5) #0.2264 CVGAM
CVgam(V1~V4+V6+s(V7,4)+V13, data=levee, nfold=5) #?
CVgam(V1~V4+V6+lo(V7, span=.8)+V13, data=levee, nfold=5) #0.2283 CVGAM

library(gam)
mod1 <- gam(V1~V4+V6+ns(V7,4)+V13, data=levee)
mod2 <- gam(V1~V4+V6+ns(V7,4)+ns(V13,2), data=levee)
mod2a <- gam(V1~V4+V6+ns(V7,4)+ns(V13,3), data=levee)
mod2b <- gam(V1~V4+V6+ns(V7,4)+ns(V13,4), data=levee)
mod2c <- gam(V1~V4+V6+ns(V7,4)+ns(V13,5), data=levee)
mod2d <- gam(V1~V4+V6+ns(V7,4)+ns(V13,6), data=levee)
anova(mod1, mod2, test="F")
anova(mod1, mod2a, test="F") #Lowest P-Value
anova(mod1, mod2b, test="F") 
anova(mod1, mod2c, test="F")
anova(mod1, mod2d, test="F")

mod3 <- gam(V1~V4+V6+ns(V7,4)+s(V13,1), data=levee)
mod3a <- gam(V1~V4+V6+ns(V7,4)+s(V13,2), data=levee)
mod3b <- gam(V1~V4+V6+ns(V7,4)+s(V13,3), data=levee)
mod3c <- gam(V1~V4+V6+ns(V7,4)+s(V13,4), data=levee)
mod3d <- gam(V1~V4+V6+ns(V7,4)+s(V13,4), data=levee)
mod3e <- gam(V1~V4+V6+ns(V7,4)+s(V13,4), data=levee)
mod3f <- gam(V1~V4+V6+ns(V7,4)+s(V13,4), data=levee)
anova(mod1, mod3, test="F") #Lowest p-value 
anova(mod1, mod3a, test="F")
anova(mod1, mod3b, test="F") 
anova(mod1, mod3c, test="F")
anova(mod1, mod3d, test="F")
anova(mod1, mod3e, test="F")
anova(mod1, mod3f, test="F")

mod4a <- gam(V1~V4+V6+ns(V7,4)+lo(V13, span=.2), data=levee)
mod4b <- gam(V1~V4+V6+ns(V7,4)+lo(V13, span=.3), data=levee)
mod4c <- gam(V1~V4+V6+ns(V7,4)+lo(V13, span=.4), data=levee)
mod4d <- gam(V1~V4+V6+ns(V7,4)+lo(V13, span=.5), data=levee)
mod4e <- gam(V1~V4+V6+ns(V7,4)+lo(V13, span=.6), data=levee)
mod4f <- gam(V1~V4+V6+ns(V7,4)+lo(V13, span=.7), data=levee)
mod4g <- gam(V1~V4+V6+ns(V7,4)+lo(V13, span=.8), data=levee)
mod4h <- gam(V1~V4+V6+ns(V7,4)+lo(V13, span=.9), data=levee)
anova(mod1, mod4a)
anova(mod1, mod4b)
anova(mod1, mod4c)
anova(mod1, mod4d)
anova(mod1, mod4e)
anova(mod1, mod4f)
anova(mod1, mod4g) #Lowest p-value 
anova(mod1, mod4h)

#mod2a, mod3, mod4g
summary()
