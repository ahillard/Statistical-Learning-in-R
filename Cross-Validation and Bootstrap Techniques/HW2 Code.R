#5.8

set.seed(1)
y=rnorm(100)
x=rnorm(100)
y=x-2*x^2+rnorm(100)

plot(x, y, main="Scatterplot of X and Y", xlab="X", ylab="Y")

library("boot")

p1 <- data.frame(x,y)
x <- vector()

for (i in 1:4){
        set.seed(1000)
        model = glm(y~poly(x,i), data=p1)
        LOOCV = cv.glm(p1, model)$delta[1]
        x <- append(x, LOOCV)  
}

LOOCV1 <- data.frame(1:4, x)
names(LOOCV1) <- c("Power of Model", "LOOCV")

summary(glm(y~x, data=p1))
summary(glm(y~poly(x,2), data=p1))
summary(glm(y~poly(x,3), data=p1))
summary(glm(y~poly(x,4), data=p1))

######

set.seed(2000)
y=rnorm(100)
x=rnorm(100)
y=x-2*x^2+rnorm(100)

plot(x, y, main="Scatterplot of X and Y", xlab="X", ylab="Y")

p1 <- data.frame(x,y)
x <- vector()

for (i in 1:4){
        model = glm(y~poly(x,i), data=p1)
        LOOCV = cv.glm(p1, model)$delta[1]
        x <- append(x, LOOCV)  
}

LOOCV2 <- data.frame(1:4, x)
names(LOOCV2) <- c("Power of Model", "LOOCV")

#5.9

library("MASS")
head(Boston)

attach(Boston)
mean(medv)
sd(medv)/sqrt(length(medv))

library("boot")
?boot
mean.fn <- function(data, index){
        return(mean(data[index]))
}

boot.se <- boot(medv, mean.fn, R=100000)
#d
t.test(medv)
boot.se
22.53281 + c(-2,2)*0.4086137

median(medv)

median.fn <- function(data, index){
        return(median(data[index])) 
}

boot.med <- boot(medv, median.fn, R=100000)

quantile(medv, .10)

tenth.fn <- function(data, index){
        return(quantile(data[index],.10))
}

boot.tenth <- boot(medv, tenth.fn, R=100000)

#Problem 3

getwd()
setwd("/Users/ahillard/Documents/2016 Fall Semester/Data 3")
pages <- read.table("pages.dat")
names(pages) <- c("time","pages","count_liners","inches_liners","lines_display")

#Create Storage Vector

x = vector()

#One Variable

m1a = glm(pages~count_liners, data=pages)
x = append(x,cv.glm(pages, m1a, K=5)$delta[1])

m1b = glm(pages~inches_liners, data=pages)
x = append(x,cv.glm(pages, m1b, K=5)$delta[1])

m1c = glm(pages~lines_display, data=pages)
x = append(x,cv.glm(pages, m1c, K=5)$delta[1])

#Two Variable

m2a = glm(pages~count_liners+inches_liners, data=pages)
x = append(x,cv.glm(pages, m2a, K=5)$delta[1])

m2b = glm(pages~count_liners+lines_display, data=pages)
x = append(x,cv.glm(pages, m2b, K=5)$delta[1])

set.seed(1)
m2c = glm(pages~inches_liners+lines_display, data=pages)
x = append(x,cv.glm(pages, m2c, K=5)$delta[1])

#Three Variable

m3 = glm(pages~count_liners+inches_liners+lines_display, data=pages)
x = append(x,cv.glm(pages, m3, K=5)$delta[1])

#Create Dataframe

y <- c("count_liners","inches_liners","lines_display","count_liners, inches_liners","count_liners,lines_display",
       "inches_liners,lines_display","count_liners,inches_liners,lines_display")

p3 <- data.frame(y,x)
names(p3) <- c("Predictors","CV Values")
p3

#Problem 4

p4 <- read.table("des_site1and2sp_2.dat")
names(p4) <- c("des_glut","des_nud","UTM_north","UTM_east",
               "southwestness","elevation","slope","geology",
               "LTA","ELT","site","plot_number")

p4 <- p4[,c(1,5:10)]
names(p4) <- c("y","sw","el","sl","geo","LTA","ELT")

#One Variable

x1 <- vector()

cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

m1a = glm(y~sw, data=p4, family=binomial)
x1 = append(x1,cv.glm(p4, m1a, cost, K=10)$delta[1])

m1b = glm(y~el, data=p4, family=binomial)
x1 = append(x1,cv.glm(p4, m1b, cost, K=10)$delta[1])

m1c = glm(y~sl, data=p4, family=binomial)
x1 = append(x1,cv.glm(p4, m1c, cost, K=10)$delta[1])

m1d = glm(y~geo, data=p4, family=binomial)
x1 = append(x1,cv.glm(p4, m1d, cost, K=10)$delta[1])

m1e = glm(y~LTA, data=p4, family=binomial)
x1 = append(x1,cv.glm(p4, m1e, cost, K=10)$delta[1])

m1f = glm(y~ELT, data=p4, family=binomial)
x1 = append(x1,cv.glm(p4, m1f, cost, K=10)$delta[1])

#Two Variables 

x2 <- vector()

cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

m1a = glm(y~sw+el, data=p4, family=binomial)
x2 = append(x2,cv.glm(p4, m1a, cost, K=10)$delta[1])

m1b = glm(y~sw+sl, data=p4, family=binomial)
x2 = append(x2,cv.glm(p4, m1b, cost, K=10)$delta[1])

m1c = glm(y~sw+geo, data=p4, family=binomial)
x2 = append(x2,cv.glm(p4, m1c, cost, K=10)$delta[1])

m1d = glm(y~sw+LTA, data=p4, family=binomial)
x2 = append(x2,cv.glm(p4, m1d, cost, K=10)$delta[1])

m1e = glm(y~sw+ELT, data=p4, family=binomial)
x2 = append(x2,cv.glm(p4, m1e, cost, K=10)$delta[1])

m1f = glm(y~el+sl, data=p4, family=binomial)
x2 = append(x2,cv.glm(p4, m1f, cost, K=10)$delta[1])

m1g = glm(y~el+geo, data=p4, family=binomial)
x2 = append(x2,cv.glm(p4, m1g, cost, K=10)$delta[1])

m1h = glm(y~el+LTA, data=p4, family=binomial)
x2 = append(x2,cv.glm(p4, m1h, cost, K=10)$delta[1])

m1i = glm(y~el+ELT, data=p4, family=binomial)
x2 = append(x2,cv.glm(p4, m1i, cost, K=10)$delta[1])

m1j = glm(y~sl+geo, data=p4, family=binomial)
x2 = append(x2,cv.glm(p4, m1j, cost, K=10)$delta[1])

m1k = glm(y~sl+LTA, data=p4, family=binomial)
x2 = append(x2,cv.glm(p4, m1k, cost, K=10)$delta[1])

m1l = glm(y~sl+ELT, data=p4, family=binomial)
x2 = append(x2,cv.glm(p4, m1l, cost, K=10)$delta[1])

m1m = glm(y~geo+LTA, data=p4, family=binomial)
x2 = append(x2,cv.glm(p4, m1m, cost, K=10)$delta[1])

m1n = glm(y~geo+ELT, data=p4, family=binomial)
x2 = append(x2,cv.glm(p4, m1n, cost, K=10)$delta[1])

m1o = glm(y~LTA+ELT, data=p4, family=binomial)
x2 = append(x2,cv.glm(p4, m1o, cost, K=10)$delta[1])

y <- c("sw,el","sw,sl","sw,geo","sw,LTA","sw,ELT","el,sl","el,geo","el,LTA",
       "el,ETA","sl,geo","sl,LTA","sl,ELT","geo,LTA","geo,ELT","LTA,ELT")

p4_2var <- data.frame(y,x2)
names(p4_2var) <- c("Model", "CV Value")

#Three Variables
combn(6,3)

x3 <- vector()

cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

m1a = glm(y~sw+el+sl, data=p4, family=binomial)
x3 = append(x3,cv.glm(p4, m1a, cost, K=10)$delta[1])

m1b = glm(y~sw+el+geo, data=p4, family=binomial)
x3 = append(x3,cv.glm(p4, m1b, cost, K=10)$delta[1])

m1c = glm(y~sw+el+LTA, data=p4, family=binomial)
x3 = append(x3,cv.glm(p4, m1c, cost, K=10)$delta[1])

m1d = glm(y~sw+el+ELT, data=p4, family=binomial)
x3 = append(x3,cv.glm(p4, m1d, cost, K=10)$delta[1])

m1e = glm(y~sw+sl+geo, data=p4, family=binomial)
x3 = append(x3,cv.glm(p4, m1e, cost, K=10)$delta[1])

m1f = glm(y~sw+sl+LTA, data=p4, family=binomial)
x3 = append(x3,cv.glm(p4, m1f, cost, K=10)$delta[1])

m1g = glm(y~sw+sl+ELT, data=p4, family=binomial)
x3 = append(x3,cv.glm(p4, m1g, cost, K=10)$delta[1])

m1h = glm(y~sw+geo+LTA, data=p4, family=binomial)
x3 = append(x3,cv.glm(p4, m1h, cost, K=10)$delta[1])

m1i = glm(y~sw+geo+ELT, data=p4, family=binomial)
x3 = append(x3,cv.glm(p4, m1i, cost, K=10)$delta[1])

m1j = glm(y~sl+LTA+ELT, data=p4, family=binomial)
x3 = append(x3,cv.glm(p4, m1j, cost, K=10)$delta[1])

m1k = glm(y~el+sl+geo, data=p4, family=binomial)
x3 = append(x3,cv.glm(p4, m1k, cost, K=10)$delta[1])

m1l = glm(y~el+sl+LTA, data=p4, family=binomial)
x3 = append(x3,cv.glm(p4, m1l, cost, K=10)$delta[1])

m1m = glm(y~el+sl+ELT, data=p4, family=binomial)
x3 = append(x3,cv.glm(p4, m1m, cost, K=10)$delta[1])

m1n = glm(y~el+geo+LTA, data=p4, family=binomial)
x3 = append(x3,cv.glm(p4, m1n, cost, K=10)$delta[1])

m1o = glm(y~el+geo+ELT, data=p4, family=binomial)
x3 = append(x3,cv.glm(p4, m1o, cost, K=10)$delta[1])

m1p = glm(y~el+LTA+ELT, data=p4, family=binomial)
x3 = append(x3,cv.glm(p4, m1p, cost, K=10)$delta[1])

m1q = glm(y~sl+geo+LTA, data=p4, family=binomial)
x3 = append(x3,cv.glm(p4, m1q, cost, K=10)$delta[1])

m1r = glm(y~sl+geo+ELT, data=p4, family=binomial)
x3 = append(x3,cv.glm(p4, m1r, cost, K=10)$delta[1])

m1s = glm(y~sl+LTA+ELT, data=p4, family=binomial)
x3 = append(x3,cv.glm(p4, m1s, cost, K=10)$delta[1])

m1t = glm(y~geo+LTA+ELT, data=p4, family=binomial)
x3 = append(x3,cv.glm(p4, m1t, cost, K=10)$delta[1])

#Four Variables
combn(6,4)

x4 <- vector()

cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

m1a = glm(y~sw+el+sl+geo, data=p4, family=binomial)
x4 = append(x4,cv.glm(p4, m1a, cost, K=10)$delta[1])

m1b = glm(y~sw+el+sl+LTA, data=p4, family=binomial)
x4 = append(x4,cv.glm(p4, m1b, cost, K=10)$delta[1])

m1c = glm(y~sw+el+sl+ELT, data=p4, family=binomial)
x4 = append(x4,cv.glm(p4, m1c, cost, K=10)$delta[1])

m1d = glm(y~sw+el+geo+LTA, data=p4, family=binomial)
x4 = append(x4,cv.glm(p4, m1d, cost, K=10)$delta[1])

m1e = glm(y~sw+el+geo+ELT, data=p4, family=binomial)
x4 = append(x4,cv.glm(p4, m1e, cost, K=10)$delta[1])

m1f = glm(y~sw+el+LTA+ELT, data=p4, family=binomial)
x4 = append(x4,cv.glm(p4, m1f, cost, K=10)$delta[1])

m1g = glm(y~sw+sl+geo+LTA, data=p4, family=binomial)
x4 = append(x4,cv.glm(p4, m1g, cost, K=10)$delta[1])

m1h = glm(y~sw+sl+geo+ELT, data=p4, family=binomial)
x4 = append(x4,cv.glm(p4, m1h, cost, K=10)$delta[1])

m1i = glm(y~sw+sl+LTA+ELT, data=p4, family=binomial)
x4 = append(x4,cv.glm(p4, m1i, cost, K=10)$delta[1])

m1j = glm(y~sw+geo+LTA+ELT, data=p4, family=binomial)
x4 = append(x4,cv.glm(p4, m1j, cost, K=10)$delta[1])

m1k = glm(y~el+sl+geo+LTA, data=p4, family=binomial)
x4 = append(x4,cv.glm(p4, m1k, cost, K=10)$delta[1])

m1l = glm(y~el+sl+geo+ELT, data=p4, family=binomial)
x4 = append(x4,cv.glm(p4, m1l, cost, K=10)$delta[1])

m1m = glm(y~el+sl+LTA+ELT, data=p4, family=binomial)
x4 = append(x4,cv.glm(p4, m1m, cost, K=10)$delta[1])

m1n = glm(y~el+geo+LTA+ELT, data=p4, family=binomial)
x4 = append(x4,cv.glm(p4, m1n, cost, K=10)$delta[1])

m1o = glm(y~sl+geo+LTA+ELT, data=p4, family=binomial)
x4 = append(x4,cv.glm(p4, m1o, cost, K=10)$delta[1])

#Five Variables
combn(6,5)

x5 <- vector()

cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

m1a = glm(y~sw+el+sl+geo+LTA, data=p4, family=binomial)
x5 = append(x5,cv.glm(p4, m1a, cost, K=10)$delta[1])

m1b = glm(y~sw+el+sl+geo+ELT, data=p4, family=binomial)
x5 = append(x5,cv.glm(p4, m1b, cost, K=10)$delta[1])

m1c = glm(y~sw+el+sl+LTA+ELT, data=p4, family=binomial)
x5 = append(x5,cv.glm(p4, m1c, cost, K=10)$delta[1])

m1d = glm(y~sw+el+geo+LTA+ELT, data=p4, family=binomial)
x5 = append(x5,cv.glm(p4, m1d, cost, K=10)$delta[1])

m1e = glm(y~sw+sl+geo+LTA+ELT, data=p4, family=binomial)
x5 = append(x5,cv.glm(p4, m1e, cost, K=10)$delta[1])

m1f = glm(y~el+sl+geo+LTA+ELT, data=p4, family=binomial)
x5 = append(x5,cv.glm(p4, m1f, cost, K=10)$delta[1])

#Six Variables
combn(6,6)

x6 = vector()

m1a = glm(y~sw+el+sl+geo+LTA+ELT, data=p4, family=binomial)
x6 = append(x6,cv.glm(p4, m1a, cost, K=10)$delta[1])

min(x1)
min(x2)
min(x3)
min(x4)
min(x5)
min(x6)
