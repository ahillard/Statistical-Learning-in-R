##Part A

y <- c(0.001, 2.6, 14.6)
x <- c(0.05, 1.1, 2.2)

theta1 <- seq(from = 4.9, to = 5.15, by = .05)
theta2 <- seq(from = 2.3, to = 2.7, by = .1)

SSEQ1 <- function(t1, t2){

gridSearch <- matrix(,3, length(t1)*length(t2))
rownames(gridSearch) <- c("theta1", "theta2", "SSE")

colcount <- 1

for (i in 1: length(t1))
	{
	
	for (j in 1: length(t2))
		{
		
		SSE <- 0
		
		for (k in 1: length(y))
			{

			SSE <- SSE + (y[k]-(t1[i]/t2[j])*x[k]^t2[j])^2

			}
		
		gridSearch[1,colcount] <- t1[i]
		gridSearch[2,colcount] <- t2[j]
		gridSearch[3,colcount] <- SSE 	
		
		colcount <- colcount + 1
		
		}
	}
	
	gridSearch

}
	

mat <- SSEQ1(theta1, theta2)
minimum <- mat[3,]== min(mat[3,])
mat[, minimum]

##Part B

logy <- log(y)
logx <- log(x)

lm(logy ~ logx)
starttheta1 <- exp(0.6956)*2.5369

##Part C

thetaNames <- c("theta1", "theta2")
thetaValues <- c(4.9, 2.4)
yValues <- c(0.001, 2.6, 14.6)
xValues <- c(0.05, 1.1, 2.2)
equation <- expression((theta1/theta2)*x^theta2)


F <- function(thetaNames, thetaValues, xValues, f){	

	Fmatrix <- matrix(nrow = length(xValues), ncol = length(thetaValues))
	
	for (i in 1 : length(xValues)){
		
		for (j in 1 : length(thetaValues)){
			
			df.dtheta <- D(f, thetaNames[j])
			Fmatrix[i,j] <- eval(df.dtheta, list(x=xValues[i], theta1=thetaValues[1], theta2=thetaValues[2]))
  
		}

	}

	Fmatrix

}


f <- function(thetaValues, xValues, f) {
	
	fmatrix <- matrix(nrow = length(xValues), ncol=1)
	
	for (i in 1 : length(xValues)){

	fmatrix[i,1] <- eval(equation, list(x=xValues[i], theta1 = thetaValues[1], theta2=thetaValues[2]))


	}
	
	fmatrix
}

gaussNewton <- function(thetaNames, thetaValues, xValues, equation) {
        Fmatrix <- F(thetaNames, thetaValues, xValues, equation)
        fequation <- f(thetaValues, xValues, equation)
        delta <- FFinverse %*% t(Fmatrix) %*% (t(t(yValues))-fequation)
        newThetaValues <- thetaValues + delta
        newThetaValues 
}


Fmatrix <- F(thetaNames, thetaValues, xValues, equation)
fequation <- f(thetaValues,xValues, equation)

FFinverse <- solve(t(Fmatrix)%*%Fmatrix)

delta <- FFinverse %*% t(Fmatrix) %*% (t(t(yValues))-fequation)

thetaValues1 <- thetaValues + delta
thetaValues1

## Part D

steepestDescent <- function(thetaNames, thetaValues, xValues, equation) {
        
        Fmatrix <- F(thetaNames, thetaValues, xValues, equation)
        fequation <- f(thetaValues, xValues, equation)
        dQ.dtheta <- -2 * t(Fmatrix) %*% (t(t(yValues))-fequation)
        delta <- -1 * diag(nrow(t(Fmatrix))) %*% dQ.dtheta
        newThetaValues <- thetaValues + delta
        newThetaValues
}

## Part E 

newtonRaphson <- function(thetaNames, thetaValues, xValues, equation) {
        
        Fmatrix <- F(thetaNames, thetaValues, xValues, equation)
        
        Hessian <- matrix(nrow=length(xValues),1)
        
        for (i in 1 : length(xValues)){
                
                for (j in 1 : length(thetaValues)){
                        
                        df.dtheta <- D(equation, thetaNames[i])
                        df2.dtheta2 <- D(df.dtheta, thetaNames[j])
                        Fmatrix[i,j] <- df.dtheta
                        Fmatrix[i,j] <- eval(df2.dtheta2, list(x=xValues[i], theta1=thetaValues[1], theta2=thetaValues[2]))
                        
                }
                
        }
        
       
}


Hessian <- function(thetaNames, thetaValues, xValues, equation){
        
        hessianMatrix <- matrix(nrow=length(xValues),ncol=1)
        doubleDerivativeMatrix <- matrix(nrow=length(thetaValues), ncol=length(thetaValues))
        
        for (k in 1 : length(xValues)){
        
                for (i in 1 : length(thetaValues)){
                
                         for (j in 1 : length(thetaValues)){
                        
                                 df.dtheta <- D(equation, thetaNames[i])
                                df2.dtheta2 <- D(df.dtheta, thetaNames[j])
                                doubleDerivativeMatrix[i,j] <- eval(df2.dtheta2, list(x=xValues[i], theta1=thetaValues[1], theta2=thetaValues[2]))
                        
                         }
                
                 }
                
                (yValues[k] - xValues[k])*doubleDerivativeMatrix
        }
}

Manual Answer

##Double Derivative Matrix

ddx1 <- matrix(nrow=2, ncol=2)
ddx1[1,1] <- eval(D(D(equation, "theta1"), "theta1"), list(x=xValues[1],theta1=thetaValues[1], theta2=thetaValues[2]))
ddx1[1,2] <- eval(D(D(equation, "theta1"), "theta2"), list(x=xValues[1],theta1=thetaValues[1], theta2=thetaValues[2]))
ddx1[2,1] <- eval(D(D(equation, "theta2"), "theta1"), list(x=xValues[1],theta1=thetaValues[1], theta2=thetaValues[2]))
ddx1[2,2] <- eval(D(D(equation, "theta2"), "theta2"), list(x=xValues[1],theta1=thetaValues[1], theta2=thetaValues[2]))

ddx2 <- matrix(nrow=2, ncol=2)
ddx2[1,1] <- eval(D(D(equation, "theta1"), "theta1"), list(x=xValues[2],theta1=thetaValues[1], theta2=thetaValues[2]))
ddx2[1,2] <- eval(D(D(equation, "theta1"), "theta2"), list(x=xValues[2],theta1=thetaValues[1], theta2=thetaValues[2]))
ddx2[2,1] <- eval(D(D(equation, "theta2"), "theta1"), list(x=xValues[2],theta1=thetaValues[1], theta2=thetaValues[2]))
ddx2[2,2] <- eval(D(D(equation, "theta2"), "theta2"), list(x=xValues[2],theta1=thetaValues[1], theta2=thetaValues[2]))


ddx3 <- matrix(nrow=2, ncol=2)
ddx3[1,1] <- eval(D(D(equation, "theta1"), "theta1"), list(x=xValues[3],theta1=thetaValues[1], theta2=thetaValues[2]))
ddx3[1,2] <- eval(D(D(equation, "theta1"), "theta2"), list(x=xValues[3],theta1=thetaValues[1], theta2=thetaValues[2]))
ddx3[2,1] <- eval(D(D(equation, "theta2"), "theta1"), list(x=xValues[3],theta1=thetaValues[1], theta2=thetaValues[2]))
ddx3[2,2] <- eval(D(D(equation, "theta2"), "theta2"), list(x=xValues[3],theta1=thetaValues[1], theta2=thetaValues[2]))

xi1 <- (t(t(yValues))-fmatrix)[1,1]*ddx1
xi2 <- (t(t(yValues))-fmatrix)[2,1]*ddx2
xi3 <- (t(t(yValues))-fmatrix)[3,1]*ddx3

dQ.dtheta <- -2 * t(Fmatrix) %*% (t(t(yValues))-fequation)
Hessian <- 2*t(Fmatrix)%*%Fmatrix-2*(xi1+xi2+xi3)

t(t(thetaValues))-solve(Hessian)%*%dQ.dtheta



##Part f

## i
cv <- c(5.0, 2.5)
FmatrixPartF <- F(thetaNames, cv, xValues, equation)
FFInversePartF <- solve(t(FmatrixPartF)%*%FmatrixPartF)

fequationPartF <- f(c(5.0, 2.5), xValues, equation)

sumVar <- function(yValues, fequationPartF) {
        
                sum <- 0
                
                for (i in 1:3){
                
                part <- (t(t(yValues))[i]-fequationPartF[i])^2
                
                sum = sum + part               

                }

                sum
}

estimatedVariance <- sumVar(yValues, fequationPartF) / (3-2)
s<-sqrt(estimatedVariance)

5.0 + c(-1,1)*qt(.975, 1)*s*sqrt(1.2422824)

##ii

eval(equation, list(x=1, theta1=5.0, theta2=2.5))

eval(D(equation, "theta1"), list(x=1, theta1=5.0, theta2=2.5))
eval(D(equation, "theta2"), list(x=1, theta1=5.0, theta2=2.5))

fForCI <- matrix(c(eval(D(equation, "theta1"), list(x=1, theta1=5.0, theta2=2.5)),eval(D(equation, "theta2"), list(x=1, theta1=5.0, theta2=2.5))) )

cv <- c(5.0, 2.5)
FmatrixPartF <- F(thetaNames, cv, xValues, equation)
FFInversePartF <- solve(t(FmatrixPartF)%*%FmatrixPartF)

CIforY <- 2 + c(-1,1)*qt(.975, 1)*s*sqrt(t(fForCI)%*%FFInversePartF%*%fForCI)
PIforY <- 2 + c(-1,1)*qt(.975, 1)*s*sqrt(1+ t(fForCI)%*%FFInversePartF%*%fForCI)

##iii

hEquation <- expression((theta1/(theta2^2))-.9)
h <- eval(hEquation, list(theta1=5, theta2=2.5))

H <- matrix(nrow=1, ncol=2)
H[1,1] <- eval(D(hEquation, "theta1"), list(theta1=5, theta2=2.5))
H[1,2] <- eval(D(hEquation, "theta2"), list(theta1=5, theta2=2.5))

W <- (h*solve( (H %*% FFInversePartF %*% t(H) ) ) * h ) / estimatedVariance

##iv
yv <- t(t(yValues))
yhat <- matrix(c((5.0/2.5)*.05^2.5, (5.0/2.5)*1.1^2.5, (5.0/2.5)*2.2^2.5))
ymean <- mean(c(.001, 2.6, 14.6))

RPseudo <- 1 - ((.001-0.001118034)^2+(2.6-2.538117413)^2+(14.6-14.357760271)^2) / ((.001 - ymean)^2 + (2.6 - ymean)^2 + (14.6 - ymean)^2)


##v

Leverage <- FmatrixPartF %*% FFInversePartF %*% t(FmatrixPartF)
