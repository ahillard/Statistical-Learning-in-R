## Part A

## setwd("C:/Users/ajhkt6/Desktop/Spring 2016/Data Analysis 2")

equation <- expression((t0+t1*x)/(1+t2*exp(0.4*x)))
data <- read.table('S16hw1pr2.dat', col.names=c("x","y"))


par(mfrow=c(1,1))
plot(data)
curve((0.1846-.3693)/(1-.947*exp(0.4*x)), 0, 20, ylim=c(-.5,2.5))

gridt0 <- seq(from=-5, to=5, by=.5)
gridt1 <- seq(from=-5, to=5, by=.5)
gridt2 <- seq(from=-5, to=5, by=.5)


SSEQ1 <- function(gridt0, gridt1, gridt2, yValues, xValues){
        
        gridSearch <- matrix(,4, 9261)
        rownames(gridSearch) <- c("theta0", "theta1", "theta2", "SSE")
        
        colcount <- 1
        
        for (i in 1: length(gridt0))
        {
                
                for (j in 1: length(gridt1))
                {
                        
                        for(l in 1: length(gridt2)){
                                
                                SSE <- 0
                        
                                for (k in 1: length(yValues))
                                {
                                
                                         SSE <- SSE + (yValues[k]-((gridt0[i]+gridt1[j]*xValues[k])/(1+gridt2[l]*exp(0.4*xValues[k]))))^2
                                
                                 }
                        
                                gridSearch[1,colcount] <- gridt0[i]
                                gridSearch[2,colcount] <- gridt1[j]
                                gridSearch[3,colcount] <- gridt2[l]
                                gridSearch[4,colcount] <- SSE 	
                        
                                colcount <- colcount + 1
                        }
                }
        }
        
        gridSearch
        
}


max(output[4,])
output[4,9249]



##Part B




thetaNames <- c("t0", "t1", "t2")
##startingValues <- c(0.184684, -.369368, -.9472331)
startingValues <- c(2.5, -1, 0)
equation <- expression((t0+t1*x)/(1+t2*exp(0.4*x)))
xValues <- data[,1]
yValues <- data[,2]

F <- function(thetaNames, thetaValues, xValues, f){	

	Fmatrix <- matrix(nrow = length(xValues), ncol = length(thetaValues))
	
	for (i in 1 : length(xValues)){
		
		for (j in 1 : length(thetaValues)){
			
			df.dtheta <- D(f, thetaNames[j])
			Fmatrix[i,j] <- eval(df.dtheta, list(x=xValues[i], t0=thetaValues[1], t1=thetaValues[2], t2=thetaValues[3]))
  
		}

	}

	Fmatrix

}


f <- function(thetaValues, xValues, f) {
	
	fmatrix <- matrix(nrow = length(xValues), ncol=1)
	
	for (i in 1 : length(xValues)){

	fmatrix[i,1] <- eval(equation, list(x=xValues[i], t0 = thetaValues[1], t1=thetaValues[2], t2=thetaValues[3]))


	}
	
	fmatrix
}

SSE <- function(xValues, yValues, tValues, equation) {
        
        SSE <- 0
        
        for (i in 1: length(xValues)) {
                
                SSE <- SSE + (yValues[i]-eval(equation, list(x=xValues[i], t0=tValues[1], t1=tValues[2], t2=tValues[3])))^2
                
        }
        
        SSE
}

gaussNewton <- function(thetaNames, startingValues, xValues, yValues, equation, iterations) {
        
        gaussNewtonMatrix <- matrix(nrow=(1+length(startingValues)), ncol=iterations)
        currentThetaValues <- startingValues
        
        for ( i in 1: iterations) {
        
        Fmatrix <- F(thetaNames, currentThetaValues, xValues, equation)
        fequation <- f(currentThetaValues, xValues, equation)
        FFinverse <- solve(t(Fmatrix) %*% Fmatrix)
        delta <- FFinverse %*% t(Fmatrix) %*% (yValues-fequation)
        
        currentSSE <- SSE(xValues, yValues, currentThetaValues, equation)
        potentialSSE <- currentSSE + 1
        k <- -1
        b <- 0
        
        while (potentialSSE > currentSSE && b < 10) {
                
                k <- k + 1
                potentialThetaValues <- currentThetaValues + t(delta)/(2^k)
                potentialSSE <- SSE(xValues, yValues, potentialThetaValues, equation)
                b <- b + 1
                }
        
        currentThetaValues <- currentThetaValues + t(delta)/(2^k)
        
        gaussNewtonMatrix[1,i] <- i
        gaussNewtonMatrix[2,i] <- currentThetaValues[1]
        gaussNewtonMatrix[3,i] <- currentThetaValues[2]
        gaussNewtonMatrix[4,i] <- currentThetaValues[3]
        
        }
        
        gaussNewtonMatrix
}

thetaSolution <- gaussNewton(thetaNames, startingValues, xValues, yValues, equation, 50)[2:4,50]
ObjectiveFunctionValue <- SSE(xValues, yValues, thetaSolution, equation)

EstimatedVariance <- ObjectiveFunctionValue / (250-3)
             
FSolution <- F(thetaNames, thetaSolution, xValues, equation)
FFInverseSolution <- solve(t(FSolution) %*% FSolution)

EstimatedVariance * FFInverseSolution





