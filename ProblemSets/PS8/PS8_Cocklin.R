library(nloptr)
library(tidyverse)

set.seed(100)
N <- 100000
K <- 10
sigma <- 0.5

x <- matrix(rnorm(N*K,mean=0,sd=sigma),N,K)
x[,1] <- 1 # first column of x should be all ones
eps <- rnorm(N,mean=0,sd=0.5)
betaTrue <- c(1.5, -1, -.25, .75, 3.5, -2, .5, 1, 1.25, 2)


y <- x%*% betaTrue + eps
  
  
beta.hat.matrix <- solve(t(x)%*%x)%*%t(x%*%wye)

alpha <- 0.0000003

gradient <- function(beta,y,x) {
  return ( as.vector(-2*t(x)%*%(y-x%*%beta)) )
}

#6

# gradient descent method to find the minimum
iter  <- 1
beta0 <- 0*beta.hat.matrix
beta <- runif(dim(x)[2])
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,y,x)[,iter] <- beta
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}


beta.hat.gd = beta 
# 7 

library(nloptr)
## Our objective function
objfun <- function(beta,y,x) {
  return (sum((y-x%*%beta)^2))
  # equivalently, if we want to use matrix algebra:
  # return ( crossprod(y-x%*%beta) )
}



## initial values
beta0 <- runif(dim(x)[2]) #start at uniform random numbers equal to number of coefficients

## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

## Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,x=x)
print(result)
beta.hat.LBFGS <-result$solution

cbind(beta.hat.matrix,beta.hat.LBFGS, beta.hat.gd)


options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e3)

## Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,x=x)
print(result)
beta.hat.NM <-result$solution

cbind(beta.hat.LBFGS, beta.hat.gd,beta.hat.NM)

#8

library(nloptr)
## Our objective function
objfun  <- function(theta,y,x) {
  # need to slice our parameter vector into beta and sigma components
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-x%*%beta)/sig)^2) ) 
  return (loglike)
}


gradient <- function (theta ,y,x) {
  grad <- as.vector( rep (0, length (theta )))
  beta <- theta [1:( length ( theta) -1)]
  sig <- theta [ length (theta )]
  grad [1:( length ( theta) -1)] <- -t(x)%*%(y - x%*%beta )/(sig^2)
  grad[ length (theta )] <- dim (x)[1] /sig - crossprod (y-x%*%beta )/(sig^3)
  return ( grad )
}

beta0 <- runif(dim(x)[2]+1)

## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

## Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,x=x)
print(result)
beta.hat.mle.LBFGS <-result$solution[[1: (length( result$solution))]]

cbind(beta.hat.matrix,beta.hat.LBFGS,beta.hat.gd,beta.hat.mle.LBFGS)

#9 basic OLS
Bols <- lm( y~ x -1)

library(stargazer)
stargazer(Bols)

