NewtonSolve <- function(x, Fprime, tol=1.e-7, maxsteps=25, verbose=FALSE){
  
  res <- Fprime(x)
  if( abs(res$Fx)<tol ){
    ## ... already got a root with 0 Newton steps
    res$conv <- 0
    return(res)
  }
  
  if(verbose)
    cat("Newton\n Step  x            Fx         DFx        NewtonCorrection\n")
  
  for(step in 1:maxsteps){
    NewtonCorrection <- res$Fx / res$DFx
    
    ## update current guess for root
    x   <- x - NewtonCorrection
    
    ## evaluate target function and its derivative
    res <- Fprime(x)
    
    if(verbose)
      cat(sprintf("%3d   % .3e   % .1e   % .1e   % .1e\n", step, x, res$Fx, res$Fx, NewtonCorrection))
    
    if( abs(res$Fx)<tol ){
      ## got a root with "step" Newton steps
      res$conv <- step
      return(res)
    }
  }
  ## no convergence
  res$conv <- -1
  res
}

ImpliedBSVol <- function(volGuess, Spot, Strike,
                         riskfree, dividend, TTM,
                         Cprice, verbose=FALSE){
  
  ## Evaluates target function whose root is to be found, as well as derivative
  ## wrt sigma of target function.
  ## Target function is the difference between the price obtained from
  ## BS formula evaluated at vol sigma ("BSprice"),
  ## minus actual call price to be inverted ("Cprice").
  BSprime <- function(sigma){
    sTTM    <- sqrt(TTM)
    sigmaT  <- sigma*sTTM
    d1      <- (log(Spot/Strike) + (riskfr - dividend + sigma^2/2)*TTM)/sigmaT
    d2      <- d1 - sigmaT
    Strikerf<- Strike*exp(-riskfree*TTM)
    Spotdivd<- Spot  *exp(-dividend*TTM)
    BSprice <- pnorm(d1)*Spotdivd - pnorm(d2)*Strikerf
    d2prime <- d1/sigma
    d1prime <- sTTM + d2prime
    BSpriceprime <- d1prime*dnorm(d1)*Spotdivd - d2prime*dnorm(d2)*Strikerf
    list(x  =sigma,
         Fx =BSprice-Cprice,
         DFx=BSpriceprime)
  }
  
  res <- NewtonSolve(volGuess,BSprime,verbose=verbose)
  res$x
}


Spot     <- 100
Strike   <- 95
riskfr   <- 0.1
dividend <- 0
TTM      <- 3/12
Cprice   <- 13.7

cat("\nExample 1: data taken from http://www.maxi-pedia.com/Black+Scholes+model\n
    Spot     <- 100
    Strike   <- 95
    riskfr   <- 0.1
    dividend <- 0
    TTM      <- 3/12
    Cprice   <- 13.7
    \n")
ivol <- ImpliedBSVol(volGuess=0.1, Spot, Strike, riskfr, dividend,
                     TTM, Cprice, verbose=TRUE)
cat("\nImplied Black-Scholes volatility: ",ivol*100,"%\n\n",sep="")
rq_iv = EuropeanOptionImpliedVolatility("call", Cprice, Spot, Strike, dividend,
                                        riskfr, TTM, 0.1)
rq_iv[1]

## Example data taken from http://www.maxi-pedia.com/Black+Scholes+model
Spot     <- 1500
Strike   <- 1600
riskfr   <- 0.05
dividend <- 0.03
TTM      <- 3
Cprice   <- 185.8385

cat("\nExample 2: data taken from http://voices.yahoo.com/the-black-scholes-formula-practice-problems-solutions-1297845.html?cat=4\n
    Spot     <- 1500
    Strike   <- 1600
    riskfr   <- 0.05
    dividend <- 0.03
    TTM      <- 3
    Cprice   <- 185.8385
    \n")
ivol <- ImpliedBSVol(volGuess=0.1, Spot, Strike, riskfr, dividend,
                     TTM, Cprice, verbose=TRUE)
cat("\nImplied Black-Scholes volatility: ",ivol*100,"%\n",sep="")

## Example data taken from AKAM EEV example 
Spot     <- 47.29
Strike   <- 46
riskfr   <- 0.005
dividend <- 0
TTM      <- 2/252
Cprice   <- 3.05

cat("\nExample 2: data taken from http://voices.yahoo.com/the-black-scholes-formula-practice-problems-solutions-1297845.html?cat=4\n
    Spot     <- 72.06
    Strike   <- 80
    riskfr   <- 0.005
    dividend <- 0.00
    TTM      <- 6
    Cprice   <- 0.315
    \n")
ivol <- ImpliedBSVol(volGuess=0.1, Spot, Strike, riskfr, dividend,
                     TTM, Cprice, verbose=TRUE)
cat("\nImplied Black-Scholes volatility: ",ivol*100,"%\n",sep="")
rq_iv = EuropeanOptionImpliedVolatility("call", Cprice, Spot, Strike, dividend,
                                        riskfr, TTM, 0.1)
rq_iv[1]
