# duplicate the iv^2 solver problem in EEV
# the solver problem tries to:
#    minimize price error (S12)
#    where price error = sqrt(sum('VWE SQ' all options in chain)/num options in chain)
#    see below for definition of 'VWE SQ' (P)
# by changing:
#    implied EV (S4)
#    A, B, and C: the coefficients to the ATMIV equations for puts and calls (S6:T8)
#    Vslope and Vcurve of A and B (U6:V6 and U8:V8)
# subject to constraints:
#    Implied EV >= user-input minimum IEV (S4 >= U4)
#    All A, B, and C and Vslope / Vcurve are between +/- user-input maximum (S6:V8 <= W4 and S6:V8 >= -W4)
#    A >= user-input minimum A coefficient (S8:T8 >= V4)
# Attempt at deciphering SolveIV2 column headers:
#    E 'Vega'       = Value from broker platform
#    F 'YrsToExp'   = (Business days to expiry) / 252
#    G 'OVVSkew'    = a bunch of error checking against user-input maximum estimation skew,
#                     ln(Strike Price / (underlying price+dividend adjustment)) / sqrt(YrsToExp),
#                     but he tries to get the max between this skew and the *negative* user input skew
#                     ***seems like he either wants a skew value, maximum skew, or 0 depending.***
#                     This is used later to help calculate Normal IV
#    H 'NormIV'     = Aggregate formula NormIV
#                     BUT the aggregate formula refers back to the currently-being-optimized IEV value
#                     How do you do this in R optimization?
#    I 'ATMNIV'     = plug and chug of *at the money* normalized IV with A, B, C parameters and exp() function
#                     (this models horizontal skew)
#    J 'SlopeParm'  = constant, VslopeA+VslopeB*t, where t=YrsToExp
#    K 'CurveParm'  = constant, VcurveA+VcurveB*t, where t=YrsToExp
#    L 'Est NormIV' = SlopeParm*OVVSkew+CurveParm*(OVVSkew^2)+ATMNIV
#                     (this models final normal IV with vertical skew [ATMNIV already has horizontal skew from above])
#    M 'NIV Error'  = Aggregate formula NormIV - EstNormIV 
#                     (here you're comparing the *normal* IV found through aggregate formula with the estimate 
#                     from the proprietary volatility model)
#                     But you are simultaneously using the IEV you're currently estimating because it's
#                     in the aggregate formula and one of the optimizer parameters???
#                     BUT the aggregate formula refers back to the currently-being-optimized IEV value
#    N 'NIVErr SQ'  = (NIVErr)^2
#    O 'VW Err'     = NIVErr*Vega*(Crazy NV/IV Multiplier with hard coded constants?)
#                     He denotes this in dollars/cents, because that's how Vega is quoted. This price
#                     error isn't total per share price (can't be, haven't estimated Greeks yet)
#    P 'VWE SQ'     = (VWErr)^2
#    Q 'UVAL'       = NIVErr*(-1*Vega)*(Crazy NV/IV Multiplier?)*100/(user-input option multiplier)
#                     Find under/overpriced options on a per share basis
#                     If it's 100 shares per contract then UVAL = -1*VW Err

# TODO figure out:
#    DONE What is that crazy multiplier thing? (is it in the book? what page?)
#       TN30 Vega? Chapter 4, figure 4.5? It's not the same, because it deals with actual time to expiration
#        and this multiplier thing deals with % normal vs. trade days remaining.
#       Whole premise is that volatility mean-reverts, and shorter-dated options' IV changes more than
#        longer-dated options for the same 1% increase/decrease in ATM normal IV.
#       Is this just a different form of the TN30 Vega formula because its input is a 
#        ratio of time instead of time itself? How could you re-work/prove it?
#    DONE How does the maximum skew fit in?
#       Only explained in the book as "capping the max VSC avoids extreme and unusual
#         volatility skew solution and this value should not be modified."
#    DONE Why does the VW Err value match the UVAL even when input params are random?
#       Because the only thing UVAL is doing is flipping the sign and adjusting for # options/contract
#    DONE Use 'bizdays' package for business day checks
#    So you have the model's NIV through the ATMNIV + vert skew, then you compare that to
#       the NIV derived through the aggregate formula? and figure out the IEV that results in the minimum
#       error across the matrix 
#    Make sure you're getting needed stuff from TZero tab
#       OVVSkew
#       NormIV

library(bizdays)
library(GenSA)
library(rootSolve)
library(RQuantLib)

setwd("~/Documents/TradingWithR/eev")

# User inputs:
uiMinIEV  = 0.011 # 1.1%
uiMinA    = 0.011
uiMaxParm = 6     # 600%
uiMaxSkew = 1
uiOptMult = 100

underly  <- 72.05
riskfr   <- 0.0025
dividend <- 0

analysis_date   = "2015-07-30"
first_u_date    = "2015-07-31"
second_u_date   = "2015-07-31"
next_earn_date  = "2015-07-30"
next_ext_date   = "2015-08-07"
cal_begin       = "2014-01-01"
cal_end         = "2019-01-01"
datecode        = "20150730"
timecode        = "1430"
ticker          = "ea"

uiHolidays = c('2014-01-01', '2014-01-20', '2014-02-17',
             '2014-04-18', '2014-05-26', '2014-07-03',
             '2014-09-01', '2014-11-27', '2014-12-25',
             '2015-01-01', '2015-01-19', '2015-02-16',
             '2015-04-03', '2015-05-25', '2015-07-03',
             '2015-09-07', '2015-11-26', '2015-12-25',
             '2016-01-01', '2016-01-18', '2016-02-15',
             '2016-03-25', '2016-05-30', '2016-07-04',
             '2016-09-05', '2016-11-24', '2016-12-26',
             '2017-01-02', '2017-01-16', '2017-02-20',
             '2017-04-14', '2017-05-29', '2017-07-04',
             '2017-09-04', '2017-11-23', '2017-12-25',
             '2018-01-01', '2018-01-15', '2018-02-19',
             '2018-03-30', '2018-05-28', '2018-07-04',
             '2018-09-03', '2018-11-22', '2018-12-25',
             '2019-01-01')

options_file = paste(ticker, datecode, timecode, '.csv',
                     sep='')
stock_file   = paste(ticker, '.asc', sep='')
mydata       = read.csv(options_file)
myivprice    = read.csv(stock_file)
myivprice    = myivprice[!is.na(myivprice$IV30),] # no NA iv
underly      = mydata[1,]$Last
mydata       = mydata[-1,]                        # top row is underlying

# Set up only to use those strikes with bids/asks, look for Date
my.matrix = mydata[!is.na(mydata$Date),]

# Optimizer parameters are only constants because they directly feed back into 
# formulas in Excel which have the objective function in it. So what's the obj
# function this time around? Is it just the output of the VWE Sq which then 
# calls other functions? [yes]
# Ac, Bc, Cc, Ap, Bp, Cp, VSA, VSB, VCA, VCB, IEV
# [1] [2] [3] [4] [5] [6] [7]  [8]  [9]  [10]  [11]
optLower = numeric(11) # TODO set defaults here
optUpper = numeric(11) # TODO set defaults here
toOpt    = numeric(11) # TODO randomize these between upper/lower defaults?

# oh god these functions are from the internet
# but they make values match more or less Brian's sheet (mean(err) ~ 1e-5)
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

# another from the internet with 'type' added by me
ImpliedBSVol <- function(type, volGuess, Spot, Strike,
                         riskfree, dividend, TTM,
                         Cprice, verbose=FALSE){
  
  ## Evaluates target function whose root is to be found, as well as derivative
  ## wrt sigma of target function.
  ## Target function is the difference between the price obtained from
  ## BS formula evaluated at vol sigma ("BSprice"),
  ## minus actual call price to be inverted ("Cprice").
  BSprimeCalls <- function(sigma){
    sTTM    <- sqrt(TTM)
    sigmaT  <- sigma*sTTM
    d1      <- (log(Spot/Strike) + (riskfr - dividend + sigma^2/2)*TTM)/sigmaT
    d2      <- d1 - sigmaT
    Strikerf<- Strike*exp(-riskfree*TTM)
    Spotdivd<- Spot  *exp(-dividend*TTM)
    # calls
    BSprice <- pnorm(d1)*Spotdivd - pnorm(d2)*Strikerf
    d2prime <- d1/sigma
    d1prime <- sTTM + d2prime
    BSpriceprime <- d1prime*dnorm(d1)*Spotdivd - d2prime*dnorm(d2)*Strikerf
    list(x  =sigma,
         Fx =BSprice-Cprice,
         DFx=BSpriceprime)
  }
  
  BSprimePuts <- function(sigma){
    sTTM    <- sqrt(TTM)
    sigmaT  <- sigma*sTTM
    d1      <- (log(Spot/Strike) + (riskfr - dividend + sigma^2/2)*TTM)/sigmaT
    d2      <- d1 - sigmaT
    Strikerf<- Strike*exp(-riskfree*TTM)
    Spotdivd<- Spot  *exp(-dividend*TTM)
    # puts
    BSprice <- pnorm(-d2)*Strikerf - pnorm(-d1)*Spotdivd
    d2prime <- d1/sigma
    d1prime <- sTTM + d2prime
    BSpriceprime <- d1prime*dnorm(d1)*Spotdivd - d2prime*dnorm(d2)*Strikerf
    list(x  =sigma,
         Fx =BSprice-Cprice,
         DFx=BSpriceprime)
  }
  if (type == "call") {
    res <- NewtonSolve(volGuess,BSprimeCalls,verbose=verbose)
    res$x
  } else if (type == "put") {
    res <- NewtonSolve(volGuess,BSprimePuts,verbose=verbose)
    res$x
  }
  else {
    stop("neither put nor call specified")
  }
}

# Some functions for easy stuff above
fSlopeParm = function(vsa, vsb, t) {
  return (vsa+vsb*t)
}

fCurveParm = function(vca, vcb, t) {
  return (vca+vcb*t)
}

fEstNIV = function(slopeParm, ovvSkew, curveParm, atmniv) {
  return (slopeParm*ovvSkew+curveParm*(ovvSkew^2)+atmniv)
}

# Make sure you're calling this with the correct put/call parameters!!
fATMNIV = function(A, B, C, t) {
  return(A+B*(1-exp(-C*t)))
}

# Replicate that hard-coded multiplier, where x = (non-ED / total biz days)
fIVNVMult = function(x) {
  return(1.7628*(x^2)-1.7138*(x)+0.9066)
}

# Limit max vertical skew
# TODO the max() and min() calls won't correctly operate on array
# use pmin() and pmax() with different setup.
fOVVSkew = function(K, U, divadj, t, maxSkew) {
  # K and t are vectors
  #ln(Strike Price / (underlying price+dividend adjustment)) / sqrt(YrsToExp),
  #but he tries to get the max between this skew and the *negative* user input skew
  #***seems like he either wants a skew value, maximum skew, or 0 depending.***
  myMaxSkew = rep(maxSkew, length(K))
  myMaxSkew[t!=0] = pmax(pmin(log(K[t!=0] / (U+divadj)) / (t[t!=0]^0.5), 
                              myMaxSkew), 
                         -1*myMaxSkew)
  myMaxSkew[t==0] = 0
  return(myMaxSkew)
}

fNormIV = function(busDays, earnDays, normDays, IEV, calcIV) {
  # uses calculated IV which will end up calling NewtRaph / rootSolve function
  myZeroes= rep(0, length(busDays))
  return((pmax(busDays*calcIV^2-earnDays*IEV^2, myZeroes) / normDays)^(0.5))
}

fcalcIV = function(optType, midPrice, U, K, divadj, riskfr, t, avgIV) {
  # avgIV is average of all "Mid IV" from broker platform of whole matrix
  #   and is used for the initial guess of the BSOPM root-finding stuff.
  # This should calculate the IV per BSOPM. Only doing this for short name.
  # TODO: check to see if user has specified EU options
  return (AmericanOptionImpliedVolatility(optType, midPrice, U, K, divadj, 
                                          riskfr, t, avgIV))
}

# TODO find a way to exclude individual options if they have pricing errors
fpriceErr = function(vweSQ) {
  return ((sum(vweSQ)/length(vweSQ))^0.5)
}

# find error priced in cents? in terms of vega?
fvwErr = function(normIV, estNormIV, vega, ivNVMult) {
  return((normIV-estNormIV)*vega*ivNVMult)
}

# Newton-Raphson root finding method for BS
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

# implementation of Black-Scholes using Newton-Raphson method
ImpliedBSVol <- function(type, volGuess, Spot, Strike,
                         riskfree, dividend, TTM,
                         Cprice, verbose=FALSE){
  
  ## Evaluates target function whose root is to be found, as well as derivative
  ## wrt sigma of target function.
  ## Target function is the difference between the price obtained from
  ## BS formula evaluated at vol sigma ("BSprice"),
  ## minus actual call price to be inverted ("Cprice").
  BSprimeCalls <- function(sigma){
    sTTM    <- sqrt(TTM)
    sigmaT  <- sigma*sTTM
    d1      <- (log(Spot/Strike) + (riskfr - dividend + sigma^2/2)*TTM)/sigmaT
    d2      <- d1 - sigmaT
    Strikerf<- Strike*exp(-riskfree*TTM)
    Spotdivd<- Spot  *exp(-dividend*TTM)
    # calls
    BSprice <- pnorm(d1)*Spotdivd - pnorm(d2)*Strikerf
    d2prime <- d1/sigma
    d1prime <- sTTM + d2prime
    BSpriceprime <- d1prime*dnorm(d1)*Spotdivd - d2prime*dnorm(d2)*Strikerf
    list(x  =sigma,
         Fx =BSprice-Cprice,
         DFx=BSpriceprime)
  }
  
  BSprimePuts <- function(sigma){
    sTTM    <- sqrt(TTM)
    sigmaT  <- sigma*sTTM
    d1      <- (log(Spot/Strike) + (riskfr - dividend + sigma^2/2)*TTM)/sigmaT
    d2      <- d1 - sigmaT
    Strikerf<- Strike*exp(-riskfree*TTM)
    Spotdivd<- Spot  *exp(-dividend*TTM)
    # puts
    BSprice <- pnorm(-d2)*Strikerf - pnorm(-d1)*Spotdivd
    d2prime <- d1/sigma
    d1prime <- sTTM + d2prime
    BSpriceprime <- d1prime*dnorm(d1)*Spotdivd - d2prime*dnorm(d2)*Strikerf
    list(x  =sigma,
         Fx =BSprice-Cprice,
         DFx=BSpriceprime)
  }
  if (type == "call") {
    res <- NewtonSolve(volGuess,BSprimeCalls,verbose=verbose)
    res$x
  } else if (type == "put") {
    res <- NewtonSolve(volGuess,BSprimePuts,verbose=verbose)
    res$x
  }
  else {
    stop("neither put nor call specified")
  }
}

# Set up calendar
my.cal = create.calendar(holidays = uiHolidays, 
                         start.date = cal_begin, 
                         end.date= cal_end, 
                         weekdays=c("saturday", "sunday"),
                         name="my.cal")
# sample bizdays call: bizdays("2014-01-02", "2014-01-21", my.cal) = 12

# Set up magic optimization problem. If you had a cool function, you'd just
# call minimizeThis(fpriceErr(vweSQ))
# but vweSQ is an array of squared vwErr: fvwErr()^2
# but fvwErr() needs to call fNormIV() and fEstNIV()
#   and fnormIV() has *as its input* the current iev, which is a value in the
#     array being passed to the optimizer
#   and fEstNIV() also has as its input some current values in the array
#     being optimized
# so do you end up doing this BS? do you call functions array wise or what?
# is there an "R" way to do this? maybe read more optimization examples...
# minimize(
#  fpriceErr(
#    vweSQ(
#       vwErr(
#         fNormIV(m$BD, 
#                 m$ED, 
#                 m$ND, 
#                 IEV, 
#                 fcalcIV(...)), 
#         fEstNIV(...), 
#         m$vega, 
#         ivNVMult(...)
#       )
#     )
#   )
# )
# Does it even work at all?
# Step by step:
#   1. SETUP: Calculate IV with for loop
#   2. SETUP: Calculate biz days / normal days 
#   3. SETUP: Initial guess on IEV and slope parameters
#   4. Calculate NIV from (BSOPM + days) info
#   5. Calculate curves for the slopes and that way to estimate ATMNIV
#   6. Calculate NIV using the #5 ATMNIV and OVVSkew 
#      and IEV day-weighted equation
#   7. Calculate the error between values in #4 and #6.
#      (optimizer is minimizing this)
# 
my.matrix$expISO = as.Date(as.character(my.matrix$Exp.Date), 
                                "%y%m%d")

# Calculate total days
my.matrix$ND     = bizdays(analysis_date, 
                                my.matrix$expISO, 
                                my.cal)

# ur in R use ur vectorz 4 date maffs
my.matrix$ED     = 1+floor(bizdays(next_earn_date, 
                                        my.matrix$expISO,
                                        my.cal) / 63)
my.matrix$BD     = my.matrix$ND - my.matrix$ED

# No options at expiration
my.matrix        = my.matrix[my.matrix$ND != 1,]

# No options without quotes
my.matrix$avgp   = (my.matrix$Bid + my.matrix$Asked) / 2
my.matrix        = my.matrix[!is.na(my.matrix$avgp),]

# Set up some other helper columns / values
my.matrix$type[my.matrix$Call.Put == "C"] = "call"
my.matrix$type[my.matrix$Call.Put == "P"] = "put"

# Calculate IV using BSOPM 
avg.iv = mean(my.matrix$Mid.IV[!is.na(my.matrix$Mid.IV)])
for (i in 1:length(my.matrix$Strike.Price)) {
  my.matrix$CalcIV[i] = 
    ImpliedBSVol(my.matrix$type[i],
                 avg.iv,
                 underly,
                 my.matrix$Strike.Price[i],
                 riskfr,
                 dividend,
                 my.matrix$ND[i]/252,
                 my.matrix$avgp[i],
                 verbose=FALSE)
  #     AmericanOptionImpliedVolatility(my.matrix$type[i], 
  #                                     my.matrix$avgp[i],
  #                                     underly,
  #                                     my.matrix$Strike.Price[i],
  #                                     dividend, 
  #                                     riskfr, 
  #                                     my.matrix$ND[i]/252,
  #                                     avg.iv)
}

# try out one of them there functions
maxOVVSkew = 1
my.matrix$OVVSkew = fOVVSkew(my.matrix$Strike.Price,
                                  underly,
                                  dividend,
                                  my.matrix$ND/252,
                                  maxOVVSkew)

# Some rough testing using some shared setup from <RQL examples.R>
toOpt[11] = 1.1439
my.matrix$NormIV = fNormIV(my.matrix$ND, 
                                my.matrix$ED, 
                                my.matrix$BD,
                                toOpt[11],
                                my.matrix$CalcIV)
# Ac, Bc, Cc, Ap, Bp, Cp, VSA, VSB, VCA, VCB, IEV
# [1] [2] [3] [4] [5] [6] [7]  [8]  [9]  [10]  [11]
toOpt[7:10] = c(-0.0613, -0.0836, 0.0215, 0.6534)
my.matrix$slope = fSlopeParm(toOpt[7], toOpt[8], my.matrix$ND/252)
my.matrix$curve = fSlopeParm(toOpt[9], toOpt[10], my.matrix$ND/252)

toOpt[1:6] = c(0.2738, -3.0751, -0.0076, 0.2691, -0.1371, -0.0684)
my.matrix$atmniv[my.matrix$type == "call"] = fATMNIV(toOpt[1], 
                                                     toOpt[2], 
                                                     toOpt[3], 
                   my.matrix$ND[my.matrix$type == "call"]/252)

my.matrix$atmniv[my.matrix$type == "put"] = fATMNIV(toOpt[4],
                                                    toOpt[5], 
                                                    toOpt[6], 
                   my.matrix$ND[my.matrix$type == "put"]/252)

my.matrix$estniv = fEstNIV(my.matrix$slope, 
                                my.matrix$OVVSkew, 
                                my.matrix$curve,
                                my.matrix$atmniv)

my.matrix$nivErr = my.matrix$NormIV - my.matrix$estniv

my.matrix$nvivMult =  fIVNVMult(my.matrix$BD / my.matrix$ND)

my.matrix$vwErr = fvwErr(my.matrix$NormIV,
                              my.matrix$estniv,
                              my.matrix$Vega,
                              my.matrix$nvivMult)

# next part doesn't match spreadsheet, but maybe it's something in fvwErr()
# no, see above, niv calcs aren't right due to bogus IEV test value
my.matrix$vwErr2 = my.matrix$vwErr^2

print(fpriceErr(my.matrix$vwErr2)) # typo with Aput not being used? 0.1220204
# results with Aput used: 0.1202432

sum(my.matrix$vwErr2)  #  4.049805
mean(my.matrix$vwErr2) #  0.01488899
max(my.matrix$nivErr)  #  0.1705075
min(my.matrix$nivErr)  # -0.2535272
max(my.matrix$vwErr)   #  0.2759597
min(my.matrix$vwErr)   # -0.6587683

