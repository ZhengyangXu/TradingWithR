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
maxOVVSkew = 1

underly  <- 60.63
riskfr   <- 0.0025
dividend <- 0

analysis_date   = "2014-07-23"
first_u_date    = "2014-07-24"
second_u_date   = "2014-07-24"
next_earn_date  = "2014-07-23"
next_ext_date   = "2014-08-07"
cal_begin       = "2014-01-01"
cal_end         = "2019-01-01"
datecode        = "20140723"
timecode        = "1600"
ticker          = "ua"

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
stock_file   = paste(ticker, '.csv', sep='')
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
optLower = c(uiMinA, rep(-1*uiMaxParm, 2), 
             uiMinA, rep(-1*uiMaxParm, 2), 
             rep(-1*uiMaxParm, 4),
             uiMinIEV)
optUpper = rep(uiMaxParm, 11)
#my.opt    = runif(11)          # GenSA generates random numbers itself

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

# Find the *normal* IV component given *aggregate* IV and *IEV*
fNormIV = function(busDays, earnDays, normDays, IEV, calcIV) {
  # uses calculated IV which will end up calling NewtRaph / rootSolve function
  myZeroes= rep(0, length(busDays))
  return((pmax(busDays*calcIV^2-earnDays*IEV^2, myZeroes) / normDays)^(0.5))
}

# Find the *aggregate* IV component given *normal* IV and *IEV*
fAggIV = function(busDays, earnDays, normDays, IEV, normIV) {
  w.iev = earnDays / busDays
  w.niv = normDays / busDays
  return(((w.iev*IEV^2)+(w.niv*normIV^2))^0.5)
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
#         fNormIV(m$ND, 
#                 m$ED, 
#                 m$BD, 
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
my.matrix$BD     = bizdays(analysis_date, 
                                my.matrix$expISO, 
                                my.cal)

# ur in R use ur vectorz 4 date maffs
my.matrix$ED     = 1+floor(bizdays(next_earn_date, 
                                        my.matrix$expISO,
                                        my.cal) / 63)
my.matrix$ND     = my.matrix$BD - my.matrix$ED

# No options at expiration
#my.matrix        = my.matrix[my.matrix$BD != 1,]
# No options outside of 2 and 504 days
my.matrix        = subset(my.matrix, BD >= 2 & BD <= 300)

# No options outside of 4/96 deltas
my.matrix        = subset(my.matrix, abs(Delta) > 4 & abs(Delta) < 96 )
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
                 my.matrix$BD[i]/252,
                 my.matrix$avgp[i],
                 verbose=FALSE)
  #     AmericanOptionImpliedVolatility(my.matrix$type[i], 
  #                                     my.matrix$avgp[i],
  #                                     underly,
  #                                     my.matrix$Strike.Price[i],
  #                                     dividend, 
  #                                     riskfr, 
  #                                     my.matrix$BD[i]/252,
  #                                     avg.iv)
}

############################## optimize time starts here?

doStuff = function(my.opt) {
  my.matrix$OVVSkew = fOVVSkew(my.matrix$Strike.Price,
                               underly,
                               dividend,
                               my.matrix$BD/252,
                               maxOVVSkew)
  
  # Some rough testing using some shared setup from <RQL examples.R>
  #my.opt[11] = 1.1439
  my.matrix$NormIV = fNormIV(my.matrix$BD, 
                             my.matrix$ED, 
                             my.matrix$ND,
                             my.opt[11],
                             my.matrix$CalcIV)
  # Ac, Bc, Cc, Ap, Bp, Cp, VSA, VSB, VCA, VCB, IEV
  # [1] [2] [3] [4] [5] [6] [7]  [8]  [9]  [10]  [11]
  #my.opt[7:10] = c(-0.0613, -0.0836, 0.0215, 0.6534)
  my.matrix$slope = fSlopeParm(my.opt[7], my.opt[8], my.matrix$BD/252)
  my.matrix$curve = fSlopeParm(my.opt[9], my.opt[10], my.matrix$BD/252)
  
  #my.opt[1:6] = c(0.2738, -3.0751, -0.0076, 0.2691, -0.1371, -0.0684)
  my.matrix$atmniv[my.matrix$type == "call"] = fATMNIV(my.opt[1], 
                                                       my.opt[2], 
                                                       my.opt[3], 
                     my.matrix$BD[my.matrix$type == "call"]/252)
  
  my.matrix$atmniv[my.matrix$type == "put"] = fATMNIV(my.opt[4],
                                                      my.opt[5], 
                                                      my.opt[6], 
                     my.matrix$BD[my.matrix$type == "put"]/252)
  
  my.matrix$estniv = fEstNIV(my.matrix$slope, 
                             my.matrix$OVVSkew, 
                             my.matrix$curve,
                             my.matrix$atmniv)
  
  my.matrix$nivErr = my.matrix$NormIV - my.matrix$estniv
  
  my.matrix$nvivMult =  fIVNVMult(my.matrix$ND / my.matrix$BD)
  
  my.matrix$vwErr = fvwErr(my.matrix$NormIV,
                           my.matrix$estniv,
                           my.matrix$Vega,
                           my.matrix$nvivMult)
  
  
  my.matrix$vwErr2 = my.matrix$vwErr^2
  
  #print(fpriceErr(my.matrix$vwErr2))
  return(fpriceErr(my.matrix$vwErr2))
}

# these only work if you put in the final optimizer results from excel
#sum(my.matrix$vwErr2)  #  4.049805
#mean(my.matrix$vwErr2) #  0.01488899
#max(my.matrix$nivErr)  #  0.1705075
#min(my.matrix$nivErr)  # -0.2535272
#max(my.matrix$vwErr)   #  0.2759597
#min(my.matrix$vwErr)   # -0.6587683
#print(doStuff())

results = list()
full.results = list()
max.tries = 10
for (i in 1:max.tries) {
  out.summary = list(finalrsq=0)
  while (out.summary[["finalrsq"]] == 0 || out.summary[["finalrsq"]] < 0.7) {
    out = GenSA(lower = optLower, upper = optUpper, fn = doStuff,
                control = list(max.time=5, temperature=10000))
    #out = GenSA(lower = optLower, upper = optUpper, fn = doStuff)
    #print(out[c("value", "par", "counts")])
    toOpt = out$par
    out.summary = optSummary(T)
    print(unlist(optSummary(T)))
  }
  results[[i]] = out$par[11]
  full.results[[i]] = out$par
}
#print(unlist(results))
plot(1:max.tries, results, type='l')
hist(unlist(results))
print(summary(unlist(results)))

# repro with values shipped in book
toOpt = c(0.265162712164448, 2.99783601119151, 0.00670523714905462,
          0.323365906648514, -0.0381381150533562, 6,
          0.0255628691868997, -0.0574220811250519,
          0.211917788619586, -0.101359387380082,
          1.37201568790316)
# Black-Scholes Option Value
# Call value is returned in values[1], put in values[2]
# spot, strike, risk free rate, time until expiration, IV, type
blackscholes <- function(S, X, rf, T, sigma, my.type) {
  d1 <- (log(S/X)+(rf+sigma^2/2)*T)/(sigma*sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  if (my.type == "call") {
    return (S*pnorm(d1) - X*exp(-rf*T)*pnorm(d2))
  } else if (my.type == "put") {
    return(X*exp(-rf*T) * pnorm(-d2) - S*pnorm(-d1))
  } else {
    return(NULL)
  }
}

TrueGreek = function(spot = underly, rfr = riskfr, 
                     iv.mod = 0, iv.replace = NULL) {
  if(length(iv.replace) == 0) {
    my.iv = my.matrix$CalcIV + rep(iv.mod, nrow(my.matrix))
  } else {
    my.iv = iv.replace
  }
  
  my.new.prices = list()
  for (j in 1:nrow(my.matrix))
    my.new.prices[[j]] = blackscholes(spot, 
                                     my.matrix$Strike.Price[j], 
                                     rfr,
                                     my.matrix$BD[j] / 252,
                                     my.iv[j],
                                     my.matrix$type[j])
  my.new.prices
}

# check results against computed prices (should be on scale of ~1e-12 - 1e-15)
# x = (my.matrix$Bid + my.matrix$Asked) / 2 - unlist(TrueGreek())
# summary(x)
# print(sum(x))
midprice   = (my.matrix$Bid + my.matrix$Asked) / 2

########## True Delta #############

d.po.plus  = unlist(TrueGreek(spot = (underly+1))) # calls go up for +1
d.po.minus = unlist(TrueGreek(spot = (underly-1))) # calls go down for -1
true.delta = (d.po.plus - d.po.minus) / 2 * 100
#plot(my.matrix$Strike.Price, my.matrix$Delta/100 - true.delta)

new.df = cbind.data.frame(my.matrix, d.po.plus)
new.df = cbind.data.frame(new.df, d.po.minus)
new.df = cbind.data.frame(new.df, true.delta)
subset(new.df, BD == 7 & Strike.Price == 60)

########## True Gamma #############

g.po.plus  = unlist(TrueGreek(spot = (underly+2))) # calls go up for +1
g.po.minus = unlist(TrueGreek(spot = (underly-2))) # calls go down for -1
true.delt2 = (g.po.plus - g.po.minus) / 4 * 100
true.gamma = (true.delta - true.delt2) / 2
#plot(my.matrix$Strike.Price, my.matrix$Gamma/100 - true.gamma)

new.df = cbind.data.frame(new.df, g.po.plus)
new.df = cbind.data.frame(new.df, g.po.minus)
new.df = cbind.data.frame(new.df, true.delt2)
new.df = cbind(data.frame(new.df, true.gamma))

########## True Earnings Vega ##########
# Aggregate IV changes due to the change in IEV, but not the normal IV,
# because by definition we are holding that constant. 
agg.iv.plus  = fAggIV(new.df$BD, 
                      new.df$ED, 
                      new.df$ND, 
                      toOpt[11]+0.01, # IEV changes in aggregate but not normal!
                      fNormIV(new.df$BD, 
                              new.df$ED, 
                              new.df$ND, 
                              toOpt[11], 
                              new.df$CalcIV))
agg.iv.minus = fAggIV(new.df$BD, 
                      new.df$ED, 
                      new.df$ND, 
                      toOpt[11]-0.01, # IEV changes in aggregate but not normal!
                      fNormIV(new.df$BD, 
                              new.df$ED, 
                              new.df$ND, 
                              toOpt[11], 
                              new.df$CalcIV))
tev.po.plus  = unlist(TrueGreek(iv.replace = agg.iv.plus))
tev.po.minus = unlist(TrueGreek(iv.replace = agg.iv.minus))
true.earn.vega = (tev.po.plus - tev.po.minus) / 2
new.df = cbind.data.frame(new.df, true.earn.vega)

########## True Normal 30 Vega #############

# Brian's curve fit function across a broad range of securities to 
# deal with the fact that changes in ATM IV do not scale horizontally across
# expirations. Short term options are affected more by changes in ATM IV than
# longer term options. fNormMult(5/252) = 1.57 and fNormMult(1) = 0.219
# argument ttm is in years
# this function gives the multiplier when finding True Normal 30 Vega,
# the component of vega that does not change due to earnings. BSOPM assumes
# TN30 Vega is 1 for all expirations, which is empirically false
#
# in english, a 1 week ATM call option that sees an increase of 1% in ATM IV
# should change in value by +57% more than what BSOPM says it should
#
# example: (blackscholes(100, 100, 0.0025, 5/252, 0.26, "call") - 
#           blackscholes(100, 100, 0.0025, 5/252, 0.25, "call")) = 0.05618412
# but:
#          (blackscholes(100, 100, 0.0025, 5/252, 0.26, "call") - 
#           blackscholes(100, 100, 0.0025, 5/252, 0.25, "call")) * 
#           fNormMult(5/252) = 0.0881926
# BSOPM says this call should only change by $0.056 but with this added 
# multiplier (and in the real world) we should expect to see it change by $0.088
fNormMult = function(ttm) {
  return(1+(-0.780854818)*(1-exp(-8.629216*(ttm-1/12))))
}

# re-estimate skew parameters with ATM+0.01
# re-calculate normal IV across matrix
# multiply new normal IV by multiplier
# use aggregate formula to get new agg IV (IEV stays constant)
# use BSOPM to calculate new option prices 
# diff the prices, divide by 2

new.df$OVVSkew = fOVVSkew(new.df$Strike.Price,
                             underly,
                             dividend,
                          new.df$BD/252,
                             maxOVVSkew)
# Ac, Bc, Cc, Ap, Bp, Cp, VSA, VSB, VCA, VCB, IEV
# [1] [2] [3] [4] [5] [6] [7]  [8]  [9]  [10]  [11]
new.df$slope = fSlopeParm(toOpt[7], toOpt[8], new.df$BD/252)
new.df$curve = fSlopeParm(toOpt[9], toOpt[10], new.df$BD/252) # not a bug

new.df$atmniv[new.df$type == "call"] = fATMNIV(toOpt[1], 
                                               toOpt[2], 
                                               toOpt[3], 
                   new.df$BD[new.df$type == "call"]/252)

new.df$atmniv[new.df$type == "put"] = fATMNIV(toOpt[4],
                                              toOpt[5], 
                                              toOpt[6], 
                   new.df$BD[new.df$type == "put"]/252)

new.df$estniv.p1 = fEstNIV(new.df$slope, 
                           new.df$OVVSkew, 
                           new.df$curve,
                           new.df$atmniv+0.01)
new.df$estniv.m1 = fEstNIV(new.df$slope, 
                           new.df$OVVSkew, 
                           new.df$curve,
                           new.df$atmniv-0.01)
new.df$tn30v.agg.iv.p1 = fAggIV(new.df$BD, new.df$ED, new.df$ND, toOpt[11],
                                new.df$estniv.p1)
new.df$tn30v.agg.iv.m1 = fAggIV(new.df$BD, new.df$ED, new.df$ND, toOpt[11],
                                new.df$estniv.m1)

new.df$tn30v.po.p1  = unlist(TrueGreek(iv.replace = new.df$tn30v.agg.iv.p1))
new.df$tn30v.po.m1  = unlist(TrueGreek(iv.replace = new.df$tn30v.agg.iv.m1))

new.df$tn30v = (new.df$tn30v.po.p1 - 
                  new.df$tn30v.po.m1)*fNormMult(new.df$BD/252)/2

############ True 30 Vega ########

# combine true earnings vega + true normal 30 vega, literally just addition
# not sure where it's used  yet

############ True Theta ##########

# it's just
# blackscholes(underly, 60, riskfr, 7/252, 0.2654666, "call") - 2.675
# not any of the rest of this shit
# the BD used in TrueGreek is key, it's 7 not 6

# new.df$tt.agg.iv.m1 = fAggIV(new.df$BD,
#                              new.df$ED-1,
#                              new.df$ND+1,
#                              toOpt[11],
#                              fNormIV(new.df$BD,
#                                      new.df$ED-1,
#                                      new.df$ND+1,
#                                      toOpt[11],
#                                      new.df$CalcIV))
# tt.agg.iv.m1 = list()
# for (i in 1:nrow(new.df)) {
#   tt.agg.iv.m1[[i]] = ImpliedBSVol(new.df$type[i],
#                                    avg.iv,
#                                    underly,
#                                    new.df$Strike.Price[i],
#                                    riskfr,
#                                    dividend,
#                                    new.df$BD[i]/252,
#                                    new.df$avgp[i],
#                                    verbose=FALSE)
# }
# new.df$tt.agg.iv.m1 = unlist(tt.agg.iv.m1)
new.df$tt.po.m1 = unlist(TrueGreek(iv.replace = new.df$estniv))
new.df$true.theta = new.df$tt.po.m1 - new.df$avgp

######### True Rho ###########

new.df$tr.po.p1 = unlist(TrueGreek(rfr = riskfr + 0.001))
new.df$tr.po.m1 = unlist(TrueGreek(rfr = riskfr - 0.001))
new.df$true.rho = (new.df$tr.po.p1 - new.df$tr.po.m1) / .2

subset(new.df, BD == 7 & Strike.Price == 60 & type == "call")

######### Calculate values for user date ############
# don't forget to add / filter options further for simulation?
# How in the hell?
#   0.  Construct the prices to iterate over
#       Then for each price:
#   1.  grab the estimate normal IV from fEstNIV
#       make sure you decrement t in the fATMNIV() call by days until U date
#       make sure you use this new potential underlying price in fOVVSkew()
#   2.  grab the IEV from toOpt[11] -- (short term options, it won't matter)
#   3.  compute aggregate IV with fAggIV (is this CalcIV? or very close?)
#   4.  Compute new option price with new price from (0) and new aggIV from (3)
#       using BSOPM. True greeks come later when looking at total position?

# we try example of the 60 call w/ 7 days left 1 day later @ 68.07
tg.ex = list()
tg.ex[["atmniv"]]  = fATMNIV(toOpt[1], toOpt[2], toOpt[3], 6/252)
tg.ex[["slope"]]   = fSlopeParm(toOpt[7], toOpt[8], 6/252)
tg.ex[["curve"]]   = fSlopeParm(toOpt[9], toOpt[10], 6/252)
tg.ex[["skew"]]    = fOVVSkew(60, 68.07, 0, 6/252, uiMaxSkew)
tg.ex[["estniv"]]  = fEstNIV(tg.ex$slope, tg.ex$skew, tg.ex$curve, tg.ex$atmniv)
tg.ex[["aggiv"]]   = fAggIV(6, 0, 6, toOpt[11], tg.ex$estniv) # same for ED=0
tg.ex[["t.delta"]] = 0.5617496
tg.ex[["t.gamma"]] = 0.06966376
tg.ex[["t.vega"]]  = 0.01335727 + 0.02321608 # TEV + TN30V ?
tg.ex[["t.theta"]] = -1.263507
tg.ex[["t.rho"]]   = 0.008725746

new.underly = 68.07

# pretty sure this function is wrong and worthless:
# calculate the *change* in option price given true greeks
# result should be added to original price to find expected price after 
# given dte
fTrueOptPrice = function(underly, new.underly, 
                         td, tg, tv, tt, tr=0,
                         estniv, aggiv,
                         days) {
  price.diff   = new.underly - underly
  price.effect = ((price.diff * tg) + td) * price.diff
  vol.diff     = (aggiv - estniv) * 100 # true vega based on whole %s
  vol.effect   = tv * vol.diff
  theta.effect = tt * days
  # ignoring rho for now
  op.diff      = price.effect + vol.effect + theta.effect
  op.diff
  
}

# fTrueOptPrice(underly, new.underly, 
#               tg.ex$t.delta, tg.ex$t.gamma, tg.ex$t.vega, tg.ex$t.theta, 
#               estniv = tg.ex$atmniv, aggiv = tg.ex$aggiv, 
#               days = 1) + 2.675
# [1] 9.972975
# way overstated for now? I think the sheet says 8.088
# TODO: set toOpt equal to sheet parameters and re-run my shit

# Thoughts
# Things that match:
#   True Earnings Vega
#   True Rho (cause it's BS Rho without an actual expected rate change)
#   
# BS Delta in the sheet is calculated with 7/252 days, CalcIV value, and other
# stuff from the TZero tab. So true delta should be calculated with the same.
# HOW DOES THE CALCIV VALUE GET THE PRICE CHANGE INTEGRATED IN?!
#   a) use ATM IV (since it would by definition be ATM for a price change?)
#       aggIV    =  0.5739678
#       +1.delta =  blackscholes(underly+1, 60, 0.0025, 7/252, 0.5739678, "call")-blackscholes(underly, 60, 0.0025, 7/252, 0.5739678, "call")
#                   0.5960701
#       expected =  0.59270
#       -1.delta = -0.5282899
#       expected = -0.51899
#   b) use ATM IV on new price but calcIV on old price? 0.5528924
#   c) use ATM IV on new price but estimated IV on old price? 0.5971447
#   d) use estimated IV on new price and calcIV on old price? 0.5518446
#       ^ this one makes the most intuitive sense but is way low
#   e) no change in IV only underlying (use CalcIV): 0.5950201
#   f) no change in IV only underlying (use CalcIV): 0.5950201
#
# Work backwards:
# book.price.p1 = 0.5927+2.675
# ImpliedBSVol("call", 0.5, underly+1, 60, 0.0025, 0, 7/252, book.price.p1)
# [1] 0.5842147
# So knowing agg IV and IEV we can compute normal:
# fNormIV(7, 1, 6, toOpt[11], 0.5842147)
# [1] 0.2906087
# check out the minus 1 though:
#  book.price.m1 = 2.675-.5190
#  ImpliedBSVol("call", 0.5, underly-1, 60, 0.0025, 0, 7/252, book.price.m1)
# [1] 0.5872025
#  fNormIV(7, 1, 6, toOpt[11], 0.5872025)
# [1] 0.2975512
# so IV rose more for the -1 dollar than it did the +1 dollar, which makes sense
# do you get a new skew? 
#   then put that in the new festniv? 
#     then put that in agg iv?
#  fOVVSkew(60, underly+1, 0, 7/252, uiMaxSkew)
# [1] -0.1608252
#  fSlopeParm(toOpt[7], toOpt[8], 7/252)
# [1] 0.02396781
#  fCurveParm(toOpt[9], toOpt[10], 7/252)
# [1] 0.2091023
 fEstNIV(fSlopeParm(toOpt[7], toOpt[8], 7/252),
          fOVVSkew(60, underly+1, 0, 7/252, uiMaxSkew),
          fCurveParm(toOpt[9], toOpt[10], 7/252),
          fATMNIV(toOpt[1], toOpt[2], toOpt[3], 7/252))
# [1] 0.2672747 # doesn't match ! see the first normal IV from start (0.2906087)