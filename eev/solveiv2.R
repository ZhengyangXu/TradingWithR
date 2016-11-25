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

library(bizdays)
library(GenSA)
library(rootSolve)

# User inputs:
uiMinIEV  = 0.011 # 1.1%
uiMinA    = 0.011
uiMaxParm = 6     # 600%
uiMaxSkew = 1
uiOptMult = 100
uiHolidays = c('2014-01-01', '2014-01-20', '2014-02-17',
             '2014-04-18', '2014-05-26', '2014-07-03',
             '2014-09-01', '2014-11-27', '2014-12-25',
             '2015-01-01', '2015-01-19', '2015-02-16',
             '2015-04-03', '2015-05-25', '2015-07-03',
             '2015-09-07', '2015-11-26', '2015-12-25',
             '2016-01-01', '2016-01-18', '2016-02-15',
             '2016-03-25')

# Optimizer parameters are only constants because they directly feed back into 
# formulas in Excel which have the objective function in it. So what's the obj
# function this time around? Is it just the output of the VWE Sq which then 
# calls other functions?
# Ac, Bc, Cc, Ap, Bp, Cp, VSA, VSB, VCA, VCB, IEV
# [0] [1] [2] [3] [4] [5] [6]  [7]  [8]  [9]  [10]
optLower = numeric(11) # TODO set defaults here
optUpper = numeric(11) # TODO set defaults here
toOpt    = numeric(11) # TODO randomize these between upper/lower defaults?

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
  return(A*B*(1-exp(-C*t)))
}

# Replicate that hard-coded multiplier
fIVNVMult = function(x) {
  return(1.7628*(x^2)-1.7138*(x)+0.9066)
}

# Limit max vertical skew
fOVVSkew = function(K, U, divadj, t, maxSkew) {
  #ln(Strike Price / (underlying price+dividend adjustment)) / sqrt(YrsToExp),
  #but he tries to get the max between this skew and the *negative* user input skew
  #***seems like he either wants a skew value, maximum skew, or 0 depending.***
  if (t > 0) 
    return (max(min(log(K / (U+divadj)) / (t^0.5), uiMaxSkew), -1*uiMaxSkew))
  else 
    return (0)
}

fNormIV = function(busDays, earnDays, normDays, IEV, calcIV) {
  # uses calculated IV which will end up calling NewtRaph / rootSolve function
  return((max(busDays*calcIV^2-earnDays*IEV^2, 0) / normDays)^(0.5))
}

fcalcIV = function(K, U, divadj, t, avgIV, midPrice, CorP) {
  # avgIV is average of all "Mid IV" from broker platform of whole matrix
  # guessing this should calculate the IV per BSOPM
  # maybe use rootSolve here
}

# Set up calendar
mycal = Calendar(holidays = uiHolidays, start.date = "2014-01-01", end.date="2015-12-31", weekdays=c("saturday", "sunday"))
# sample bizdays call: bizdays("2014-01-02", "2014-01-21", mycal) = 12
