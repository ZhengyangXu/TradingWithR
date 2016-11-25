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
#                     BUT the aggregate formula refers back to the currently-being-optimized IEV value
#    N 'NIVErr SQ'  = (NIVErr)^2
#    O 'VW Err'     = NIVErr*Vega*(Crazy NV/IV Multiplier with hard coded constants?)
#    P 'VWE SQ'     = (VWErr)^2
#    Q 'UVAL'       = NIVErr*(-1*Vega)*(Crazy NV/IV Multiplier?)*100/(user-input option multiplier)
#                     Find under/overpriced options on a per share basis
#                     If it's 100 shares per contract then UVAL = -1*VW Err

# TODO figure out:
#    What is that crazy multiplier thing? (is it in the book? what page?)
#       Maybe explained in ch 7 with the vertical / horizontal skew formulas. sort of.
#    How does the maximum skew fit in? (is it in the book? what page?)
#    DONE Why does the VW Err value match the UVAL even when input params are random?
#       Because the only thing UVAL is doing is flipping the sign and adjusting for # options/contract
#    DONE Use 'bizdays' package for business day checks
#    So you have the model's NIV through the ATMNIV + vert skew, then you compare that to
#    the NIV derived through the aggregate formula? and figure out the IEV that results in the minimum
#    error across the matrix? But where in the aggregate formula is the strike price? 

library(bizdays)
library(GenSA)

# User inputs:
uiMinIEV  = 0.011 # 1.1%
uiMinA    = 0.011
uiMaxParm = 6     # 600%
uiMaxSkew = 1
uiOptMult = 100

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
