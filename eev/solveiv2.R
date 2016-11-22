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
#    H 'NormIV'     = Aggregate formula NormIV
#                     BUT the aggregate formula refers back to the currently-being-optimized IEV value
#    I 'ATMNIV'     = plug and chug of normalized IV with A, B, C parameters and exp() function
#    J 'SlopeParm'  = constant, VslopeA+VslopeB*t, where t=YrsToExp
#    K 'CurveParm'  = constant, VcurveA+VcurveB*t, where t=YrsToExp
#    L 'Est NormIV' = SlopeParm*OVVSkew+CurveParm*(OVVSkew^2)+ATMNIV
#    M 'NIV Error'  = Aggregate formula NormIV - EstNormIV 
#                     BUT the aggregate formula refers back to the currently-being-optimized IEV value
#    N 'NIVErr SQ'  = (NIVErr)^2
#    O 'VW Err'     = NIVErr*Vega*(Crazy NV/IV Multiplier with hard coded constants?)
#    P 'VWE SQ'     = (VWErr)^2
#    Q 'UVAL'       = (Crazy NV/IV Multiplier?)*100*NIVErr*(-1*Vega)/(user-input option multiplier)
#                     big kahuna function to find under/overpriced options on a per share basis