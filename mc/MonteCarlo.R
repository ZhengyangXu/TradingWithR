# Try to simulate some equity curves given P(win) and Avg Win size
# Inputs:
#   number of trades per curve
#   number of curves
#   probability of winning
#   profit factor -OR- (average win size + average loss size) -OR- W:L
#   whether wins/losses are in $ or %
#   dollar multiplier of W:L if W:L in $ 
#     (e.g. each trade wins $50 and loses $100 if W:L is 0.5 and mult is 100)
#   max loss per trade if W:L in % 
#     (e.g. each trade wins 5% and loses 10% if W:L is 0.5 max loss is 10)
#   starting equity
# Risk of ruin inputs:
#   Loss level % (be clear what this is: losing X% or having X% remaining)
# Outputs:
#   Basic statistical analysis of all curve results
#   Basic statistical analysis of drawdowns by time
#     answer the question: "how hard is this to stick to when times are bad?"
#   Basic statistical analysis of drawdowns by amount
#     answer the question: "how pissed would people be looking at results?"
#   Graph of random sample of X curves
# Risk of ruin outputs:
#   Risk of ever experiencing loss level % from any given point on curve
#   Risk of ever experiencing loss level % of starting capital

library('zoo')
library('xts')
library('PerformanceAnalytics')

pwin   = 0.9
wlr    = 0.4
#avg_w  = 2
#avg_l  = 5
if (wlr < 1) {
  avg_w = 1
  avg_l = (1/wlr)
} else {
  avg_w = wlr
  avg_l = 1
}
#wlr    = avg_w / avg_l
#profac = wlr * (pwin/(1-pwin))
d_mult = 3600           # d mode: dollars won per trade. loss = d_mult / wlr
maxpct = 4              # f mode: max % loss on the losing trades
f_mult = maxpct / avg_l # use this to sync wlr w/ max loss % on a trade
ncurve = 5000
ntrade = 90
d_or_f = 'd'            # dollars or fixed fractional risk mode
equity = 125000

probs  = runif(ncurve * ntrade)
probs  = matrix(probs, nrow=ntrade, ncol=ncurve)

trades = probs
if (d_or_f == "d") {
  trades[probs <= pwin] = avg_w * d_mult
  trades[probs > pwin]  = avg_l * d_mult * (-1)
} else {
  trades[probs <= pwin] = f_mult * avg_w / 100
  trades[probs > pwin]  = f_mult * avg_l / 100 * (-1)
}

curves = rep(equity, ncurve)
temp   = rbind(curves, trades)
#curves = rollapply(curves, 1, sum) # not this easy

# rollapply is unusably slow as used below
# brute force is easier than going to get RcppRoll and trying again
excel_roll_sum = function(x) {
  y = x
  for (i in 1:length(x)) { y[i] = rollapply(x, i, sum)[1]}
  return(y)
}

excel_roll_sum2 = function(x) {
  for (i in 1:(length(x)-1)) { 
    x[i+1] = x[i]+x[i+1]
  }
  return(x)
}

excel_roll_mult = function(x) {
  for (i in 1:(length(x)-1)) { 
    x[i+1] = x[i]*(1+x[i+1])
  }
  return(x)
}

if (d_or_f == 'd') {
  curves = apply(temp, 2, excel_roll_sum2)
} else 
  curves = apply(temp, 2, excel_roll_mult)

avg_crv = rowSums(curves)/ncurve

# plot them
xrange = c(0, ntrade)
yrange = range(curves)

plot(xrange, yrange, type="n", xlab="# trades", ylab="$ equity", 
     main=paste("pwin: ", pwin, " wlr: ", wlr, " curves: ", ncurve))
colors   = rainbow(ncurve)

for (i in 1:ncurve) {
  #lines(1:(ntrade+1), curves[,i], col=colors[i])
  lines(1:(ntrade+1), curves[,i], col="grey")
}

lines(1:(ntrade+1), avg_crv, col="black", lwd=3)
cat("average return of all curves ", (tail(avg_crv,1) - equity)/equity*100, "%\n")
cat("expected value ", pwin*avg_w - (1-pwin)*avg_l, "\n")
cat("kelly value", (wlr*pwin-(1-pwin))/wlr, "%\n")
print(paste(" Min.   1Q   Median Mean  3Q    Max."))
print(summary(as.vector(tail(curves, 1))))

# worst curve is:
#  curves[,(curves[101,] == min(curves[101,]))]
# best curve is:
#  first (if multiple) curves[,(curves[101,] == max(curves[101,]))][,1]
#  otherwise: curves[,(curves[101,] == max(curves[101,]))]
# plot best and worst curves
if (is.null(ncol(curves[,(curves[(ntrade+1),] == min(curves[(ntrade+1),]))]))) {
  lines(1:(ntrade+1), 
        curves[,(curves[(ntrade+1),] == min(curves[(ntrade+1),]))], 
        col="red", 
        lwd=2)
} else {
  lines(1:(ntrade+1), 
        curves[,(curves[(ntrade+1),] == min(curves[(ntrade+1),]))][,1], 
        col="red", 
        lwd=2)
}

if (is.null(ncol(curves[,(curves[(ntrade+1),] == max(curves[(ntrade+1),]))]))) {
  lines(1:(ntrade+1), 
        curves[,(curves[(ntrade+1),] == max(curves[(ntrade+1),]))], 
        col="dark green", 
        lwd=2)
} else {
  lines(1:(ntrade+1), 
        curves[,(curves[(ntrade+1),] == max(curves[(ntrade+1),]))][,1], 
        col="dark green", 
        lwd=2)
}
# maybe do the above in a for loop to see how best and worst curves at 
# trade (25%, 50%, 75%) ended up? use an array for colors like
# [magenta", "orange"   "blue", "yellow     "pink", "lime"      "green, "red"]
#  best/worst at 25%    best/worst at 50%   best/worst at 75%     end
# maybe even don't even graph the grey ones?

# curves with final equity < start equity:
sux = matrix(curves[,curves[ntrade,] < equity], nrow=ntrade+1)
print(paste("number of curves losing money over", ntrade, "trades out of",
            ncurve, "curves:", ncol(sux)), sep=" ")
print(paste(ncol(sux)/ncurve*100, "% of curves lost money", sep=" "))

# OISUF is ~ 300 profit / 600  loss per contract on RUT and SPX
#          ~ 600 profit / 1200 loss per contract on NDX
# Taking a signal within a day or two of each other all on the same
#   expiration: $2400 at risk per contract (3 instruments)
# Taking a signal within a day or two of each other all on the same
#   expiration with 2x RUT/SPX size: $3600 at risk per event
# pct of account   2%      4%     6% 
# size  of (1)     120k    60k    40k !! Run sim 2.4k/40k/10k/18 look at DD !!
# size  of (2)     180k    90k    60k

# Every time you're putting that $X at risk, you're expecting ($X * 0.65)
#   so even betting on 3 at once with 1 contract, you're expecting to make $
#   the hard part is consecutive losses. but even these odds with 6% losses
#   the average still weighs heavily in your favor. only ~1% lose money over
#   20 trades. 0% lose money over 40 trades.

# So on the optimistic side, how many times were there 3 trades on in same
#   expiration month?
# And on the pessimist side, how many times were there 6 or 9 trades on
#   in total? What would a massive slippage event looked like in that regard?
#   or worse, a flash crash triggering limits then recovery?

# Timelines: if you average 0.75 trades/month/instrument, that's only 27/year
#   on plan (1) that's 9*2400*0.65 = $14k profit
#   on plan (2) that's 9*3600*0.65 = $21k profit
# Note: you have to do 9*dollars cause that's what's different potentially.
#   you don't have the potential to trade 27 times risking 2400/ea, you are
#   trading 9 times risking 2400 or 3600 each.

# How many curves have a 25% drawdown from start equity?
# How many curves have a 25% drawdown ever?
elementOr = function(x) { 
  if (length(x) == 1) return(x) 
  else return(x[1] | elementOr(x[2:length(x)]))
}

# curves with % dd from equity:
pctdd    = 25
valdd    = (1-(pctdd/100))*equity
ddcurves = length(apply(curves < valdd, 2, elementOr)[apply(curves < valdd, 2, elementOr) == TRUE])
print(paste(ddcurves, "of", ncurve, "curves (", ddcurves/ncurve*100, "%) fell below", pctdd, "% starting equity"))

# use maxDrawdown() to find if drawdown was worse than X%
xcurves  = xts(curves, order.by=(Sys.Date()-nrow(curves)+(1:nrow(curves))))
curveret = apply(xcurves, 2, Delt) # gotta pass it Delt() before you can 
                                   # use maxDrawdown and re-make xts
xcurveret = xts(curveret, order.by=(Sys.Date()-nrow(curves)+(1:nrow(curves))))
myDDs     = apply(xcurveret, 2, maxDrawdown)
ddanytime = length(myDDs[myDDs > 0.15])
print(paste(ddanytime, "of", ncurve, "curves (", ddanytime/ncurve*100, 
            "%) experienced", pctdd, "% drawdown (ever)"))
print(paste("Worst drawdown (in blue): ", 
            signif(max(myDDs)*100, 3),
            "%"))

worstcurve = seq(1:ncurve)[myDDs == max(myDDs)]
lines(1:(ntrade+1), 
      curves[,worstcurve], 
      col="blue", 
      lwd=2)
print("Drawdown summary:")
print(paste(" Min.   1Q   Median Mean  3Q    Max."))
print(summary(myDDs))
# for calculation of risk of DD, use the idea of running out of money, just
# make the starting amount of money equal to the % of bankroll you're looking
# for DD statistics on. so 25% dd on 100k is losing all of 25k



