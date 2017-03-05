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

library(zoo)

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
d_mult = 1000           # d mode: dollars to multiply avg win/loss by
maxpct = 10             # max % loss on the losing trades
f_mult = maxpct / avg_l # use this to sync wlr w/ max loss % on a trade
ncurve = 100
ntrade = 100
d_or_f = 'd'            # dollars or fixed fractional risk mode
equity = 25000

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

plot(xrange, yrange, type="n", xlab="# trades", ylab="$ equity")
colors   = rainbow(ncurve)

for (i in 1:ncurve) {
  lines(1:(ntrade+1), curves[,i], col=colors[i])
}

lines(1:(ntrade+1), avg_crv, col="black", lwd=3)
cat("average return of all curves ", (tail(avg_crv,1) - equity)/equity*100, "%\n")
cat("expected value ", pwin*avg_w - (1-pwin)*avg_l, "\n")
cat("kelly value", (wlr*pwin-(1-pwin))/wlr, "%\n")
cat(" Min.   1Q   Median Mean  3Q    Max.\n",
    summary(as.vector(tail(curves, 1))))

# worst curve is:
#  curves[,(curves[101,] == min(curves[101,]))]
# best curve is:
#  first (if multiple) curves[,(curves[101,] == max(curves[101,]))][,1]
#  otherwise: curves[,(curves[101,] == max(curves[101,]))]
# plot best and worst curves
if (is.null(ncol(curves[,(curves[101,] == min(curves[101,]))]))) {
  lines(1:(ntrade+1), 
        curves[,(curves[101,] == min(curves[101,]))], 
        col="black", 
        lwd=2)
} else {
  lines(1:(ntrade+1), 
        curves[,(curves[101,] == min(curves[101,]))][,1], 
        col="black", 
        lwd=2)
}

if (is.null(ncol(curves[,(curves[101,] == max(curves[101,]))]))) {
  lines(1:(ntrade+1), 
        curves[,(curves[101,] == max(curves[101,]))], 
        col="black", 
        lwd=2)
} else {
  lines(1:(ntrade+1), 
        curves[,(curves[101,] == max(curves[101,]))][,1], 
        col="black", 
        lwd=2)
}