require(quantmod)
require(PerformanceAnalytics)

# Step 1: Get the data
getSymbols("SPY")

# Step 2: Create your indicator
#dvi <- DVI(Cl(GSPC))
sma <- SMA(Ad(SPY), n=200)

# Step 3: Construct your trading rule
#sig <- Lag(ifelse(dvi[,1] < 0.5, 1, -1))
sig <- Lag(ifelse(Ad(SPY) > sma, 1, 0))

# Step 4: The trading rules/equity curve
ret <- ROC(Ad(SPY))*sig
ret <- ret['2009-01-01/2017-04-01']
eq <- exp(cumsum(ret))
plot(eq)

# Step 5: Evaluate strategy performance
table.Drawdowns(ret, top=10)
table.DownsideRisk(ret)
charts.PerformanceSummary(ret)

# Step 6: The benchmark
ret <- ROC(Ad(SPY))
ret <- ret['2009-01-01/2017-04-01']
charts.PerformanceSummary(ret)
