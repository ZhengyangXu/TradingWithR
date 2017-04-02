# Replicate http://www.the-lazy-trader.com/2015/01/ETF-Rotation-Systems-to-beat-the-Market-SPY-IWM-EEM-EFA-TLT-TLH-DBC-GLD-ICF-RWX.html
# Add:
#  1. transaction fees
#  2. short term capital gains tax
#  3. make it work with variable number of instruments

#How are the three best ETFs selected every month?
#There are three ingredients involved in the formula:
  
#- The 3 months return (40% weight in the final score)
#- The 20 day return (30% weight in the final score)
#- The 20 days volatility (30% weight in the final score). 

# Volatility is the annualized standard deviation of daily returns. 
# So, the 20-day Volatility is the standard deviation of the past 20 
# 1-day returns multiplied by sqrt(252) (annualized). The idea is to 
# penalize the instruments that are having large variations in their 
# daily returns. Those that are quiet and consistent are favored.

# !!!NB!!! Author's spreadsheet uses k=19 not 20 for 20 day return
#          and doesn't use the adjusted close price
#          and uses k=65 days for 3 month performance
#          and uses stdevp not stdev
#          ETFreplay itself says 6mo is 126 trading days and 3mo is 63

#  TODO   ETFreplay does a tiebreak based on:
#         In the event of a tie, where two or more ETFs have the same 
#         weighted rank, then the factor that has been assigned the 
#         largest weighting determines the order.


# !!!NB!!! TODO this code does not yet factor dividends correctly
#          and I don't think adjusted data from Yahoo is good enough?

library("quantmod")
library("RcppRoll") # no xts/zoo support?
library("PerformanceAnalytics")
e.a.e <- Vectorize(function(x, y) {isTRUE(all.equal(x, y))})

# 1. Be able to read data from either yahoo/disk
# 2. Write function to take in the latest data and spit out scores
#     a. keep track of all scores all the time for audit
# 3. Write function to 'cycle'
#     a. if one of the new is same as one of the old, do nothing
#     b. otherwise, mark one as got-to-go, reinvest procedes of sale
#     c. if you have 3 positions, keep 1, do you split the procedes amongst
#        the two evenly? i think so. maybe add option of rebalancing every mo?
# 4. How to deal with dividends? Are they taken care of in back-data?

symbols = c("SPY", "IWM", # 4 div/yr.
            "EFA", "EEM", # 2 div/yr.  EEM starts 2003-04-14
            "TLT", "TLH", # 12 div/yr. TLT starts 2002-07-30, TLH 2007-01-11
            "DBC", "GLD", # 0 div/yr.  DBC starts 2006-02-06, GLD 2004-11-18
            "ICF", "RWX") # 4 div/yr.  RWX starts 2006-12-19

#symbols = c("SPY", "EFA", "IEF", "GLD", "ICF")
adjust_for_dividends = 6 # 6 is adjusted close, 4 is close. 6 matches ETFreplay
initBal   = 60000
nFunds    = 3 # can't change to 2 yet with this code
data.start.year = 2007
data.start.mon  = 1
data.end.year   = 2017
data.end.mon    = 1
data.start.time = paste(data.start.year, data.start.mon, 1, sep='-')
data.end.time   = paste(data.end.year, data.end.mon, 1, sep='-')

analysis.start.year  = 2007
analysis.start.mon   = 12

# Get some dataz
for (sym in symbols) {
  #getSymbols(sym, from="2002-01-01", to="2014-12-31") # need this data
  #getSymbols(sym, from="2007-02-01", to="2016-12-31")
  getSymbols(sym, from=data.start.time, to=data.end.time)
  if (sym == symbols[1]) {
    adjCl = get(sym)[,adjust_for_dividends]
  } else {
    adjCl = cbind(adjCl, get(sym)[,adjust_for_dividends]) 
  }
}

# Compute the metrics (do it for all time, who cares?)
for (i in 1:ncol(adjCl)) {
  if (i == 1) {
    x =         Delt(adjCl[,i], k=20, type='arithmetic')
    y =         Delt(adjCl[,i], k=63, type='arithmetic')
    z = roll_sd(Delt(adjCl[,i], k=1,  type='arithmetic'), # daily returns
                20,                                       # lookback is 20
                fill=0,
                align='right') * (252)^0.5
  } else {
    x = cbind(x,         Delt(adjCl[,i], k=20, type='arithmetic'))
    y = cbind(y,         Delt(adjCl[,i], k=63, type='arithmetic'))
    z = cbind(z, roll_sd(Delt(adjCl[,i], k=1,  type='arithmetic'), # daily!
                         20,
                         fill=0,
                         align='right') * (252)^0.5
                 )
  }
}

adjCl = cbind(adjCl, x, y, z)

colnames(adjCl) = c(symbols,
                    paste(symbols, "20 day"),
                    paste(symbols, "63 day"),
                    paste(symbols, "20 vol"))

# adjCl is now 1:10 prices, 11:20 20 day, 21:30 60 day, 31:40 20 vol
# so Pth symbol out of N symbols' 20 day return is N+P
# so Pth symbol out of N symbols' 63 day return is 2*N+P
# so Pth symbol out of N symbols' 20 day vol is 3*N+P

# for ranking, make a new data structure with dates of adjCl
# then 'apply' a function row-wise to the adjCl object to get the output.

# inputs
#   foo = data from adjCl for last day of month
#   A = weight of 20 day returns
#   B = weight of 63 day returns
#   C = weight of 20 day volatility
getRanks = function(foo, A, B, C) {
  n  = length(foo) / 4 # how many symbols?
  # HIGHEST must be 1 (first place) so note the negative foo (-foo[...])
  r1 = rank(as.numeric(-foo[,(1+n):(2*n)]), ties.method='first')   
  # HIGHEST must be 1 (first place) so note the negative foo (-foo[...])
  r2 = rank(as.numeric(-foo[,(1+2*n):(3*n)]), ties.method='first')
  # LOWEST must be 1
  r3 = rank(as.numeric(foo[,(1+3*n):(4*n)]), ties.method='first')  
  r4 = rbind(r1, r2, r3)
  result = colSums(r4*c(A, B, C))
  
  bar = sort(result)[1:4]
  # [A, B, C, D, ...] return A B C
  # [A, A, B, C, ...] return A A B
  # [A, A, B, B, ...] return A A (max weight B)
  # [A, A, A, B, ...] return A A A
  # [A, A, A, A, ...] return (max weight A) first 3 not (max weight A)
  # [A, B, B, C, ...] return A B B
  # [A, B, B, B, ...] return A (max weight B) (second max weight B)
  # [A, B, C, C, ...] return A B (max weight C)
  
  if (length(symbols[result == bar[1]]) == 2) # [A, A, B, C] or [A, A, B, B]
    if (length(symbols[result == bar[3]]) == 2) # TODO [A, B, C, C, C, ...]
      baz = c(symbols[result == bar[1]],
              symbols[min(r2[symbols[result == bar[3]][1] == symbols], 
                          r2[symbols[result == bar[3]][2] == symbols]) == r2])
              # tie break on only matching symbols
    else
      baz = c(symbols[result == bar[1]], # returns 2 symbols
            symbols[result == bar[3]])
  # this case doesn't consider sort(foo) == [A, A, B, B, ...]
  else if (length(symbols[result == bar[1]]) == 3)
    baz = symbols[result == bar[1]]
    # this doesn't solve case of [A, A, A, A, ...]
  else
    if (length(symbols[result == bar[3]]) == 2)  # TODO [A, B, C, C, C, ...]
      baz = c(symbols[result == bar[1]],
              symbols[result == bar[2]],
              symbols[min(r2[symbols[result == bar[3]][1] == symbols], 
                          r2[symbols[result == bar[3]][2] == symbols]) == r2])
    else
      baz = c(symbols[result == bar[1]], 
              symbols[result == bar[2]], 
              symbols[result == bar[3]])
  return (baz[1:3])
}

# takes in vector of ranks with length equal to # symbols
# output: lowest 3 ranking (that is, best) performing symbols to choose
# (think of rank 1 as first place)
# To truly match ETFreplay, need: In the event of a tie, where two or more 
#   ETFs have the same weighted rank, then the factor that has been assigned 
#   the largest weighting determines the order.
# --- DEPRECATED --- use getRanks instead now
#pickThree = function(foo) {
#  bar = sort(foo)[1:3]
#  if (length(symbols[foo == bar[1]]) == 2)
#    baz = c(symbols[foo == bar[1]], # returns 2 symbols
#            symbols[foo == bar[3]])
#    # this case doesn't consider sort(foo) == [A, A, B, B, ...]
#  else if (length(symbols[foo == bar[1]]) == 3)
#    baz = symbols[foo == bar[1]]
#  else
#    baz = c(symbols[foo == bar[1]], 
#            symbols[foo == bar[2]], 
#            symbols[foo == bar[3]])
#  return (baz[1:3])
#}

# example output for the start of December 2016
#pickThree(getRanks(last(adjCl["2016-7"], '1 day'), 0.3, 0.4, 0.3))





# matrix of daily returns
returns     = apply(adjCl[,1:length(symbols)], 2, Delt, type='arithmetic')
returns     = xts(returns, order.by=index(adjCl))
returns[1,] = 0 # cause Delt() made row 1 == NA

# initialize balances object. contains running balances by fund.
# for final equity curve, just rowSums()
balances = xts(
            matrix(rep(0, length(symbols)*nrow(adjCl)), 
                   nrow=nrow(adjCl), 
                   ncol=length(symbols)), 
            order.by=index(adjCl)
           )
colnames(balances) = symbols

### try to do everything in for loop
#### get first recommendations (y will contain what to invest in for 2016-01)
###y = pickThree(getRanks(last(adjCl["2015-12"], '1 day'), 0.3, 0.4, 0.3))
#### set initial balances for funds (only 3 future funds will use this)
###balances[index(last(balances["2015-12"], '1 day')),] = (initBal/nFunds)
#### first, put returns into balances, then do cumprod()
###for (i in 1:length(y)) {
###  balances["2016-1",which(symbols == y[i])] = 
###                    1+returns["2016-1", which(symbols == y[i])]
###}
#### get the actual balances for the month
###balances["2016-1",] = cumprod(rbind(last(balances["2015-12"], '1 day'), 
###                              balances["2016-1"]))["2016-1",]

# now you have the returns for 2016-1, and you repeat the loop since you 
# can reference the previous returns. but now that you're repeating the loop
# you have to do error checking / see if you're already invested in something

###x = pickThree(getRanks(last(adjCl["2016-1"], '1 day'), 0.3, 0.4, 0.3))
###a = x[(x %in% y) == TRUE]  # funds in y (and x) that will stay invested
###b = y[(y %in% x) == FALSE] # funds in y that will swap out
###c = x[(x %in% y) == FALSE] # funds in x that will be new

# build the list of picks
# l$"2016-1" is what to invest in at the end of the month for 2016-2
l = list()
r = list()
tmp = paste(analysis.start.year, 12, sep='-')
l[[tmp]] = getRanks(last(adjCl[paste(tmp)], '1 day'), 0.3, 0.4, 0.3)
#r[[tmp]] = getRanks(last(adjCl[paste(tmp)], '1 day'), 0.3, 0.4, 0.3)
for (y in (analysis.start.year+1):(data.end.year-1)) {
  for (m in 1:12) {
    if (y == 2017 && m > 3) {
      break
    } else {
        tmp = paste(y, m, sep='-')
        l[[tmp]] = getRanks(last(adjCl[tmp], '1 day'), 0.3, 0.4, 0.3)
        #r[[tmp]] = getRanks(last(adjCl[tmp], '1 day'), 0.3, 0.4, 0.3)
    }
  }
}
old = names(l)[1]
new = names(l)[2]
for (sym in l[[old]]) {
  balances[index(last(balances[paste(old)], '1 day')),which(symbols == sym)] = 
    (initBal/nFunds)
}

for (i in 2:length(l)) {
  # calculate balances into next month
  # calculate balances for your new month, then get new month names
  # DON'T CONFUSE new AND l[[new]] !!!!
  for (sym in l[[old]]) 
    balances[paste(new),which(symbols == sym)] = 
      1+returns[paste(new),which(symbols == sym)]
  
  balances[paste(new),] = cumprod(rbind(last(balances[paste(old)], '1 day'), 
                                        balances[paste(new)]))[paste(new),]
  # funds in old (and new) that will stay invested
  a = l[[old]][(l[[old]] %in% l[[new]]) == TRUE]  
  # funds in old that will swap out
  b = l[[old]][(l[[old]] %in% l[[new]]) == FALSE] 
  # funds in new that will be new
  c = l[[new]][(l[[new]] %in% l[[old]]) == FALSE] 
  ############### should all the 'news' below be 'olds' if prework is done?
  ############### and not include 2015 in list?
  # crux is that you made your decisions for january returns on 12/31
  #   then you filled out returns from january
  #   then calculated what 'old' and 'new' are -- REMEMBER l[["2016-1"]]
  #   is what to invest in for 2016-2
  if (length(a) == 3) {
    # do nothing. old balances are new balances.
  } else if (length(a) == 2) {
    # only one fund to swap
    # on last day of the current (?) month
    # set new fund (c) balance to (old fund (b) * last return day of (b))
    balances[paste(new),][nrow(balances[paste(new)]),which(symbols == c)] =
      balances[paste(new),][nrow(balances[paste(new)]),which(symbols == b)]
    # set old fund balance (b) to 0
    balances[paste(new),][nrow(balances[paste(new)]),which(symbols == b)] = 0
  } else if ((length(a) == 1) || (length(a) == 0)) {
    # two funds to swap
    # for each old fund, get previous balance, add to sum, set to 0
    tempSum = 0
    for (sym in b) {
      tempSum = tempSum + as.numeric(
                            last(balances[paste(new),which(symbols == sym)], 
                                 '1 day'))
      balances[paste(new),][nrow(balances[paste(new)]),which(symbols == sym)] = 0
    }
    # for each new fund, set new balance
    for (sym in c) {
      balances[paste(new),][nrow(balances[paste(new)]),which(symbols == sym)] =
        tempSum / length(c)
    }
  } else {
    print('broken')
    break
  }
  old = names(l)[i] # pick better names here. invested?
  if (i == length(l)) {
    break
  } else {
    new = names(l)[i+1]   # toInvest?
  }
}

equity = cbind(balances,rowSums(balances))[,length(symbols)+1][paste(analysis.start.year+1, 2017, sep="::"),]
bench  = cumprod(
          rbind(initBal * first(1+returns$SPY[paste(analysis.start.year+1),], '1 day'), 
                first(1+returns$SPY[paste(analysis.start.year+1, 2017, sep="::"),], '-1 day')))
#plot(last(bench, '-1 day'), ylim=range(equity, bench))
#lines(last(equity, '-1 day'), col='red')
plot(last(equity, '-1 day'), ylim=range(equity, bench), 
     main="Rotation v. Benchmark")
lines(last(bench, '-1 day'), col='red')
legend('topleft', 
       legend=c("rotate", "bench"), 
       col=c('black', 'red'),
       lty=1,
       lwd=2)

# redo everything with log returns so you can use PerformanceSummary
# graphs in quantmod?
print(Delt(to.yearly(equity)[,4]))
# something very wrong with backtest code
# returns don't match LT's at all some years
# TODO: confirm a test on 1 year of returns by hand in excel

# get some stuff going reading from CSVs instead of Yahooooooo-oo-oooo!
setwd("~/Documents/TradingWithR/rotation")
spyb = read.csv("data/SPY.CSV", header=TRUE)
#as.Date(spyb[,1], format="%m/%d/%y")
zabba = xts(spyb[,2:5], as.Date(spyb[,1], format="%m/%d/%y"))
# no no no use getSymbols with src="csv"!
# But wait....
# "If you want to use getSymbols.csv, 
# your data has must have the Date and 6 columns (OHLCVA)"

# How would you verify a data set?
#   any prices < 10% of starting price (does daily diffs check this?)
#   any prices > 300% of starting price (does daily diffs check this?)
#   any NAs
#   any 1 day returns -5% < x > 5% (know these facts per symbol?)
#   max string of positive / negative days > X (known per symbol?)
#   any non-numbers
#   how to check for systemic over/under valuation? compare two data sets
#     and see if the sum of the positive diffs > X 
#     and sum of negative diffs < Y?

# graph the optionvue data vs the yahoo data
##for (sym in c("SPY", "IWM", "EFA")) {
##  for (y in 2010:2016) {
##    spyb = read.csv(paste("data/", sym, ".CSV", sep=""), header=TRUE)
##    zabba = xts(spyb[,2:5], as.Date(spyb[,1], format="%m/%d/%y"))
##    charts.PerformanceSummary(Delt(zabba$CLOSE[paste(y)]), 
##                              main=paste(sym,"-OV-",y))
##    charts.PerformanceSummary(Delt(Cl(get(sym)[paste(y)])), 
##                              main=paste(sym,"-Y-",y))
##  }
##}
# suspects from graphs with more than a few differences
#   ICF 2015
#   TLH 2015
#   TLH 2014
#   TLH 2010
#   IWM 2016





