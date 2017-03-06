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

library("quantmod")
library("RcppRoll") # no xts/zoo support?

# 1. Be able to read data from either yahoo/disk
# 2. Write function to take in the latest data and spit out scores
#     a. keep track of all scores all the time for audit
# 3. Write function to 'cycle'
#     a. if one of the new is same as one of the old, do nothing
#     b. otherwise, mark one as got-to-go, reinvest procedes of sale
#     c. if you have 3 positions, keep 1, do you split the procedes amongst
#        the two evenly? i think so. maybe add option of rebalancing every mo?
# 4. How to deal with dividends? Are they taken care of in back-data?

#symbols = c("SPY", "IWM", 
#            "EFA", "EEM", # EEM starts 2003-04-14
#            "TLT", "TLH", # TLT starts 2002-07-30, TLH 2007-01-11
#            "DBC", "GLD", # DBC starts 2006-02-06, GLD 2004-11-18
#            "ICF", "RWX") # RWX starts 2006-12-19

symbols = c("SPY", "EFA", "IEF", "GLD", "ICF", "DBC")
adjust_for_dividends = 6 # 6 is adjusted close, 4 is close

# Get some dataz
for (sym in symbols) {
  #getSymbols(sym, from="2002-01-01", to="2014-12-31") # need this data
  #getSymbols(sym, from="2007-02-01", to="2016-12-31")
  #getSymbols(sym, from="2016-01-01", to="2017-03-01")
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

getRanks = function(foo, A, B, C) {
  n  = length(foo) / 4 # how many symbols?
  r1 = rank(as.numeric(-foo[,(1+n):(2*n)]), ties.method='first')   # HIGHEST must be first
  r2 = rank(as.numeric(-foo[,(1+2*n):(3*n)]), ties.method='first') # HIGHEST must be first
  r3 = rank(as.numeric(foo[,(1+3*n):(4*n)]), ties.method='first')  # LOWEST must be first
  r4 = rbind(r1, r2, r3)
  return (colSums(r4*c(A, B, C)))
}

# takes in vector of ranks with length equal to # symbols
# output: bottom 3 ranking (that is, best) performing symbols to choose
pickThree = function(foo) {
  bar = sort(foo)[1:3]
  baz = c(symbols[foo == bar[1]], 
          symbols[foo == bar[2]], 
          symbols[foo == bar[3]])
  return (baz[1:3]) # forces first 3 entries if ties, e.g. (1st, 1st, 2nd)
                    # ties don't matter; you want 1 1 2 if possible
                    # if you chop 1 2 3 (3) no big deal
}

# example output for the start of December 2016
pickThree(getRanks(first(adjCl["2016-8"], '1 day'), 0.3, 0.4, 0.3))

for (y in 2016:2017) {
  for (m in 1:12) {
    if (y == 2017 && m > 2) {
      break
    } else {
      print(c(paste(y, m, sep='-'), pickThree(getRanks(last(adjCl[paste(y, m, sep='-')], '1 day'), 0.3, 0.4, 0.3))))
      # get the monthly returns for each symbol, store them in 1 of 3 columns
      # then you get to just add up the columns for total return since you're
      # using log returns. you _are_ using log returns, right?
    }
  }
}
