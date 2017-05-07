# Options Backtester
# v0.2 requirements:
#    0. make it faster. Data load for 7 years is ~30 seconds. Backtest 1TPS
#       in same amount of time. Where is the bottleneck? Clearly the copy.
#       Turn O(n) for trade updates to O(1) and / or don't copy anything.
#    1. Make an open.trades object that you update every day. When it's closed,
#       move it to closed.trades. Floating profit is sum of open trades / day.
#       Closed profit is sum of closed trades on that day.

# use a zoo object for time, and first column is the list of quotes and
# second column is list of stats??

# Use EAE() instead of == on floating point numbers
EAE <- Vectorize(function(x, y) {isTRUE(all.equal(x, y))})

# Setup date stuff
library('bizdays')
library('quantmod')
cal.begin       = "2010-01-01"
cal.end         = "2019-01-01"
my.holidays     = c('2010-01-01', '2010-01-18', '2010-02-15',
                    '2010-04-02', '2010-05-31', '2010-07-05',
                    '2010-09-06', '2010-11-25', '2010-12-24',
                    '2011-01-01', '2011-01-17', '2011-02-21',
                    '2011-04-22', '2011-05-30', '2011-07-04',
                    '2011-09-05', '2011-11-24', '2011-12-26',
                    '2012-01-02', '2012-01-16', '2012-02-20',
                    '2012-04-06', '2012-05-28', '2012-07-04',
                    '2012-09-03', '2012-11-22', '2012-12-25',
                    '2013-01-01', '2013-01-21', '2013-02-18',
                    '2013-03-29', '2013-05-27', '2013-07-04',
                    '2013-09-02', '2013-11-28', '2013-12-25',
                    '2014-01-01', '2014-01-20', '2014-02-17',
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
# sample bizdays call: bizdays("2014-01-02", "2014-01-21", mycal) = 12
# Set up calendar
my.cal = create.calendar(holidays = my.holidays, 
                         start.date = cal.begin, 
                         end.date=cal.end, 
                         weekdays=c("saturday", "sunday"),
                         name="my.cal")
setwd("~/Documents/TradingWithR/backtester/data")
getSymbols("^RUT", from=cal.begin)
#oisuf.raw    = read.csv("../oisuf-rut-2014-2016.csv")
oisuf.raw    = read.csv("../oisuf-rut-all.csv") # 2004-2017
oisuf.values = as.xts(oisuf.raw[,2], order.by=as.Date(oisuf.raw[,1]))
kOisufThresh = 20


# Choose 1TPX or 1TPS
global.mode = "1TPX"

# PickByDelta: return an index of x that has the closest value to y. if there
#              is a tie, return the first one you come to. This function
#              may be erroneous due to floating point comparison. Check
#              with sprintf("%.54f", x) later on.
# inputs:
#   my.deltas, a list of deltas to compare against
#   my.value,  a value to compare to the list of deltas
# output:
#   the index (position) of the closest delta to your given value
# example:
#   my.deltas = c(0.005, 0.076, 0.090, 0.110, 0.121)
#   my.value  = 0.11
#   pick.by.delta(my.deltas, my.value) = 4
#   my.value  = 0.08
#   pick.by.delta(my.deltas, my.value) = 2
PickByDelta = function(my.deltas, my.value) {
  which.min(abs(my.value - my.deltas))
}

# OptionQuotesCsv: return a data frame that consists of the exported OptionVue
#                  quote data
# inputs:
#   ticker, the underlying symbol
#   datecode, the date of the export in format YYYYMMDD
#   timecode, the time of the export in format HHMM (24 hour time)
# output:
#   the data that was in the OptionVue CSV export as a data frame
# example:
#   data in a file called SPX201703201530.csv
#   my.df = get.quotes.csv("SPX", 20170320, 1530)
OptionQuotesCsv = function(options.file) {
  my.data       = read.csv(options.file)
  return(my.data[-1,]) # first row is underlying. use price.quotes for that
}

# EnrichOptionsQuotes: return the option quotes df with some extra info
#                      that makes it easier for the backtester to work with
# inputs:
#   my.df, the original quote data frame
#   ASSUMES SOME STUFF ABOUT PREVIOUSLY SET UP CALENDARS
# output:
#  the data frame plus the following new fields: 
#   my.exp.date, the Exp.Date field as a proper YYYY-MM-DD date format
#   biz.dte, business days until expiry
#   cal.dte, calendar days until expiry
#   mid.price, the average of the quoted bid and ask (NAs?)
EnrichOptionsQuotes = function(my.df) {
  my.df       = my.df[!is.na(my.df$Date),]
  my.df       = my.df[!is.na(my.df$Bid),]
  my.df       = my.df[!is.na(my.df$Asked),]
  my.exp.date = as.Date(as.character(my.df$Exp.Date), "%y%m%d")
  my.iso.date = as.Date(as.character(my.df$Date), "%y%m%d")
  biz.dte     = bizdays(my.iso.date,
                        my.exp.date,
                        my.cal)
  cal.dte     = as.numeric(my.exp.date - my.iso.date)
  mid.price   = (my.df$Bid + my.df$Asked) / 2
  return(cbind(my.df, my.iso.date, my.exp.date, biz.dte, cal.dte, mid.price))
}

# FindCondor: find the strikes of iron condor legs by delta: short 11 long 8
# inputs:
#   my.df, a data frame consisting of a Delta column and Existing.Posn. column
#   is.list, a boolean of whether or not the input is actually a list
# outputs:
#   a data frame consisting of only the trades to take to establish the condor
#   if there is more than one available expiration, take the min for now
#   TODO: proper multi-expiration handling
# example:
#   call FindCondor with lapply: foo = lapply(my.data, FindCondor, TRUE)
#   call FindCondor with normal data frame: foo = FindCondor(my.df)
FindCondor = function(my.df, is.list = FALSE) {
  if (is.list) {
    my.df = my.df[[1]]
  }
  # clean up to only include traditional monthlies
  # (should be checking this in data export as well)
  my.df = my.df[!grepl("D\\d{1,2}$", my.df$Symbol, perl = TRUE),]
  # clean up to only include possible candidates
  my.df = subset(my.df, cal.dte > 49 & cal.dte < 76)
  # if you have no expirations (a few days like this), return NULL
  if (nrow(my.df) == 0) {
    return(NULL)
  } else {
    # if you have two expirations, pick the minimum for now
    my.df = subset(my.df, cal.dte == min(cal.dte))
    my.df[PickByDelta(my.df$Delta,   8),]$Existing.Posn. =  1
    my.df[PickByDelta(my.df$Delta,  11),]$Existing.Posn. = -1
    my.df[PickByDelta(my.df$Delta,  -8),]$Existing.Posn. =  1
    my.df[PickByDelta(my.df$Delta, -11),]$Existing.Posn. = -1
    # Existing.Posn is NA by default, so this works but is not intuitive:
    my.open.trades = my.df[!is.na(my.df$Existing.Posn.),]
    # add new $orig.price column for later
    my.open.trades$orig.price = my.open.trades$mid.price
    return(my.open.trades)
  }
}

# redo data load with many files. filename format is mandatory and 
# only works for 3 letter symbols (e.g. SYM): SYMYYYYMMDDHHMM.csv
file.names     = list.files(pattern="*.csv")
my.data        = rep(list(), length(file.names))

for (i in 1:length(file.names)) {
  my.data[[i]] = EnrichOptionsQuotes(
                        OptionQuotesCsv(file.names[i])
                      )
}

names(my.data) = as.Date(substr(file.names, 4, 11), "%Y%m%d")

# find floating profit given df of open trades w/ column orig.price
FloatingProfit = function(trades) {
  floating.profit = (trades$mid.price - trades$orig.price) * 
    trades$Existing.Posn.
  floating.profit = sum(floating.profit)
}

# find initial credit given df of open trades w/ column orig.price
InitialCredit = function(trades) {
  net.credit = sum(trades$Existing.Posn. * trades$orig.price)
}

# return the strike price of the short call in a single condor
ShortCall = function(trades) {
  strike = min(trades[trades$Call.Put == "C",6])
}

# return the strike price of the short put in a single condor
ShortPut = function(trades) {
  strike = max(trades[trades$Call.Put == "P",6])
}

# see if exit conditions are met for a given condor, xts underlying, char date
ShouldExit = function(my.df, my.highs, my.lows, my.date) {
  if (is.null(my.cal) || !exists("my.cal")) # should check dates too
    stop("global calendar (my.cal) does not exist or is NULL")
  if (FloatingProfit(my.df) >= 0.89 * (-1 * InitialCredit(my.df))) 
    return(TRUE)
  else if (FloatingProfit(my.df) <= 2 * InitialCredit(my.df)) # negative val
    return(TRUE)
  else if (bizdays(as.Date(my.date), as.Date(my.df[1,24]), my.cal) <= 5) 
    return(TRUE)
  else if (as.numeric(my.highs[my.date]) >= ShortCall(my.df)) 
    return(TRUE)
  else if (as.numeric(my.lows[my.date]) <= ShortPut(my.df))
    return(TRUE)
  else 
    return(FALSE)
}

# see if entry conditions are met for a given day's list 
# idea here will be to check if you have an open trade on that quote day
# for the given condor you'd get out of the quote from that day
# Don't open a trade in more than one expiration at a time
# Don't open a trade outside of given trade window (min/max days)
#   (that's handled by FindCondor function)
ShouldEnter = function(my.trade.list, my.quote) {
  my.open.trades = do.call('rbind', my.trade.list)
  if (is.null(my.open.trades)) {
    return(TRUE)
  } else {
    open.exps = unique(my.open.trades$my.exp.date)
  }
  would.trade.exp = unique(FindCondor(my.quote)$my.exp.date)
  # check for null before running %in%
  if (is.null(would.trade.exp)) {
    return(FALSE)
  } else if (would.trade.exp %in% open.exps && global.mode == "1TPX") {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# output a short trade summary data frame given:
# a trade you're about to close
# the current date as.Date
TradeSummary = function(my.df, my.date) {
  trade.data = list(open.date     = my.df[1,]$my.iso.date,
                    exp.month     = substr(my.df[1,]$Description, 1, 3),
                    strikes       = toString(my.df$Strike.Price),
                    init.cred     = InitialCredit(my.df),
                    float.profit  = FloatingProfit(my.df),
                    cal.days.open = as.numeric(my.date - my.df[1,]$my.iso.date),
                    close.date    = my.date)
  return(as.data.frame(trade.data))
}

# Other trade keeping method:
#   1.  If there's an open trade, copy the current day's quote into the open 
#       positions object
#   2.  Final profit is max(dates open).mid.price - min(dates open).mid.price
#   3.  Can graph each position's P/L in the end, and all positions floating
#       is just the running sum of balance + open positions P/L
#   4.  Need to add the final P/L to total equity somehow otherwise you never
#       book profit.


# Every day:
#   1.  Update current trade prices to today's quotes
#   2.  Decide if we should exit them
#   3a. If no exit, do nothing
#   3b. If exit, delete the open position. record P/L somehow
#   4.  Decide if we should make a new trade
#   5.  If we trade, add to today's trade list
#   6.  Copy all open trades to tomorrow
#   7.  Log portfolio stats to today's portfolio stat object


# Create a stats object for every day, then rbind them all together later
# after you've completed the backtest to graph $ and stuff
stats = c("Delta", "Gamma",      "Theta",    "Vega",     "Rho", 
                   "Closed P/L", "Open P/L", "Days P/L", "Reg-T Req")
# this needs to copy the my.data object for dates
portfolio.stats           = rep(0, 9) 
dim(portfolio.stats)      = c(1, 9)
colnames(portfolio.stats) = stats

my.stats         = rep(list(portfolio.stats), length(my.data))
names(my.stats)  = names(my.data)

highs = Hi(RUT)
lows  = Lo(RUT)

total.trades   = 0
num.inc.quotes = 0
open.trades    = list()
closed.trades  = list()

# 2015-09-08 had fucked up price on SEP 1300 calls, fixed by hand in csv
# main backtest loop for now, operates on days
# symbols change names at i=29, delta messed up at i < 88
profvis({
for (i in 88:(length(my.data)-1)) { 
  #browser()
  #if (i > 146) browser()
  # steps 1 through 3b, operates on open trades
  if (length(open.trades) > 0) { # don't run w/ 0 trades open
    # create indicies of today's trades to exit
    to.exit = rep(FALSE, length(open.trades)) # ignore quote df w/ FALSE
    for (j in 1:length(open.trades)) {
      # if you ever close a trade before getting done with this loop
      # j will be ahead of the number of entries. check that.
      if (j > length(open.trades)) break
      # update today's trades with today's quotes
      # if your quoted symbols aren't there, raise error
      to.update = my.data[[i]]$Symbol %in% open.trades[[j]]$Symbol
      num.true  = length(to.update[to.update == TRUE])
      if (num.true <= 4) {
        if (num.true < 4) {
          num.inc.quotes = num.inc.quotes + 1
          symbols.to.update    = my.data[[i]][to.update,]$Symbol
          tmp.update           = subset(open.trades[[j]], 
                                        Symbol %in% symbols.to.update)
          # TODO how do you know what order this is in?
          tmp.update$mid.price = my.data[[i]][to.update,]$mid.price
          open.trades[[j]]     = rbind(tmp.update, 
                                       subset(open.trades[[j]], 
                                              !(Symbol %in% symbols.to.update)))
          open.trades[[j]] = open.trades[[j]][order(
                                open.trades[[j]]$Strike.Price),]
        } else {
          # TODO how do you know what order this is in?
          open.trades[[j]]$mid.price = my.data[[i]][to.update,]$mid.price
        }
        # exit if 90% profit, 200% loss, 5 days till exp, short strike touch
        if (ShouldExit(open.trades[[j]], highs, lows, names(my.data)[i]))
          to.exit[j] = TRUE
        # to.exit is now something like c(TRUE, TRUE, TRUE, FALSE, FALSE)
      } else {
        stop(paste("Quote for", 
                   names(my.data)[i], 
                   "does not contain entries for open trades"))
      }
    }
    # record open P/L as closed P/L for today for those indicies set TRUE
    # use cumsum() later to build an equity curve
    if (length(to.exit[to.exit == TRUE]) > 0) {
      # record profit as closed
      my.stats[[i]][6] = sum(unlist(lapply(open.trades[to.exit], 
                                           FloatingProfit)))
      # add to trade log
      for (k in 1:length(open.trades[to.exit])) {
        closed.trades[[length(closed.trades)+1]] = 
          TradeSummary(open.trades[to.exit][[k]],
                       as.Date(names(my.data)[i]))
      }
      # close trades by setting those indicies to NULL
      open.trades[to.exit] = NULL
    }
  }
  
  # Step 4 and 5. Decide if we should make a new trade
  # in 1 TPS backtest, easy, you create new trade every day
  # in 1 TPX backtest, use ShouldEnter()
  # if FindCondor returns NULL, this shouldn't do anything (I think?)
  if (global.mode == "1TPX" && as.numeric(oisuf.values[names(my.data)[i]]) > kOisufThresh && ShouldEnter(open.trades, my.data[[i]]) && !is.null(FindCondor(my.data[[i]])) && nrow(FindCondor(my.data[[i]])) == 4) {
    open.trades[[length(open.trades) + 1]] = FindCondor(my.data[[i]])
    total.trades = total.trades + 1
  } else if (global.mode == "1TPS" && !is.null(FindCondor(my.data[[i]])) && as.numeric(oisuf.values[names(my.data)[i]]) > kOisufThresh && nrow(FindCondor(my.data[[i]])) == 4) {
    open.trades[[length(open.trades) + 1]] = FindCondor(my.data[[i]])
    total.trades = total.trades + 1
  }
  
  # Record the floating profit
  my.stats[[i]][7] = sum(unlist(lapply(open.trades, 
                                       FloatingProfit)))
  # Something something portfolio stats something
  
}
})
# Easier view of stats manually after the fact
df.stats = data.frame(
            matrix(
              unlist(my.stats), 
              nrow=length(my.stats), 
              byrow=T, 
              dimnames=list(names(my.stats), colnames(portfolio.stats))))

# Plot the floating and closed profit
plot(1:nrow(df.stats), 
     cumsum(df.stats$Closed.P.L) + df.stats$Open.P.L, 
     type='l', 
     col='red')
lines(1:nrow(df.stats), 
      cumsum(df.stats$Closed.P.L))

df.closed.tradesF = do.call('rbind', closed.trades)

# Unit Tests
# Test ShouldExit does not close brand new trades
TestNoNewExit = function() {
  my.df = read.csv("sample-trade")
  return(ShouldExit(my.df, RUT, as.Date("2015-01-02")) == FALSE)
}
# Test ShouldExit's profit-based exit
TestProfitExit = function() {
  my.df = read.csv("sample-trade")
  my.df$mid.price = c(6, 1, 1, 4)
  return(ShouldExit(my.df, RUT, as.Date("2015-01-02")) == TRUE)
}
# Test ShouldExit's loss-based exit
TestLossExit = function() {
  my.df = read.csv("sample-trade")
  my.df$mid.price = c(1, 10, 10, 1)
  return(ShouldExit(my.df, RUT, as.Date("2015-01-02")) == TRUE)
}
# Test ShouldExit's time-based exit doesn't take you out early
TestEarlyExit = function() {
  my.df = read.csv("sample-trade")
  return(ShouldExit(my.df, RUT, as.Date("2016-01-04")) == FALSE)
}
# Test ShouldExit's time-based exit doesn't take you out late
TestLateExit = function() {
  my.df = read.csv("sample-trade")
  my.date  = "2016-02-16"
  modified.rut = RUT
  modified.rut[my.date,3] = xts(2000, order.by=as.Date(my.date))
  return(ShouldExit(my.df, modified.rut, as.Date(my.date)) == TRUE)
}
# Test ShouldExit's short-strike-based exit
TestShortCallExit = function() {
  my.df = read.csv("sample-trade")
  my.date  = "2016-02-01"
  modified.rut = RUT
  modified.rut[my.date,2] = xts(2000, order.by=as.Date(my.date))
  return(ShouldExit(my.df, modified.rut, as.Date(my.date)) == TRUE)
}
TestShortPutExit = function() {
  my.df = read.csv("sample-trade")
  my.date  = "2016-02-01" # this date already is lower than short put
  return(ShouldExit(my.df, RUT, as.Date(my.date)) == TRUE)
}

unit.tests = c(TestEarlyExit(), 
               TestLateExit(), 
               TestLossExit(), 
               TestNoNewExit(), 
               TestProfitExit(), 
               TestShortCallExit(), 
               TestShortPutExit())
if (all(unit.tests)) {
  print("Tests pass")
} else{
  print(paste("....-----++++[Tests failed: ", toString(unit.tests)))
}


# Profit factor
sum.wins   = sum(subset(df.stats, Closed.P.L > 0)$Closed.P.L)
sum.losses = sum(subset(df.stats, Closed.P.L < 0)$Closed.P.L)
if (sum.losses == 0) {
  print("Profit factor: inf.")
} else {
  print(paste("From: ", cal.begin, " To: ", cal.end,  
              " OISUF level: ", kOisufThresh))
  print(paste("N trades: ", total.trades, 
              " Profit factor: ", abs(sum.wins / sum.losses), sep=""))
}
















