# Options Backtester
# v0.1 requirements:
#    0. DONE be able to read in quotes from OptionVue
#       also, use quantmod to read the chains in from Yahoo finance
#    1. be able to tell the expiration months apart and keep track of DTE
#       a. what data structure to use here? read one matrix from disk at a 
#          time or store all data in an array or list of data frames?
#          put everything in one data frame with expirations caught in 
#          the symbol names? I think this is how it normally works.
#           i. Probably starting with a list of data frames
#    2. be able to choose options based on delta in the quotes
#    3. be able to track open P/L and closed P/L
#    4. be able to exit a trade on delta in the quotes, P/L, DTE
#    5. be able to print the greeks of the total position every time interval
#    6. With all the above, should be able to backtest an iron condor strategy

# How to deal with time? Probably use a list.
#   - If each time interval is a new list entry, and every tick of the tester
#     operates on one list entry at a time, you could test with any frequency
#     data.
#   - You can name the list entries with timestamps. Either unix epochs or 
#     normal dates or datetimes. 
#   - You can use lapply() on every list entry if need be.

# How to deal with keeping track of P/L? Probably use a list?
#   - If your time is kept track of by a list, you can have the first
#     element be the quote data frame and the second element be the current
#     P/L of the strategy. So you'd have a list of lists.
#   - What to track, delta or P/L wise? The same stuff as OptionVue. Track
#     these on a per-trade basis and as a total portfolio: orig price,
#     current greeks (delta, gamma, theta, vega, rho), market price,
#     DTE (by position only), DTE at start?, open P/L, day change P/L,
#     Reg-T margin req
#   - How do you track a trade vs a position vs a portfolio? Easiest thing is
#     grouping by letter or number or alphanumeric or hash. Just need a lookup
#     table to answer "which trades make up a position?" Or do you just do
#     like optionvue and do it by expiration and account and symbol? Simplest
#     for right now is by expiration and account and symbol.

# What does namespace stuff look like?
#   - R Global Environment
#     - Account environment (single variable)
#       - Symbol environment (list of lists)
#         - names of list entries are dates
#           - first entry in the list is the df for that time / symbol
#           - second entry is the stats of that symbol (and by nature of the
#             data structure, that account) at that time. open expirations
#             must be taken into account in this data frame
#  -or-
#
#   - R Global
#     - generic 'data' object (list of lists)
#       - names of list entries are dates
#         - names of each list content are addressed as symbol and symbol-stats
#           this way, no empty data for when there are no trades open for a
#           certain symbol
#  -or-
#
# fuck it; backtester MVP does one symbol and one account at a time
#   - R global
#     - data object (list of lists)
#       - names of list entries are dates
#         - first entry in the list is the df for that time / symbol
#         - second entry is the stats of that symbol (and by nature of the
#           data structure, that account) at that time. open expirations
#           must be taken into account in this data frame
#
# -or-
#
# use a zoo object for time, and first column is the list of quotes and
# second column is list of stats??

# Use EAE() instead of == on floating point numbers
EAE <- Vectorize(function(x, y) {isTRUE(all.equal(x, y))})

# Setup date stuff
library('bizdays')
cal.begin       = "2016-01-01"
cal.end         = "2018-12-31"
my.holidays     = c('2014-01-01', '2014-01-20', '2014-02-17',
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
OptionQuotesCsv = function(ticker, date.code, time.code) {
  options.file  = paste(ticker, date.code, time.code, '.csv',
                       sep='')
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
  my.exp.date = as.Date(as.character(my.df$Exp.Date), "%y%m%d")
  my.iso.date = as.Date(as.character(my.df$Date), "%y%m%d")
  biz.dte     = bizdays(my.iso.date,
                        my.exp.date,
                        my.cal)
  cal.dte     = as.numeric(my.exp.date - my.iso.date)
  mid.price   = (my.df$Bid + my.df$Asked) / 2
  return(cbind(my.df, my.iso.date, my.exp.date, biz.dte, cal.dte, mid.price))
}

# works fine
my.df = EnrichOptionsQuotes(OptionQuotesCsv("RUT", 20170306, 1600))




