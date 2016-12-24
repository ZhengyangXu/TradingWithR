library("RQuantLib")

## Example data taken from AKAM EEV example 
Spot     <- 47.29
Strike   <- 46
riskfr   <- 0.005
dividend <- 0
TTM      <- 2/252
Cprice   <- 3.05

rq_iv = EuropeanOptionImpliedVolatility("call", Cprice, Spot, Strike, dividend,
                                        riskfr, TTM, 0.1)
cat("Excel thinks Calculated IV is 141.447%, but RQL thinks it's: ", 
    rq_iv[1]*100, "%")

strikes = seq(42, 48.5, by=0.5)
vols = seq(0.01, 0.14, by=0.01)

iv_array = EuropeanOptionImpliedVolatility

# these will prob change every time
analysis_date   = "2015-08-27"
#analysis_datep1 = analysis_date + 1 # broken
first_u_date    = "2015-08-28"
second_u_date   = "2015-08-28"
next_earn_date  = "2015-08-27"
next_ext_date   = "2015-08-07"
datecode        = "20150730"
timecode        = "1430"
ticker          = "ea"
options_file = paste(ticker, datecode, timecode, '.csv',
                     sep='')
stock_file   = paste(ticker, '.asc', sep='')
mydata       = read.csv(options_file)
myivprice    = read.csv(stock_file)
myivprice    = myivprice[!is.na(myivprice$IV30),] # no NA iv
underly      = mydata[1]$Last
mydata       = mydata[-1,]                        # top row is underlying

# Set up only to use those strikes with bids/asks?
matrixComplete = mydata[!is.na(mydata$Date),]

# What other filters apply for the IEV calculation?

# Do all the date math
mydata$isoDate = as.Date(mydata$Date, "%y%m%d") # will error if any NAs
mydata$ND      = 1                              # total days
mydata$ED      = 1                              # earnings days
mydata$BD      = 1                              # non-earnings days

for (i in 1:length(mydata$Strike.Price)) {
  mydata[i]$CalcIV = AmericanOptionImpliedVolatility("call", 
                                                     mydata[i]$Last,
                                                     underly,
                                                     mydata[i]$Strike.Price,
                                                     dividend, 
                                                     riskfr, 
                                                     mydata[i]$ND,
                                                     0.1)
}
