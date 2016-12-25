library("RQuantLib")

# Static example data taken from EA EEV example 
Spot     <- 72.05
Strike   <- 81
riskfr   <- 0.005
dividend <- 0
TTM      <- 6/252
Cprice   <- 0.355

rq_iv = EuropeanOptionImpliedVolatility("call", Cprice, Spot, Strike, dividend,
                                        riskfr, TTM, 0.1)
cat("Excel thinks Calculated IV is 60.32%, but RQL thinks it's: ", 
    rq_iv[1]*100, "%")
##############################

# example on whole array to see why excel != RQL
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

# Set up only to use those strikes with bids/asks, look for Date
matrixComplete = mydata[!is.na(mydata$Date),]

# What other filters apply for the IEV calculation?

# Do all the date math
matrixComplete$isoDate = as.Date(mydata$Date, "%y%m%d") # will error if any NAs
matrixComplete$ND      = 1                              # total days
matrixComplete$ED      = 1                              # earnings days
matrixComplete$BD      = 1                              # non-earnings days

for (i in 1:length(matrixComplete$Strike.Price)) {
  matrixComplete[i]$CalcIV = 
    AmericanOptionImpliedVolatility("call", 
                                    matrixComplete[i]$Last,
                                    underly,
                                    matrixComplete[i]$Strike.Price,
                                    dividend, 
                                    riskfr, 
                                    matrixComplete[i]$ND,
                                    0.1)
}
