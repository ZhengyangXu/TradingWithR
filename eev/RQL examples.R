library("RQuantLib")
library("bizdays")
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
analysis_date   = "2015-07-30"
first_u_date    = "2015-07-31"
second_u_date   = "2015-07-31"
next_earn_date  = "2015-07-30"
next_ext_date   = "2015-08-07"
cal_begin       = "2014-01-01"
cal_end         = "2017-12-31"
datecode        = "20150730"
timecode        = "1430"
ticker          = "ea"
uiHolidays      = c('2014-01-01', '2014-01-20', '2014-02-17',
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
                    '2017-09-04', '2017-11-23', '2017-12-25')

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
# Set up calendar
mycal = Calendar(holidays = uiHolidays, 
                 start.date = cal_begin, 
                 end.date=cal_end, 
                 weekdays=c("saturday", "sunday"))

# sample bizdays call: bizdays("2014-01-02", "2014-01-21", mycal) = 12
matrixComplete$expISO = as.Date(as.character(matrixComplete$Exp.Date), 
                                "%y%m%d")             # make sure no NAs
matrixComplete$ND     = bizdays(analysis_date, 
                                matrixComplete$expISO, 
                                mycal)                # total days
# if next earn date < expdate, 1
# if next earn date > expdate, 0
# else 1+floor(bizdays(next earn date, expdate) / 63)
#matrixComplete$ED     = floor(matrixComplete$ND / 63) # earnings days
# ur in R use ur vectorz 
#a = next_earn_date > matrixComplete$expISO
#b = next_earn_date < matrixComplete$expISO
matrixComplete$ED     = 1+floor(bizdays(next_earn_date, 
                                        matrixComplete$expISO,
                                        mycal) / 63)
matrixComplete$BD     = matrixComplete$ND - matrixComplete$ED

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
