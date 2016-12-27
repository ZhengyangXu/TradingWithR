library("RQuantLib")
library("bizdays")
# Static example data taken from EA EEV example 
underly  <- 72.05
Strike   <- 81
riskfr   <- 0.005
dividend <- 0
TTM      <- 6/252
Cprice   <- 0.355

rq_iv = AmericanOptionImpliedVolatility("call", Cprice, underly, Strike, 
                                        dividend, riskfr, TTM, 0.1)
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
                    '2017-09-04', '2017-11-23', '2017-12-25',
                    '2018-01-01', '2018-01-15', '2018-02-19',
                    '2018-03-30', '2018-05-28', '2018-07-04',
                    '2018-09-03', '2018-11-22', '2018-12-25',
                    '2019-01-01')

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

# What other filters apply for the IEV calculation? After bids/asks, we are
# left with 334 but Brian only has 271

# Do all the date math
# sample bizdays call: bizdays("2014-01-02", "2014-01-21", mycal) = 12
# Set up calendar
mycal = Calendar(holidays = uiHolidays, 
                 start.date = cal_begin, 
                 end.date=cal_end, 
                 weekdays=c("saturday", "sunday"))
# Make dates R friendly, be sure to filter our NAs before this
matrixComplete$expISO = as.Date(as.character(matrixComplete$Exp.Date), 
                                "%y%m%d")

# Calculate total days
matrixComplete$ND     = bizdays(analysis_date, 
                                matrixComplete$expISO, 
                                mycal)

# ur in R use ur vectorz 4 date maffs
matrixComplete$ED     = 1+floor(bizdays(next_earn_date, 
                                        matrixComplete$expISO,
                                        mycal) / 63)
matrixComplete$BD     = matrixComplete$ND - matrixComplete$ED

# No options at expiration
matrixComplete        = matrixComplete[matrixComplete$ND != 1,]

# No options without quotes
matrixComplete$avgp   = (matrixComplete$Bid + matrixComplete$Asked) / 2
matrixComplete        = matrixComplete[!is.na(matrixComplete$avgp),]

# Set up some other helper columns / values
matrixComplete$type[matrixComplete$Call.Put == "C"] = "call"
matrixComplete$type[matrixComplete$Call.Put == "P"] = "put"

# Calculate IV using BSOPM 
for (i in 1:length(matrixComplete$Strike.Price)) {
  matrixComplete$CalcIV[i] = 
    AmericanOptionImpliedVolatility(matrixComplete$type[i], 
                                    matrixComplete$avgp[i],
                                    underly,
                                    matrixComplete$Strike.Price[i],
                                    dividend, 
                                    riskfr, 
                                    matrixComplete$ND[i]/252,
                                    0.1)
}

# try out one of them there functions
maxOVVSkew = 1
matrixComplete$OVVSkew = fOVVSkew(matrixComplete$Strike.Price,
                                  underly,
                                  dividend,
                                  matrixComplete$ND/252,
                                  maxOVVSkew)

# fake iev value to test function
iev = 1.34
matrixComplete$NormIV = fNormIV(matrixComplete$ND, 
                                matrixComplete$ED, 
                                matrixComplete$BD,
                                iev,
                                matrixComplete$CalcIV)
