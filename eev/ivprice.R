# read in a file and do some filtering... maybe.
# first line of data is the underlying stock

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

dATMIV_per_1pct_dPrice_inc = 0.0 # C16
dATMIV30 = 0                     # F16
pct_uvov_correction = 100        # C17 100%
pct_earnings_vol_realized = 175  # C19 175%
pct_earnings_vol_implied  = 230  # D19 230%
ln_price_return = 1              # C21

options_file = paste(ticker, datecode, timecode, '.csv',
                     sep='')
stock_file   = paste(ticker, '.asc', sep='')
mydata       = read.csv(options_file)
myivprice    = read.csv(stock_file)
myivprice    = myivprice[!is.na(myivprice$IV30),] # no NA iv

# many assumptions / filters
# Basic stuff shouldn't change
ticker         = mydata[1,]['Symbol'] # redefined
u_price        = mydata[1,]['Last']
div_per_year   = 4
risk_free_rate = 0.25     # 0.25%
opt_mult       = 100
opt_murican    = 1
tcost_slip      = -0.10          # C25
tcost_commish   = -0.75          # D25

# might change
div_yield = 0.0
div_amt   = div_yield * u_price / div_per_year
min_opt_price        = 0.20
min_tdays_to_exp     = 2
#min_tdays_to_exp_opt = # actual trade days
max_tdays_to_exp     = 504
max_tdays_to_exp_opt = 75
min_vol         = 0
min_open_int    = 0
min_call_delt   = 4
max_call_delt   = 96
min_put_delt    = -96
max_put_delt    = -4
max_est_skew    = 1
min_ror_ivprice = -30 # -30%
max_ror_ivprice = 30
holidays = c('01/01/2014', '01/20/2014', '02/17/2014',
             '04/18/2014', '05/26/2014', '07/03/2014',
             '09/01/2014', '11/27/2014', '12/25/2014',
             '01/01/2015', '01/19/2015', '02/16/2015',
             '04/03/2015', '05/25/2015', '07/03/2015',
             '09/07/2015', '11/26/2015', '12/25/2015',
             '01/01/2016', '01/18/2016', '02/15/2016',
             '03/25/2016')

# Calculate things
# Print summary
# Prompt / get changed user inputs

# IVPrice duplication
max_days_lookback_price = 22 # D1
max_days_lookback_iv30  = 22 # G1
lagpad <- function(x, k) {
    if (!is.vector(x))
        stop('x must be a vector')
    if (!is.numeric(x))
        stop('x must be numeric')
    if (!is.numeric(k))
        stop('k must be numeric')
    if (1 != length(k))
        stop('k must be a single number')
    c(rep(NA, k), x)[1 : length(x)]
}

# Col E
# get a lagged close price
myivprice$closelag = lagpad(myivprice$CLOSE, k=max_days_lookback_price)
# compute log returns
myivprice$logret = log(myivprice$CLOSE / myivprice$closelag)
# fill in gaps for the first returns < position 'xvar'
myivprice$logret[is.na(myivprice$logret)] = log(myivprice$CLOSE[is.na(myivprice$logret)] / myivprice$CLOSE[1])

# Col F
# TODO: Brian checks for IV30 < 10 (1000%); why?
# get a lagged iv30
myivprice$iv30lag = lagpad(myivprice$IV30, k=max_days_lookback_iv30)
# compute delta 30-day IV
myivprice$deltiv30 = (myivprice$IV30 - myivprice$iv30lag) / 1000
# fill in gaps for NA
myivprice$deltiv30[is.na(myivprice$deltiv30)] = (myivprice$IV30[is.na(myivprice$deltiv30)] - myivprice$IV30[1])/1000

# slope calc is sumproduct(E range: F range) / sumproduct(E range:E range)
# x = myivprice$logret[2:3]
# y = myivprice$deltiv30[2:3]
# slope calc for row 3 = sum(x*y) / sum(x*x) / 100
# but this should only run on rolling 22 entries
# fuck it; use a for loop
myivprice$slope = rep(0, nrow(myivprice))
for (i in 2:nrow(myivprice)) {
    # tried to do this for legibility but didn't work
    #x_range = paste(max(i - max_days_lookback_price + 1, 2), i, sep=':')
    #y_range = paste(max(i - max_days_lookback_price + 1, 2), i, sep=':')
    myivprice$slope[i] = sum(myivprice$logret[max(i - max_days_lookback_price  + 1, 2):i] *
                             myivprice$deltiv30[max(i - max_days_lookback_iv30 + 1, 2):i]) /
                         sum(myivprice$logret[max(i - max_days_lookback_price  + 1, 2):i] *
                             myivprice$logret[max(i - max_days_lookback_price  + 1, 2):i]) / 100
}
# not proper statistics :)
# NOTE!!! this whole thing will drop IV = 0 entries somehow
myivprice$slope[is.nan(myivprice$slope)] = 0
avg_slope  = mean(myivprice$slope)
last_slope = myivprice$slope[nrow(myivprice)]
print(sprintf("Average slope: %1.4f%%", avg_slope*100))
print(sprintf("Last 22 days:  %1.4f%%", last_slope*100))
print(sprintf("Those 2 avg'd: %1.4f%%", mean(c(avg_slope, last_slope))*100))

filtered_data = rbind(mydata[mydata[mydata$Delta > 4,]$Delta < 96,], 
                      mydata[mydata[mydata$Delta < -4,]$Delta > -96,])
