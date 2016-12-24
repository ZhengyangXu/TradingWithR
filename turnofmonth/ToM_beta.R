# Test the hypothesis that all the returns are located at T-3 and T+3 of the month
# Where T = last trading day of the month and we only use trading days

# Authors found that this behavior was stronger over those stocks that were more
# liquid, more widely held, more volatile (controled for liquidity)

# TODO / DONE:
# --use adjusted prices-- DONE.
# --combine both time periods into one file DONE.
# --legend-- DONE.
# --arithmetic or log returns? ARITHMETIC. DONE.
# --make actual $ demo with quantstrat DONE.
# - run on array of stocks all at once DONE.
# - which stocks work best? which are worst?
# - do pairs trading like Friesen suggested
# - mid-month effect?
library(PerformanceAnalytics)
library(quantmod)

Sys.setenv(TZ="UTC")
final_g    = NULL
final_p    = NULL
vol_bh     = NULL
vol_tm     = NULL
perfdiff   = NULL
begin_year = 2014
end_year   = 2016
symbol     = "DJIA"
#tickers     = c('MMM', 'AXP', 'T', 'BA', 'CAT', 'CVX', 'CSCO', 'KO', 'DIS', 'DD',
#                'XOM', 'GE', 'GS', 'HD', 'IBM', 'INTC', 'JNJ', 'JPM', 'MCD', 'MRK',
#                'MSFT', 'NKE', 'PFE', 'PG', 'TRV', 'UTX', 'UNH', 'VZ', 'V', 'WMT')
tickers     = c('MMM', 'AXP', 'T', 'BA', 'CAT', 'CVX', 'CSCO', 'KO', 'DIS', 'DD',
                'XOM', 'GE',        'HD', 'IBM', 'INTC', 'JNJ', 'JPM', 'MCD', 'MRK',
                'MSFT', 'NKE', 'PFE', 'PG', 'TRV', 'UTX', 'UNH', 'VZ',       'WMT')

return_type = "arithmetic" # 'log' or 'arithmetic'

getSymbols(paste(tickers),from=paste((begin_year-1),'01','01',sep='-'))
tickers = gsub("[][^]", "", tickers)
for (symbol in tickers) {
   final_g = NULL
   final_p = NULL
   for (y in begin_year:end_year) {
     for (i in 1:12) {
       if (i == 12) {
         if (y == 2016) break;
         tmp_g      = rbind(first(last(get(symbol)[paste(y, i, sep="-")],"5 days")),
                            last(first(get(symbol)[paste((y+1), 1, sep="-")],"3 days")))
         tmp_g_delt = Delt(Ad(tmp_g),type=return_type)[nrow(tmp_g)]
         final_g    = rbind(final_g, tmp_g_delt)

         tmp_p = last(first(get(symbol)[paste(y,i,sep='-')],"-2 days"),"-4 days")
         tmp_p_delt = Delt(Ad(tmp_p), k=(nrow(tmp_p)-1), type=return_type)[(nrow(tmp_p))]
         final_p = rbind(final_p, tmp_p_delt)
       }
       else if (is.null(final_g)) {
         tmp_g      = rbind(first(last(get(symbol)[paste(y, i, sep="-")],"5 days")),
                             last(first(get(symbol)[paste(y, (i+1), sep="-")],"3 days")))
         tmp_g_delt = Delt(Ad(tmp_g),type=return_type)[nrow(tmp_g)]
         final_g    = tmp_g_delt

         tmp_p = last(first(get(symbol)[paste(y,i,sep='-')],"-2 days"),"-4 days")
         tmp_p_delt = Delt(Ad(tmp_p), k=(nrow(tmp_p)-1), type=return_type)[(nrow(tmp_p))]
         final_p = tmp_p_delt
       }
       else {
         tmp_g      = rbind(first(last(get(symbol)[paste(y, i, sep="-")],"5 days")),
                             last(first(get(symbol)[paste(y, (i+1), sep="-")],"3 days")))
         tmp_g_delt = Delt(Ad(tmp_g),type=return_type)[nrow(tmp_g)]
         final_g    = rbind(final_g, tmp_g_delt)

         tmp_p = last(first(get(symbol)[paste(y,i,sep='-')],"-2 days"),"-4 days")
         tmp_p_delt = Delt(Ad(tmp_p), k=(nrow(tmp_p)-1), type=return_type)[(nrow(tmp_p))]
         final_p = rbind(final_p, tmp_p_delt)
       }
     }
   }

   # compare to buy-and-hold
   bandh = Delt(Ad(to.monthly(get(symbol))),
                type=return_type)[paste(paste(begin_year,"02",sep='-'),end_year,sep='::')]

   # Calculate cumulative returns
   c_bandh   = cumsum(bandh)
   c_final_g = cumsum(final_g)
   c_final_p = cumsum(final_p)

   # Prep chart params
   ymin = min(c_bandh,c_final_g, c_final_p)
   ymax = max(c_bandh,c_final_g, c_final_p)

   # Keep a tally of stats for later
   vol_bh   = c(vol_bh, sd(c_bandh))
   vol_tm   = c(vol_tm, sd(c_final_g))
   perfdiff = c(perfdiff, as.numeric(last(c_final_g)) / as.numeric(last(c_bandh)))

   # See what happened - this works just takes time
   plot(c_bandh,main=paste("Turn-of-Month cumulative monthly\n", return_type, "returns for", symbol), ylim=c(ymin,ymax))
   lines(c_final_g, col='blue')
   lines(c_final_p, col='red')
   axis(side=4)
   l_labels = c("Buy & Hold", "T-3 to T+3", "T+4 to T-4")
   l_colors = c("black", "blue", "red")
   legend("topleft", # Legend position
          l_labels,  # Series labels
          lty=1,     # Line type #1
          lwd=2,     # Line width 2 points
          col=l_colors, # Series colors
          cex=0.75)
}

vol           = as.matrix(cbind(vol_bh, vol_tm, perfdiff))
rownames(vol) = tickers
vol           = as.matrix(vol)
#nice_sd       = vol[order(vol), , drop=FALSE]


# Summarize results
#summary = cbind(table.Stats(final_g), table.Stats(bandh), table.Stats(final_p))
#colnames(summary) = c("T-3 to T+3", "Buy & Hold", "T+4 to T-4")

# Other fun graphs
# charts.PerformanceSummary(bandh)
# charts.PerformanceSummary(final_g)