# Test the hypothesis that all the returns are located at T-3 and T+3 of the month
# Where T = last trading day of the month and we only use trading days

# Authors found that this behavior was stronger over those stocks that were more
# liquid, more widely held, more volatile (controled for liquidity)

# TODO
# - run on array of stocks all at once
# - which stocks work best? which are worst?
# - do pairs trading like Friesen suggested
# - mid-month effect?
library("PerformanceAnalytics")
Sys.setenv(TZ="UTC")
final_g = NULL
final_p = NULL
begin_year = 2013
end_year = 2016
symbol = "SPY"
return_type = "log" # 'log' or 'arithmetic'

getSymbols(paste(symbol),from=paste((begin_year-1),'01','01',sep='-'))
symbol = gsub("[][^]", "", symbol)

for (y in begin_year:end_year) {
  for (i in 1:12) {
    if (i == 12) {
      if (y == 2016) break;
      tmp_g = rbind(last(get(symbol)[paste(y, i, sep="-")],"5 days"),
                  first(get(symbol)[paste((y+1), 1, sep="-")],"3 days"))
      tmp_g_delt = Delt(Ad(tmp_g),k=(nrow(tmp_g)-1),type=return_type)[nrow(tmp_g)]
      final_g = rbind(final_g, tmp_g_delt)

      tmp_p = last(first(get(symbol)[paste(y,i,sep='-')],"-2 days"),"-4 days")
      tmp_p_delt = Delt(Ad(tmp_p), k=(nrow(tmp_p)-1), type=return_type)[(nrow(tmp_p))]
      final_p = rbind(final_p, tmp_p_delt)
    }
    else if (is.null(final_g)) {
      tmp_g = rbind(last(get(symbol)[paste(y, i, sep="-")],"5 days"),
                  first(get(symbol)[paste(y, (i+1), sep="-")],"3 days"))
      tmp_g_delt = Delt(Ad(tmp_g),k=(nrow(tmp_g)-1),type=return_type)[nrow(tmp_g)]
      final_g = tmp_g_delt

      tmp_p = last(first(get(symbol)[paste(y,i,sep='-')],"-2 days"),"-4 days")
      tmp_p_delt = Delt(Ad(tmp_p), k=(nrow(tmp_p)-1), type=return_type)[(nrow(tmp_p))]
      final_p = tmp_p_delt
    }
    else {
      tmp_g = rbind(last(get(symbol)[paste(y, i, sep="-")],"5 days"),
                  first(get(symbol)[paste(y, (i+1), sep="-")],"3 days"))
      tmp_g_delt = Delt(Ad(tmp_g),k=(nrow(tmp_g)-1),type=return_type)[nrow(tmp_g)]
      final_g = rbind(final_g, tmp_g_delt)

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
c_bandh = cumsum(bandh)
c_final_g = cumsum(final_g)
c_final_p = cumsum(final_p)

# Prep chart params
ymin = min(c_bandh,c_final_g, c_final_p)
ymax = max(c_bandh,c_final_g, c_final_p)

# See what happened
plot(c_bandh,
     main=paste("Turn-of-Month cumulative monthly\n", return_type, "returns for", symbol),
     ylim=c(ymin,ymax))
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
       cex=1)

# Summarize results
summary = cbind(table.Stats(final_g), table.Stats(bandh), table.Stats(final_p))
colnames(summary) = c("T-3 to T+3", "Buy & Hold", "T+4 to T-4")

# Other fun graphs
 charts.PerformanceSummary(bandh)
 charts.PerformanceSummary(final_g)
