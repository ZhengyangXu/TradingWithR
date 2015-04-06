# Test the hypothesis that all the returns are located at T-3 and T+3 of the month
# Where T = last trading day of the month and we only use trading days

# TODO / DONE:
# --use adjusted prices-- DONE.
# --combine both time periods into one file DONE.
# --legend-- DONE.
# --arithmetic or log returns? ARITHMETIC. DONE.
# --make actual $ demo with quantstrat DONE.
# - run on array of stocks all at once - arithmetic can sum returns
# - which stocks work best? which are worst?
# - do pairs trading like Friesen suggested
# - mid-month effect?
# - re-read january effect debunk
# - figure out real label for log return axis

Sys.setenv(TZ="UTC")
final_g     = NULL
final_p     = NULL
begin_year  = 2002
end_year    = 2014
symbol      = "MSFT"
return_type = "arithmetic" # 'log' or 'arithmetic'
initCash    = 100000
initBal     = NULL
initShares  = NULL
rollBal     = NULL

getSymbols(paste(symbol),from=paste((begin_year-1),'01','01',sep='-'))
symbol = gsub("[][^]", "", symbol) # so we can use get(symbol)

initPrice  = Ad(first(to.monthly(get(symbol))[paste(paste(begin_year,"01",sep='-'),end_year,sep='::')]))
initShares = floor(initCash / initPrice)
initBal    = initPrice * initShares

for (y in begin_year:end_year) {
  for (i in 1:12) {
    if (i == 12) {
      if (y == 2014) break;
      tmp_g      = rbind(last(get(symbol)[paste(y, i, sep="-")],"5 days"),
                         first(get(symbol)[paste((y+1), 1, sep="-")],"3 days"))
      tmp_g_delt = Delt(Ad(tmp_g),k=(nrow(tmp_g)-1),type=return_type)[nrow(tmp_g)]
      final_g    = rbind(final_g, tmp_g_delt)
    }
    else if (is.null(final_g)) {
      tmp_g      = rbind(last(get(symbol)[paste(y, i, sep="-")],"5 days"),
                         first(get(symbol)[paste(y, (i+1), sep="-")],"3 days"))
      tmp_g_delt = Delt(Ad(tmp_g),k=(nrow(tmp_g)-1),type=return_type)[nrow(tmp_g)]
      final_g    = tmp_g_delt
    }
    else {
      tmp_g      = rbind(last(get(symbol)[paste(y, i, sep="-")],"5 days"),
                         first(get(symbol)[paste(y, (i+1), sep="-")],"3 days"))
      tmp_g_delt = Delt(Ad(tmp_g),k=(nrow(tmp_g)-1),type=return_type)[nrow(tmp_g)]
      final_g    = rbind(final_g, tmp_g_delt)
    }
  }
}

# Compute cash results
m_rollBal = Cl(to.monthly(final_g))
m_final_g = Cl(to.monthly(final_g))
m_rollBal[1] = initBal
# -20 for transaction costs
for (i in 1:(nrow(m_final_g)-1)) { m_rollBal[i+1] = m_rollBal[i]*(1+m_final_g[i])-20 }

# Compute buy-and-hold results
m_bandh = as.integer(initShares) * Ad(to.monthly(get(symbol)[paste(paste(begin_year,"02",sep='-'),end_year,sep='::')]))


# Plots on plots on plots
ymin = min(m_rollBal, m_bandh)
ymax = max(m_rollBal, m_bandh)
ypos = seq(0, signif(ymax,digits=0), by=50000)
plot(m_bandh,
     main=paste("Cash account value for ToM vs. B&H for", symbol),
     ylim=c(ymin,ymax),
     ylab="Account Balance ($)"
     )
lines(m_rollBal, col="blue")
#axis(4, at=ypos, labels=sprintf("%dk", ypos/10))
axis(4)
l_labels = c("Buy & Hold", "Turn-of-month")
l_colors = c("black", "blue")
legend("topleft", # Legend position
       l_labels,  # Series labels
       lty=1,     # Line type #1
       lwd=2,     # Line width 2 points
       col=l_colors, # Series colors
       cex=0.75)

# compare to buy-and-hold
#bandh = Delt(Ad(to.monthly(get(symbol))),
#             type=return_type)[paste(paste(begin_year,"02",sep='-'),end_year,sep='::')]

# Calculate cumulative returns
#c_bandh = cumsum(bandh)
#c_final_g = cumsum(final_g)
#c_final_p = cumsum(final_p)

# Prep chart params
#ymin = min(c_bandh,c_final_g)
#ymax = max(c_bandh,c_final_g)

# See what happened
#plot(c_bandh,main=paste("Turn-of-Month cumulative monthly\n", return_type, "returns for", symbol), ylim=c(ymin,ymax))
#lines(c_final_g, col='blue')
#lines(c_final_p, col='red')
#axis(side=4)
#l_labels = c("Buy & Hold", "T-3 to T+3", "T+4 to T-4")
#l_colors = c("black", "blue", "red")
#legend("topleft", # Legend position
#       l_labels,  # Series labels
#       lty=1,     # Line type #1
#       lwd=2,     # Line width 2 points
#       col=l_colors, # Series colors
#       cex=0.75)

# Summarize results
#summary = cbind(table.Stats(final_g), table.Stats(bandh), table.Stats(final_p))
#colnames(summary) = c("T-3 to T+3", "Buy & Hold", "T+4 to T-4")

# Other fun graphs
# charts.PerformanceSummary(bandh)
# charts.PerformanceSummary(final_g)