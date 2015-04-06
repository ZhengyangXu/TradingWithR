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
begin_year  = 1994
end_year    = 2014
symbol      = "V"
# Why does V start out as 115k on b&h?
tickers      = c('VZ', 'T', 'PFE')
#tickers     = c('MMM', 'AXP', 'T', 'BA', 'CAT', 'CVX', 'CSCO', 'KO', 'DIS', 'DD',
#               'XOM', 'GE', 'HD', 'IBM', 'INTC', 'JNJ', 'JPM', 'MCD', 'MRK',
#                'MSFT', 'NKE', 'PFE', 'PG', 'TRV', 'UTX', 'UNH', 'VZ',  'WMT')
return_type = "arithmetic" # 'log' or 'arithmetic'
initCash    = 300000
initBal     = NULL
initShares  = NULL
rollBal     = NULL
final       = NULL
final_t     = NULL
final_bh    = NULL

#getSymbols(paste(symbol),from=paste((begin_year-1),'01','01',sep='-'))
getSymbols(tickers,from=paste((begin_year-1),'01','01',sep='-'))
sane_tickers = gsub("[][^]", "", tickers)
for (tick in tickers) {
  #symbol = gsub("[][^]", "", symbol) # so we can use get(symbol)
  tick       = gsub("[][^]", "", tick)
  initPrice  = Ad(first(to.monthly(get(tick))[paste(paste(begin_year,"01",sep='-'),end_year,sep='::')]))
  initShares = floor(initCash / length(tickers) / initPrice)
  initBal    = initPrice * initShares

  for (y in begin_year:end_year) {
    for (i in 1:12) {
      if (i == 12) {
        if (y == 2014) break;
        tmp_g      = rbind(first(last(get(tick)[paste(y, i, sep="-")],"5 days")),
                           last(first(get(tick)[paste((y+1), 1, sep="-")],"3 days")))
        tmp_g_delt = Delt(Ad(tmp_g),type=return_type)[nrow(tmp_g)]
        final_g    = rbind(final_g, tmp_g_delt)
      }
      else if (is.null(final_g)) {
        tmp_g      = rbind(first(last(get(tick)[paste(y, i, sep="-")],"5 days")),
                           last(first(get(tick)[paste(y, (i+1), sep="-")],"3 days")))
        tmp_g_delt = Delt(Ad(tmp_g),type=return_type)[nrow(tmp_g)]
        final_g    = tmp_g_delt
      }
      else {
        tmp_g      = rbind(first(last(get(tick)[paste(y, i, sep="-")],"5 days")),
                           last(first(get(tick)[paste(y, (i+1), sep="-")],"3 days")))
        tmp_g_delt = Delt(Ad(tmp_g),type=return_type)[nrow(tmp_g)]
        final_g    = rbind(final_g, tmp_g_delt)
      }
    }
  }

  # Compute cash results
  m_rollBal    = Cl(to.monthly(final_g))
  m_final_g    = Cl(to.monthly(final_g))
  m_rollBal[1] = initBal

  # -20 for transaction costs
  for (i in 1:(nrow(m_final_g)-1)) { m_rollBal[i+1] = m_rollBal[i]*(1+m_final_g[i])-20 }
  colnames(m_rollBal) = paste(tick,"tom",sep="-")

  # Compute buy-and-hold results
  m_bandh = as.integer(initShares) *
            Ad(to.monthly(get(tick)[paste(paste(begin_year,"02",sep='-'),
                                          end_year,sep='::')]))
  colnames(m_bandh) = paste(tick,"bandh",sep="-")

  if (is.null(final_t) && is.null(final_bh)) {
    final_t  = m_rollBal
    final_bh = m_bandh
  }
  else {
    final_t  = merge(final_t, m_rollBal) # balances by ticker
    final_bh = merge(final_bh, m_bandh) # balances by ticker
  }
}


# Plot the tom vs bh baskets
# assumes arithmetic averages
plotBaskets = function(basket1, basket2) {
   # These don't keep xts objects
   s1 = apply(basket1, 1, sum) #function used to be mean? wtf?
   s2 = apply(basket2, 1, sum) #function used to be mean? wtf?
   # Instead use 1337 R tricks
   #"[<-"(basket1, , vapply(basket1, sum, FUN.VALUE = numeric(nrow(basket1))))
   #"[<-"(basket1, , vapply(basket1, sum, FUN.VALUE = numeric(nrow(basket1))))
   plot(s1,
        type='l',
        ylim=c(min(s1, s2),max(s1, s2)),
        ylab="Account Balance ($)",
        main="Baskets: Buy-and-hold vs. Turn-of-month"
        )
   lines(s2, col='blue')
   axis(4)
   legend("topleft", # Legend position
          c('Buy-and-hold',
            'Turn-of-month'),  # Series labels
          lty=1,     # Line type #1
          lwd=2,     # Line width 2 points
          col=c('black', 'blue'), # Series colors
          cex=0.75
         )
}
plotBaskets(final_bh, final_t)

# --- begin multi plot ---
# Plots on plots on plots
# 'final' now contains all our series
#ymin = min(final)
#ymax = max(final)
#mcol = sample(colors(distinct=TRUE), 6) # get 6 random colors, they suck sometimes

#mcol = c('black', rainbow(5))
# this is messy without labeled series
#mcol = c("black", "red", "blue", "green", 'orange', 'purple')
#ypos = seq(0, signif(ymax,digits=0), by=50000)
#plot(final[,1],
     #main=paste("Cash account value for ToM vs. B&H for", tickers),
#     ylim=c(ymin,ymax),
#     ylab="Account Balance ($)"
#     )
#for (i in 2:6) { lines(final[,i], col=mcol[i]) }
#l_labels = NULL
#for (i in 1:3) { l_labels[i] = paste(sane_tickers[i],'_tom',sep='') }
#l_labels = c(l_labels, sane_tickers)
   #axis(4, at=ypos, labels=sprintf("%dk", ypos/10))
#axis(4)

#legend("topleft", # Legend position
#       l_labels,  # Series labels
#       lty=1,     # Line type #1
#       lwd=2,     # Line width 2 points
#       col=mcol, # Series colors
#       cex=0.75)
# --- end multi plot ---
# Summarize results
#summary = cbind(table.Stats(final_g), table.Stats(bandh), table.Stats(final_p))
#colnames(summary) = c("T-3 to T+3", "Buy & Hold", "T+4 to T-4")

# Other fun graphs
# charts.PerformanceSummary(bandh)
# charts.PerformanceSummary(final_g)