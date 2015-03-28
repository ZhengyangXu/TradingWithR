# steps to actually trade and keep track of balance
# while there are rates to get
#   get newest rates
#   convert to R format
#   convert to usable format for this script
#   compute max profit (irates)
#   print a log of the trade with timestamp
#   update balance

#rates = fromJSON('{"USD_JPY": "102.4590", "USD_USD": "1.0000000", "JPY_EUR": "0.0075", "BTC_USD": "115.65", 
#                   "JPY_BTC": "0.0000811", "USD_EUR": "0.7779", "EUR_USD": "1.2851", "EUR_JPY": "131.711", 
#                   "JPY_USD": "0.0098", "BTC_BTC": "1.0000000", "EUR_BTC": "0.01125", "BTC_JPY": "12325.44", 
#                   "JPY_JPY": "1.0000000", "BTC_EUR": "88.8499", "EUR_EUR": "1.0000000", "USD_BTC": "0.0083"}')
winnings = 0
english = c("USD EUR BTC USD",
            "USD JPY BTC USD",
            "USD BTC EUR USD",
            "USD BTC JPY USD",
            "USD EUR BTC JPY USD",
            "USD EUR JPY BTC USD",
            "USD JPY EUR BTC USD",
            "USD JPY BTC EUR USD",
            "USD BTC EUR JPY USD",
            "USD BTC JPY EUR USD")
while (winnings < 100) {
  rates = fromJSON(readLines("http://fx.priceonomics.com/v1/rates/"))
  irates = matrix(as.numeric(c(rates$USD_EUR, rates$EUR_BTC, rates$BTC_USD, 
                               rates$USD_JPY, rates$JPY_BTC, rates$BTC_USD, 
                               rates$USD_BTC, rates$BTC_EUR, rates$EUR_USD, 
                               rates$USD_BTC, rates$BTC_JPY, rates$JPY_USD)), 
                  ncol=3,byrow=TRUE)
  irates_l = matrix(as.numeric(c(rates$USD_EUR, rates$EUR_BTC, rates$BTC_JPY, rates$JPY_USD,
                                 rates$USD_EUR, rates$EUR_JPY, rates$JPY_BTC, rates$BTC_USD,
                                 rates$USD_JPY, rates$JPY_EUR, rates$EUR_BTC, rates$BTC_USD,
                                 rates$USD_JPY, rates$JPY_BTC, rates$BTC_EUR, rates$EUR_USD,
                                 rates$USD_BTC, rates$BTC_EUR, rates$EUR_JPY, rates$JPY_USD,
                                 rates$USD_BTC, rates$BTC_JPY, rates$JPY_EUR, rates$EUR_USD)), 
                    ncol=4,byrow=TRUE)
  findit = matrix(100*irates[,1]*irates[,2]*irates[,3], nrow=4)
  findit_l = matrix(100*irates_l[,1]*irates_l[,2]*irates_l[,3]*irates_l[,4], nrow=6)
  final = matrix(c(findit,findit_l), nrow=10, ncol=1, byrow=TRUE)
  #print(final)
  whichtrade = english[which.max(final)]
  profit = final[which.max(final)] - 100
  winnings = winnings + profit
  print(c(date(), whichtrade, profit, winnings))
  Sys.sleep(1)
}