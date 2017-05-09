profvis({
for (i in 1:length(my.data)) {
  my.date = as.Date(names(my.data)[i])
  print(my.date)
}
})

profvis({
  my.date = as.Date(names(my.data))
  print(my.date)
})

profvis({
  print(names(my.data))
})

profvis({
  for (i in 1:1000) {
  if (nrow(underlying[my.date]) == 0)
    stop(paste(my.date, "not a trading day"))
  if (is.null(my.cal) || !exists("my.cal")) # should check dates too
    stop("global calendar (my.cal) does not exist or is NULL")
  FloatingProfit(my.df) >= 0.89 * (-1 * InitialCredit(my.df))
  FloatingProfit(my.df) <= 2 * InitialCredit(my.df) # negative val
  bizdays(date, as.Date(my.df[1,]$my.exp.date), my.cal) <= 5 
  as.numeric(Hi(underlying[my.date])) >= ShortCall(my.df)
  as.numeric(Lo(underlying[my.date])) <= ShortPut(my.df)
  }
})

my.date="2010-02-01"
microbenchmark(FloatingProfit(my.df), 
               InitialCredit(my.df), 
               bizdays(as.Date(my.date), as.Date(my.df[1,24]), my.cal), 
               ShortCall(my.df), 
               ShortPut(my.df), 
               as.numeric(highs[my.date]),
               as.numeric(lows[my.date]),
               times=1000)

microbenchmark(as.numeric(Hi(underlying[my.date])),
               as.numeric(Lo(underlying[my.date])),
               as.numeric(highs[my.date]),
               as.numeric(lows[my.date]),
               times=1000)

microbenchmark(ShortCall(my.df),
               ShortPut(my.df),
               times=1000)

# return the strike price of the short call in a single condor
NewShortCall = function(trades) {
  strike = trades[trades$Call.Put == "C",][1,6]
}

# return the strike price of the short put in a single condor
NewShortPut = function(trades) {
  strike = trades[trades$Call.Put == "P",][2,6]
}

# return the strike price of the short call in a single condor
SecNewShortCall = function(trades) {
  strike = min(trades[trades$Call.Put == "C",6])
}

# return the strike price of the short put in a single condor
SecNewShortPut = function(trades) {
  strike = max(trades[trades$Call.Put == "P",6])
}

TriNewShortCall = function(trades) {
  strike = min(trades[trades[,7] == "C",6])
}

# return the strike price of the short put in a single condor
TriNewShortPut = function(trades) {
  strike = max(trades[trades[,7] == "P",6])
}

microbenchmark(ShortCall(my.df),
               ShortPut(my.df),
               NewShortCall(my.df),
               NewShortPut(my.df),
               SecNewShortCall(my.df),
               SecNewShortPut(my.df),
               TriNewShortCall(my.df),
               TriNewShortPut(my.df),
               times=1000)

my.truefalse = list()
for (i in 89:length(my.data)) {
  if (!is.null(FindCondor(my.data[[i]])) &&
      nrow(FindCondor(my.data[[i]])) == 4) {
    #browser()
    my.condor = FindCondor(my.data[[i]])
    my.truefalse[[i]] = SecNewShortCall(my.condor) == ShortCall(my.condor)
  }
}

if (all(unlist(my.truefalse)) == TRUE)
  print("PASS for valid iron condors")

#profile FindCondor
profvis({
  is.list = FALSE
  my.df = my.data[[1]]
for (i in 1:1000) {
  if (is.list) {
    my.df = my.df[[1]]
  }
  # clean up to only include traditional monthlies
  # (should be checking this in data export as well)
  my.df = my.df[!grepl("D\\d{1,2}$", my.df$Symbol, perl = TRUE),]
  # clean up to only include possible candidates
  my.df = subset(my.df, cal.dte > 49 & cal.dte < 76)
  # if you have no expirations (a few days like this), return NULL
  if (nrow(my.df) == 0) {
  } else {
    # if you have two expirations, pick the minimum for now
    my.df = subset(my.df, cal.dte == min(cal.dte))
    my.df[PickByDelta(my.df$Delta,   8),]$Existing.Posn. =  1
    my.df[PickByDelta(my.df$Delta,  11),]$Existing.Posn. = -1
    my.df[PickByDelta(my.df$Delta,  -8),]$Existing.Posn. =  1
    my.df[PickByDelta(my.df$Delta, -11),]$Existing.Posn. = -1
    # Existing.Posn is NA by default, so this works but is not intuitive:
    my.open.trades = my.df[!is.na(my.df$Existing.Posn.),]
    # add new $orig.price column for later
    my.open.trades$orig.price = my.open.trades$mid.price
  }
}
})

open.trades = list(FindCondor(my.data[[100]]))
microbenchmark(FindCondor(my.data[[100]]),
               ShouldEnter(open.trades, my.data[[120]]),
               times=1000)

#profile NewFindCondor
profvis({
  is.list = FALSE
  my.df = my.data[[1]]
  for (i in 1:1000) {
    if (is.list) {
      my.df = my.df[[1]]
    }
    # clean up to only include traditional monthlies
    # (should be checking this in data export as well)
    my.df = my.df[!grepl("D\\d{1,2}$", my.df$Symbol, perl = TRUE),]
    # clean up to only include possible candidates
    my.df = subset(my.df, cal.dte > 49 & cal.dte < 76)
    # if you have no expirations (a few days like this), return NULL
    if (nrow(my.df) == 0) {
    } else {
      # if you have two expirations, pick the minimum for now
      my.df = subset(my.df, cal.dte == min(cal.dte))
      my.df[PickByDelta(my.df[,16],   8),1] =  1
      my.df[PickByDelta(my.df[,16],  11),1] = -1
      my.df[PickByDelta(my.df[,16],  -8),1] =  1
      my.df[PickByDelta(my.df[,16], -11),1] = -1
      # Existing.Posn is NA by default, so this works but is not intuitive:
      my.open.trades = my.df[!is.na(my.df[,1]),]
      # add new $orig.price column for later
      my.open.trades[,28] = my.open.trades[,27]
    }
  }
})

NewFindCondor = function(my.df, is.list = FALSE) {
  if (is.list) {
    my.df = my.df[[1]]
  }
  # clean up to only include traditional monthlies
  # (should be checking this in data export as well)
  my.df = my.df[!grepl("D\\d{1,2}$", my.df$Symbol, perl = TRUE),]
  # clean up to only include possible candidates
  my.df = subset(my.df, cal.dte > 49 & cal.dte < 76)
  # if you have no expirations (a few days like this), return NULL
  if (nrow(my.df) == 0) {
  } else {
    # if you have two expirations, pick the minimum for now
    my.df = subset(my.df, cal.dte == min(cal.dte))
    my.df[PickByDelta(my.df[,16],   8),1] =  1
    my.df[PickByDelta(my.df[,16],  11),1] = -1
    my.df[PickByDelta(my.df[,16],  -8),1] =  1
    my.df[PickByDelta(my.df[,16], -11),1] = -1
    # Existing.Posn is NA by default, so this works but is not intuitive:
    my.open.trades = my.df[!is.na(my.df[,1]),]
    # add new $orig.price column for later
    my.open.trades[,28] = my.open.trades[,27]
    return(my.open.trades)
  }
}

FindCondor = function(my.df, is.list = FALSE) {
  if (is.list) {
    my.df = my.df[[1]]
  }
  # clean up to only include traditional monthlies
  # (should be checking this in data export as well)
  my.df = my.df[!grepl("D\\d{1,2}$", my.df$Symbol, perl = TRUE),]
  # clean up to only include possible candidates
  my.df = subset(my.df, cal.dte > 49 & cal.dte < 76)
  # if you have no expirations (a few days like this), return NULL
  if (nrow(my.df) == 0) {
    return(NULL)
  } else {
    # if you have two expirations, pick the minimum for now
    my.df = subset(my.df, cal.dte == min(cal.dte))
    my.df[PickByDelta(my.df$Delta,   8),]$Existing.Posn. =  1
    my.df[PickByDelta(my.df$Delta,  11),]$Existing.Posn. = -1
    my.df[PickByDelta(my.df$Delta,  -8),]$Existing.Posn. =  1
    my.df[PickByDelta(my.df$Delta, -11),]$Existing.Posn. = -1
    # Existing.Posn is NA by default, so this works but is not intuitive:
    my.open.trades = my.df[!is.na(my.df$Existing.Posn.),]
    # add new $orig.price column for later
    my.open.trades$orig.price = my.open.trades$mid.price
    return(my.open.trades)
  }
}

foo.data = my.data[[1]]
microbenchmark(FindCondor(foo.data),
               NewFindCondor(foo.data),
               times=1000)
