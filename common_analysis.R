# common analysis

# get returns for all etfs in the env
getReturn <- function(enviro) {
  getPeriodReturn <- eapply(enviro, function(symbol){
    periodReturn(symbol, period='quarterly', subset="2013::")
  })
  getPeriodReturn
}

# if an ETF hasn't been around for this period of time, then do not consider
# it for inclusion in this model
cleanAndConvertToDf <- function(returnSummary) {
  retSum <- lapply(returnSummary, function(x){
    if(length(x)[1] < 10) {
      NULL
    } else {
      x[1:10]
    }
  })
  retSum[sapply(retSum, is.null)] <- NULL
  retSum.df <- as.data.frame(retSum)
  names(retSum.df) <- names(retSum)
  retSum.df <- t(retSum.df)
  retSum.df
}

# understand qualitatively how often an individual ticker has beat the S+P
# TODO: allow for S+P, DJIA, NASDAQ or any other index/ticker
# NOTE: seq[1:10] finds the next 10 quarters from the beginning of the analysis
qualComp <- function(compare, comparedTo) {
  matr <- sapply(seq(1:10), function(x) compare[, x] > comparedTo[, x])
  df <- as.data.frame(matr)
  names(df) <- names(compare)
  df
}

# understand quantitatively how the ticker beat the S+P
# NOTE: (S+P had positive returns each quarter in this period)
quantComp <- function(compare, comparedTo) {
  mag <- sapply(seq(1:10), function(x) compare[, x] - comparedTo[, x])
  mag <- as.data.frame(mag) # each column is a date period
  mag
}

etfPeriod <- getReturn(index)
# convert to a data frame
etfPeriod.df <- cleanAndConvertToDf(etfPeriod)
View(etfPeriod.df)

# load up S+P
sandp.df <- cleanAndConvertToDf(sandp)

qual <- qualComp(etfPeriod.df, sandp.df)
quant <- quantComp(etfPeriod.df, sandp.df)

rowSums(qual) # how many times as each ticker beat the S+P
rowSums(quant)
