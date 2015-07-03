library('XML')
etf = 'http://etfdb.com/screener/'
etf.table = readHTMLTable(etf, header=T,stringsAsFactors=F)

panel.hist <- function(x, ...) {
usr <- par("usr"); on.exit(par(usr))
par(usr = c(usr[1:2], 0, 1.5) )
h <- hist(x, plot = FALSE)
breaks <- h$breaks; nB <- length(breaks)
y <- h$counts; y <- y/max(y)
rect(breaks[-nB], 0, breaks[-1], y, col = "darkblue", ...)
}


etfs <- new.env()
getSymbols(etf.table$etfs$Symbol, env=etfs)

# TODO if symbol is negative, or if S&P is negative, subtraction is not a good idea
etfdff2y <- eapply(etfs, function(symbol){
  periodReturn(symbol, period='quarterly', subset="2013::") -
    periodReturn(ticker$GSPC, period='quarterly', subset="2013::")
})

etfdiff2y.df <- as.data.frame(etfdff2y)
names(etfdiff2y.df) <- names(etfdff2y)

# summarize the difference of each symbol from the S&P500
diffs.2y <- colSums(etfdiff2y.df)
qplot(y=diffs.2y)

gspc <- periodReturn(ticker$GSPC, period='quarterly', subset="2013::", leading=)
gspc <- (t(as.data.frame(gspc)))
# cases:
# s+p is negative
# symbol is negative
# s+p is positive
# symbol is positive

# SYMBOL  S+P     OUTCOME
# -2 - (-3)       1 # WRONG
# -2 - 3         -5 # WRONG
# 2 - (-1)        3 # WRONG
# 2  - 3          -1 # WRONG
etf1y <- eapply(etfs, function(symbol){
  periodReturn(symbol, period='quarterly', subset="2013::", leading=)
})

# if an ETF hasn't been around for this period of time, then do not consider
# it for inclusion in this model
etf1y <- lapply(etf1y, function(x){
  if(length(x)[1] < 10) {
    NULL
  } else {
    x
  }
})
etf1y[sapply(etf1y, is.null)] <- NULL
etf1y.df <- as.data.frame(etf1y)
names(etf1y.df) <- names(etf1y)
etf1y.df <- t(etf1y.df)
View(etf1y.df)

etf1y.mat <- sapply(seq(1:10), function(x) etf1y.df[, x] > gspc[, x])

etf1y.mat <- as.data.frame(etf1y.mat) # each column is a date period
rowSums(etf1y.mat) # how many times as each ticker beat the S+P

# By how much did these tickers beat the S+P (S+P had positive returns each quarter in this period)
etf1y.mag <- sapply(seq(1:10), function(x) etf1y.df[, x] - gspc[, x])
etf1y.mag <- as.data.frame(etf1y.mag) # each column is a date period
summary(rowSums(etf1y.mag)) # how many times as each ticker beat the S+P
qplot(x=rowSums(etf1y.mag))
etf1y.sum_returns <- rowSums(etf1y.mag)
etf1y.sum_returns_positive <- etf1y.sum_returns[etf1y.sum_returns > 0]


# find ETFs that have beat the S+P in the last two years, the last year and the
# last two quarters
warriorETFs <- (etf1y.df[, '2015-03-31'] > gspc[, '2015-03-31'] &
                etf1y.df[, '2014-12-31'] > gspc[, '2014-12-31'] &
                etf1y.df[, '2013-12-31'] > gspc[, '2013-12-31'])


e <- sapply(names(warriorETFs[warriorETFs == TRUE]), function(x) etf1y.df[x, ])
e <- t(e)
View(e)
# I happen to know that gspc is not negative
q1diff <- e[, '2015-03-31'] - gspc[, '2015-03-31']
y1diff <- e[, '2014-12-31'] - gspc[, '2014-12-31']
y2diff <- e[, '2013-12-31'] - gspc[, '2013-12-31']
y2return <- e[, '2013-12-31'] - gspc[, '2015-03-31']

diffdf <- data.frame(q1diff, y1diff, y2diff, y2return)
diffdf$avg <- apply(diffdf, 1, function(x) mean(x[1:3]))
diffdf$med <- apply(diffdf, 1, function(x) median(x[1:3]))
diffdf$stddev <- apply(diffdf, 1, function(x) sd(x[1:3]))
diffdf$tot <- rowSums(diffdf)

pairs(data=diffdf, ~ q1diff + y1diff + y2diff + y2return + stddev, cex=0.7, pch=4, upper.panel=panel.hist)
qplot(data=diffdf, x=tot, y=stddev)
# END NEW STUFF

diffs.1y <- colSums(etfdiff1y.df)
qplot(y=diffs.1y)

etfdff1q <- eapply(etfs, function(symbol){
  periodReturn(symbol, period='quarterly', subset="2015::") -
    periodReturn(ticker$GSPC, period='quarterly', subset="2015::")
})

etfdiff1q.df <- as.data.frame(etfdff1q)
names(etfdiff1q.df) <- names(etfdff1q)

diffs.1q <- colSums(etfdiff1q.df)
qplot(y=diffs.1q)

qplot(y=diffs.1y, x=diffs.1q)
qplot(y=diffs.2y, x=diffs.1q)
qplot(y=diffs.2y, x=diffs.1y)

names(sort(diffs.1y, decreasing = T))
names(sort(diffs.2y, decreasing = T))
names(sort(diffs.1q, decreasing = T))


View(l)
