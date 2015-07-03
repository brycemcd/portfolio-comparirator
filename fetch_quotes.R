# retreive symbols we own
library('quantmod')
symbols <- c('JORNX',
             'NASDX',
             'TRDFX',
             'TWCGX',
             'VEIEX',
             '^GSPC',
             'IBB',
             'QQQ',
             'EZM',
             'RFG',
             'EES',
             'RSP',
             'PRF',
             'VUG',
             'VTI',
             'VOO',
             'RWR',
             'XLE')
ticker <- new.env()
getSymbols(symbols, env=ticker)

qtr <- eapply(ticker, function(symbol){
  periodReturn(symbol, period='quarterly', subset="2013::")
})

# How subtract returns of each symbol from the S&P500
qtrdff <- eapply(ticker, function(symbol){
  periodReturn(symbol, period='quarterly', subset="2013::") -
  periodReturn(ticker$GSPC, period='quarterly', subset="2013::")
})

qtrdiffdf <- as.data.frame(qtrdff)
names(qtrdiffdf) <- names(qtrdff)

# summarize the difference of each symbol from the S&P500
colSums(qtrdiffdf)

eapply(ticker, function(symbol){
  chartSeries(symbol, period='quarterly', subset="2013::", theme=chartTheme('white'), TA=NULL)
})

qtrdf <- as.data.frame(qtr)
names(qtrdf) <- names(qtr)

#qtrdf <- t(qtrdf) # tickers as rows, dates as columns
qplot(y=qtrdf$JORNX) + geom_point(aes(y=qtrdf$NASDX), color='red')
