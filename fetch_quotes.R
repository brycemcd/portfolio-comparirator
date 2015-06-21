# retreive symbols we own
library('quantmod')
symbols <- c('JORNX',
             'NASDX',
             'TRDFX',
             'TWCGX',
             'VEIEX',
             '^GSPC')
ticker <- new.env()
getSymbols(symbols, env=ticker)

qtr <- eapply(ticker, function(symbol){
  periodReturn(symbol, period='quarterly', subset="2013::")
})

qtrdff <- eapply(ticker, function(symbol){
  print(as.name(symbol))
  periodReturn(symbol, period='quarterly', subset="2013::")
})

eapply(ticker, function(symbol){
  chartSeries(symbol, period='quarterly', subset="2013::", theme=chartTheme('white'), TA=NULL)
})

qtrdf <- as.data.frame(qtr)
names(qtrdf) <- names(qtr)
qtrdf <- t(qtrdf) # tickers as rows, dates as columns
qplot(y=qtrdf$JORNX) + geom_point(aes(y=qtrdf$NASDX), color='red')
