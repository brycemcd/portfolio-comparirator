source('common_analysis.R')

indexes <- c('^GSPC', '^DJI', '^IXIC')
index <- new.env()
getSymbols(indexes, env=index)

sandp <- periodReturn(index$GSPC, period='quarterly', subset="2013::")
djia <- periodReturn(index$DJI, period='quarterly', subset="2013::")
nasdaq <- periodReturn(index$IXIC, period='quarterly', subset="2013::")

# load up indexes
sandp.df <- cleanAndConvertToDf(sandp)
djia.df <- cleanAndConvertToDf(djia)
nasdaq.df <- cleanAndConvertToDf(nasdaq)