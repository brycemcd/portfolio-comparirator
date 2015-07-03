indexes <- c('^GSPC', '^DJI', '^IXIC')
index <- new.env()
gs <- getSymbols(indexes, env=index)

sandp <- periodReturn(index$GSPC, period='quarterly', subset="2013::")
dji <- periodReturn(index$DJI, period='quarterly', subset="2013::")
nasdaq <- periodReturn(index$IXIC, period='quarterly', subset="2013::")
