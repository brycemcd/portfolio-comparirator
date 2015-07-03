gs <- getSymbols("^GSPC", env=index)
sandp2y <- periodReturn(index$GSPC, period='quarterly', subset="2013::")
