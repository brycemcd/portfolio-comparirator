# Compare Stock Portfolio

> Compare stocks in one portfolio to stocks in another portfolio - or - keep
> your broker honest

## What

This series of R scripts is meant to compare a portfolio of stocks to another
portfolio of stocks (like indexes). The design is meant to abstract common
patterns of comparison to easily check the returns of a portfolio over a period
of time and compare them to another portfolio

## How To Use

> Since 2013, how many times have my biotech stocks beat the S+P 500?

1) Create a file in `portfolios` that describes the portfolio (i.e. biotech.R)

2) contents of portfolios/biotech.R - create vector of symbols

```R
source('common_analysis.R')
biotech_symbols <- c(
  'KYTH',
  'RARE',
  'HALO',
  'IMGN',
  'ANAC'
)
```
3) Create an environment to store the historical performance of these symbols (this is best practice in quantmod)

```R
biotech <- new.env()
getSymbols(midterm_symbols, env=biotech)
```

4) Capture the historical returns of each symbol

```R
bioPeriod <- getReturn(biotech)
midPeriod.df <- cleanAndConvertToDf(midPeriod)
View(midPeriod.df)
```

5) Do analysis and compare the returns of the new environment with another fund

```R
qual <- qualComp(midPeriod.df, sandp.df)
quant <- quantComp(midPeriod.df, sandp.df)

rowSums(qual) # how many times as each ticker beat the S+P
rowSums(quant) # by how much did they beat or lose?
rowSums(quant) > 0 # do the wins outrank the losses?

summary(rowSums(qual))
```