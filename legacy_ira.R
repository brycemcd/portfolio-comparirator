legacy_ira <- c('LEXCX',
                'LGILX',
                'PASPX',
                'PCEMX',
                'PCGLX',
                'PCGTX',
                'PCIEX',
                'PCIFX',
                'PCLCX',
                'PCLVX',
                'PCSGX',
                'PCSIX',
                'PCSVX',
                'PREQX',
                'UMBWX')

ira <- new.env()
getSymbols(legacy_ira, env=ira)
ira.qtr <- eapply(ira, function(symbol){
  periodReturn(symbol, period='quarterly', subset="2013::")
})
ira.df <- as.data.frame(ira.qtr)
names(ira.df) <- names(ira.qtr)
ira.df <- t(ira.df)
ira.df <- as.data.frame(ira.df)
ira.df$avg <- apply(ira.df, 1, function(x) mean(x[1:10]))
ira.df$med <- apply(ira.df, 1, function(x) median(x[1:10]))
ira.df$stddev <- apply(ira.df, 1, function(x) sd(x[1:10]))
ira.df$tot <- rowSums(ira.df[1:10])
ira.df$ticker <- names(ira.qtr)

View(ira.df)

# How often did these beat the S+P?
mat <- sapply(seq(1:10), function(x) ira.df[, x] > gspc[, x])

mat <- as.data.frame(mat) # each column is a date period
rowSums(mat) # how many times as each ticker beat the S+P

# By how much did these tickers beat the S+P (S+P had positive returns each quarter in this period)
mag <- sapply(seq(1:10), function(x) ira.df[, x] - gspc[, x])
mag <- as.data.frame(mag) # each column is a date period
rowSums(mag) # how many times as each ticker beat the S+P
colSums(mag) # did the portfolio do okay in each time period?


## Time based inquiry:

# Get the return of $1 over each time period:
startP <- c('2013-03-28', '2013-06-28' , '2013-09-30', '2013-12-31',
            '2014-03-31', '2014-06-30', '2014-09-30', '2014-12-31',
            '2015-03-31', '2015-06-16')
endP   <- '2015-06-19'

# NOTE: 4 is the Close price
ira.period_return <- eapply(ira, function(symbol){
  (as.double(symbol[endP, 4]) - as.double(symbol[startP, 4])) / as.double(symbol[startP, 4])
})
# This makes no sense :/
ira.period_return <- as.data.frame(t(as.data.frame(ira.period_return)))
names(ira.period_return) <- startP
ira.period_return

# Get the return of $1 over each time period for S+P:
sandp.period_return <- (as.double(index$GSPC[endP, 4]) - as.double(index$GSPC[startP, 4])) / as.double(index$GSPC[startP, 4])

# Would I have been better off putting $ in an index?
# NOTE this only works because each return in the sandp was nonnegative
ira_to_sanp_comp <- t(apply(ira.period_return, 1, function(row) row - sandp.period_return))

summary(rowSums(ira_to_sanp_comp) > 0)
summary(ira_to_sanp_comp > 0)
