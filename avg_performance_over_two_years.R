etfperf2y <- eapply(etfs, function(symbol){
  periodReturn(symbol, period='quarterly', subset="2013::")
})

etfperf2y.nulled <- lapply(etfperf2y, function(x){
  if(length(x)[1] < 10) {
    NULL
  } else {
    x
  }
})
etfperf2y.nulled[sapply(etfperf2y.nulled, is.null)] <- NULL
etfperf2y.df <- as.data.frame(etfperf2y.nulled)
names(etfperf2y.df) <- names(etfperf2y.nulled)
etfperf2y.df <- t(etfperf2y.df)
etfperf2y.df <- as.data.frame(etfperf2y.df)
class(etfperf2y.df)
View(etfperf2y.df)

etfperf2y.df$avg <- apply(etfperf2y.df, 1, function(x) mean(x))
etfperf2y.df$med <- apply(etfperf2y.df, 1, function(x) median(x))
etfperf2y.df$stddev <- apply(etfperf2y.df, 1, function(x) sd(x))
etfperf2y.df$tot <- rowSums(etfperf2y.df)

gspc <- periodReturn(ticker$GSPC, period='quarterly', subset="2013::", leading=)
gspc <- (t(as.data.frame(gspc)))
gspc <- as.data.frame(gspc)
gspc$avg <- apply(gspc, 1, function(x) mean(x[1:10]))
gspc$med <- apply(gspc, 1, function(x) median(x[1:10]))
gspc$stddev <- apply(gspc, 1, function(x) sd(x[1:10]))
gspc$tot <- rowSums(gspc[1:10])
View(gspc)

potential_winners <- subset(etfperf2y.df, med > gspc$med)
potential_winners$ticker <- row.names(potential_winners)

pairs(data=potential_winners, ~ med + stddev + tot + avg)

nrow(potential_winners)
potential_winners %>%
  select(ticker, med) %>%
  arrange(med) %>%
  top_n(20)
