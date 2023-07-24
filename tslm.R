rm(list = ls())
require(fpp3)
goog<-gafa_stock|>filter(Symbol=="GOOG",year(Date)==2018) |>
    #mutate(trading_day=row_number()) |> # missing days
    #update_tsibble(index=trading_day,regular=TRUE)
    as_tsibble(index = Date, regular = TRUE)  
close<-goog|>select(Close) # just get close
sum(is.na(close)) # no na's
min(close$Close) # min is positive
m<-model(close,TSLM(Close ~ trend() + season()))
m[[1]]
autoplot(close)
models<-model(close,
    #tslm=TSLM(Close ~ trend() + season()),              
    #snaive=SNAIVE(Close), # complains about missing values, suggets: fill_gaps.
    naive=NAIVE(Close), # could be just RW(). # complains about missing values, suggets: fill_gaps.
    drift=RW(Close~drift()), # complains about missing values, suggets: fill_gaps.
    mean=MEAN(Close)
)

