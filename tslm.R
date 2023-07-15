rm(list = ls())
require(fpp3)
goog<-gafa_stock|>filter(Symbol=="GOOG",year(Date)==2018)
close<-goog|>select(Close) # just get close
sum(is.na(close)) # no na's
min(close$Close) # min is positive
#fitClose <- close |> model(TSLM(Close ~ trend() + season())) # complains
# Warning message:
# 1 error encountered for TSLM(Close ~ trend() + season())
# [1] argument must be coercible to non-negative integer
fitClose <- model(close,TSLM(Close ~ trend() + season())) # complains
c<-close |> mutate(Close=as.integer(Close)) # convert to integer
#autoplot(c)
#close|>mutate(Close = Close / first(Close))
fit <- c |> model(TSLM(Close ~ trend() + season())) # still complains
report(fit)

# https://stackoverflow.com/questions/76449136/how-to-fix-tslm-warning-argument-must-be-coercible-to-non-negative-integer/76453110?noredirect=1#comment134821188_76453110
gafa_stock |> 
    filter(Symbol=="GOOG", year(Date)==2018) |> 
    as_tsibble(index = Date, regular = TRUE) |> 
    model(TSLM(Close ~ trend() + season())) |> 
    report()
