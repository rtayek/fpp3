require(fpp3)
# tsibble
goog<-gafa_stock|>filter(Symbol=="GOOG",year(Date)==2018)
close<-goog|>select(Close)
close|>mutate(Close = Close / first(Close))
#hilo<-goog|>mutate(hilo=High-Low)
autoplot(close)

#t<-as_tsibble(goog)
#t0<-t|>select("hilo")
#names(t)
#autoplot(t,.vars=hilo)+labs(y="high-low")
#mean(t$hilo)
var(t$hilo)

