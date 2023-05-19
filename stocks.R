# https://palomar.home.ece.ust.hk/MAFS6010R_lectures/Rsession_time_series_modeling.html#multivariate_garch_models
# https://itadviser.dev/stock-market-data-normalization-for-time-series/#:~:text=Z%2Dscore%20normalization%2C%20also%20known,a%20standard%20deviation%20of%201.&text=Where%3A,Z%2Dscore%20(normalized%20value)
rm(list = ls())
library(quantmod)
get<-function(symbols) {
    print(symbols)
    prices <- xts()
    for (j in 1:length(symbols)) prices <- cbind(prices, Ad(getSymbols(symbols[j], from = "2013-01-01", to = "2016-12-31", auto.assign = FALSE)))
    colnames(prices) <- symbols
    tclass(prices) <- "Date"
    return(prices)
}
standardize<-function(ts) { as.xts(apply(ts, 2, function(x) x / x[1])) }
symbols <- c("SPY", "TLT", "IEF")
prices <- get(symbols)
plot(prices, col = c("black", "blue", "magenta"), main = "original-prices", legend.loc = "topleft")
logreturns <- diff(log(prices))[-1]
plot(log(prices), col = c("black", "blue", "magenta"), main = "Log-prices", legend.loc = "topleft")
y<-standardize(prices)
plot(y, col = c("black", "blue", "magenta"), main = "standardized-prices", legend.loc = "topleft")
var(prices)
var(y)
