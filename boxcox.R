require(fpp3)
require(forecast)
lambda <- BoxCox.lambda(AirPassengers,lower=0)
air.fit <- Arima(AirPassengers, order=c(0,1,1),
                 seasonal=list(order=c(0,1,1),period=12), lambda=lambda)
plot(forecast(air.fit))
