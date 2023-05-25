library(fpp) # To load the data set a10
plot(a10, ylab="$ million", xlab="Year", main="Antidiabetic drug sales")
plot(log(a10), ylab="", xlab="Year", main="Log Antidiabetic drug sales")

k <- 60 # minimum data length for fitting a model
n <- length(a10)
mae1 <- mae2 <- mae3 <- matrix(NA,n-k,12)
st <- tsp(a10)[1]+(k-2)/12

for(i in 1:(n-k))
{
    xshort <- window(a10, end=st + i/12)
    xnext <- window(a10, start=st + (i+1)/12, end=st + (i+12)/12)
    fit1 <- tslm(xshort ~ trend + season, lambda=0)
    fcast1 <- forecast(fit1, h=12)
    fit2 <- Arima(xshort, order=c(3,0,1), seasonal=list(order=c(0,1,1), period=12), 
                  include.drift=TRUE, lambda=0, method="ML")
    fcast2 <- forecast(fit2, h=12)
    fit3 <- ets(xshort,model="MMM",damped=TRUE)
    fcast3 <- forecast(fit3, h=12)
    mae1[i,1:length(xnext)] <- abs(fcast1[['mean']]-xnext)
    mae2[i,1:length(xnext)] <- abs(fcast2[['mean']]-xnext)
    mae3[i,1:length(xnext)] <- abs(fcast3[['mean']]-xnext)
}

plot(1:12, colMeans(mae1,na.rm=TRUE), type="l", col=2, xlab="horizon", ylab="MAE",
     ylim=c(0.65,1.05))
lines(1:12, colMeans(mae2,na.rm=TRUE), type="l",col=3)
lines(1:12, colMeans(mae3,na.rm=TRUE), type="l",col=4)
legend("topleft",legend=c("LM","ARIMA","ETS"),col=2:4,lty=1)
