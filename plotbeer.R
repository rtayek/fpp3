require(fpp3)
beer <- aus_production |> filter(year(Quarter)>=1992)
train <- beer |> filter(year(Quarter)>=2007)
fit<- train |>  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    Seasonal_naive=SNAIVE(Beer),
    Drift = NAIVE(Beer ~ drift())
)
accuracy(fit)
fc<-fit |> forecast(h=10)
fc |> autoplot()
