library(fpp2)
library(fpp3)
library(fable)
library(glue)
library(broom)
library(fable.prophet)
cement <- aus_production |>
    filter(year(Quarter) >= 1988)
train <- cement |>
    filter(year(Quarter) <= 2007)
fit <- train |>
    model(
        arima = ARIMA(Cement),
        ets = ETS(Cement),
        prophet = prophet(Cement ~ season(period = 4, order = 2,
                                          type = "multiplicative"))
    )

fit <- us_change |>
    model(
        aicc = VAR(vars(Consumption, Income)),
        bic = VAR(vars(Consumption, Income), ic = "bic")
    )
fit
glance(fit)
fit |>
    augment() |>
    ACF(.innov) |>
    autoplot()

fit |>
    select(aicc) |>
    forecast() |>
    autoplot(us_change |> filter(year(Quarter) > 2010))

sunspots <- sunspot.year |> as_tsibble()
fit <- sunspots |>
    model(NNETAR(sqrt(value)))
fit |>
    forecast(h = 30) |>
    autoplot(sunspots) +
    labs(x = "Year", y = "Counts", title = "Yearly sunspots")
