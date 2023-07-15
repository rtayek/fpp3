# Chapter 12 Advanced forecasting methods
# 12.1 Complex seasonality
library(fpp3)
bank_calls |>
    fill_gaps() |>
    autoplot(Calls) +
    labs(y = "Calls",
         title = "Five-minute call volume to bank")

calls <- bank_calls |>
    mutate(t = row_number()) |>
    update_tsibble(index = t, regular = TRUE)

calls |>
    model(
        STL(sqrt(Calls) ~ season(period = 169) +
                season(period = 5*169),
            robust = TRUE)
    ) |>
    components() |>
    autoplot() + labs(x = "Observation")

my_dcmp_spec <- decomposition_model(
    STL(sqrt(Calls) ~ season(period = 169) +
            season(period = 5*169),
        robust = TRUE),
    ETS(season_adjust ~ season("N"))
)

fc <- calls |>
    model(my_dcmp_spec) |>
    forecast(h = 5 * 169)

# Add correct time stamps to fable
fc_with_times <- bank_calls |>
    new_data(n = 7 * 24 * 60 / 5) |>
    mutate(time = format(DateTime, format = "%H:%M:%S")) |>
    filter(
        time %in% format(bank_calls$DateTime, format = "%H:%M:%S"),
        wday(DateTime, week_start = 1) <= 5
    ) |>
    mutate(t = row_number() + max(calls$t)) |>
    left_join(fc, by = "t") |>
    as_fable(response = "Calls", distribution = Calls)

# Plot results with last 3 weeks of data
fc_with_times |>
    fill_gaps() |>
    autoplot(bank_calls |> tail(14 * 169) |> fill_gaps()) +
    labs(y = "Calls",
         title = "Five-minute call volume to bank")

# Dynamic harmonic regression with multiple seasonal periods

fit <- calls |>
    model(
        dhr = ARIMA(sqrt(Calls) ~ PDQ(0, 0, 0) + pdq(d = 0) +
                        fourier(period = 169, K = 10) +
                        fourier(period = 5*169, K = 5)))
# takes a real long time

# Warning message:
#    Provided exogenous regressors are rank deficient,
#    removing regressors:
#    `fourier(period = 5 * 169, K = 5)C5_845`,
#    `fourier(period = 5 * 169, K = 5)S5_845` 

fc <- fit |> forecast(h = 5 * 169)

# Add correct time stamps to fable
fc_with_times <- bank_calls |>
    new_data(n = 7 * 24 * 60 / 5) |>
    mutate(time = format(DateTime, format = "%H:%M:%S")) |>
    filter(
        time %in% format(bank_calls$DateTime, format = "%H:%M:%S"),
        wday(DateTime, week_start = 1) <= 5
    ) |>
    mutate(t = row_number() + max(calls$t)) |>
    left_join(fc, by = "t") |>
    as_fable(response = "Calls", distribution = Calls)
# Plot results with last 3 weeks of data
fc_with_times |>
    fill_gaps() |>
    autoplot(bank_calls |> tail(14 * 169) |> fill_gaps()) +
    labs(y = "Calls",
         title = "Five-minute call volume to bank")

# Example: Electricity demand

vic_elec |>
    pivot_longer(Demand:Temperature, names_to = "Series") |>
    ggplot(aes(x = Time, y = value)) +
    geom_line() +
    facet_grid(rows = vars(Series), scales = "free_y") +
    labs(y = "")

elec <- vic_elec |>
    mutate(
        DOW = wday(Date, label = TRUE),
        Working_Day = !Holiday & !(DOW %in% c("Sat", "Sun")),
        Cooling = pmax(Temperature, 18)
    )
elec |>
    ggplot(aes(x=Temperature, y=Demand, col=Working_Day)) +
    geom_point(alpha = 0.6) +
    labs(x="Temperature (degrees Celsius)", y="Demand (MWh)")

fit <- elec |> # takes a long time
    model(
        ARIMA(Demand ~ PDQ(0, 0, 0) + pdq(d = 0) +
                  Temperature + Cooling + Working_Day +
                  fourier(period = "day", K = 10) +
                  fourier(period = "week", K = 5) +
                  fourier(period = "year", K = 3))
    )


#
    
    elec_newdata <- new_data(elec, 2*48) |>
    mutate(
        Temperature = tail(elec$Temperature, 2 * 48),
        Date = lubridate::as_date(Time),
        DOW = wday(Date, label = TRUE),
        Working_Day = (Date != "2015-01-01") &
            !(DOW %in% c("Sat", "Sun")),
        Cooling = pmax(Temperature, 18)
    )
fc <- fit |>
    forecast(new_data = elec_newdata)

fc |>
    autoplot(elec |> tail(10 * 48)) +
    labs(title="Half hourly electricity demand: Victoria",
         y = "Demand (MWh)", x = "Time [30m]")

fit |> gg_tsresiduals()

# 12.2 Prophet model

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

fc <- fit |> forecast(h = "2 years 6 months")
fc |> autoplot(cement)
fc |> accuracy(cement)

# Example: Half-hourly electricity demand

fit <- elec |>
    model(
        prophet(Demand ~ Temperature + Cooling + Working_Day +
                    season(period = "day", order = 10) +
                    season(period = "week", order = 5) +
                    season(period = "year", order = 3))
    )
fit |>
    components() |>
    autoplot()

fit |> gg_tsresiduals()

fc <- fit |>
    forecast(new_data = elec_newdata)

fc |>
    autoplot(elec |> tail(10 * 48)) +
    labs(x = "Date", y = "Demand (MWh)")

# 12.3 Vector autoregressions

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

# 12.4 Neural network models

sunspots <- sunspot.year |> as_tsibble()
fit <- sunspots |>
    model(NNETAR(sqrt(value)))
fit |>
    forecast(h = 30) |>
    autoplot(sunspots) +
    labs(x = "Year", y = "Counts", title = "Yearly sunspots")

fit |>
    generate(times = 9, h = 30) |>
    autoplot(.sim) +
    autolayer(sunspots, value) +
    theme(legend.position = "none")

# 12.5 Bootstrapping and bagging

cement <- aus_production |>
    filter(year(Quarter) >= 1988) |>
    select(Quarter, Cement)
cement_stl <- cement |>
    model(stl = STL(Cement))
cement_stl |>
    components() |>
    autoplot()

cement_stl |>
    generate(new_data = cement, times = 10,
             bootstrap_block_size = 8) |>
    autoplot(.sim) +
    autolayer(cement, Cement) +
    guides(colour = "none") +
    labs(title = "Cement production: Bootstrapped series",
         y="Tonnes ('000)")

# Bagged forecasts

sim <- cement_stl |>
    generate(new_data = cement, times = 100,
             bootstrap_block_size = 8) |>
    select(-.model, -Cement)

ets_forecasts <- sim |>
    model(ets = ETS(.sim)) |>
    forecast(h = 12)
ets_forecasts |>
    update_tsibble(key = .rep) |>
    autoplot(.mean) +
    autolayer(cement, Cement) +
    guides(colour = "none") +
    labs(title = "Cement production: bootstrapped forecasts",
         y="Tonnes ('000)")

bagged <- ets_forecasts |>
    summarise(bagged_mean = mean(.mean))
cement |>
    model(ets = ETS(Cement)) |>
    forecast(h = 12) |>
    autoplot(cement) +
    autolayer(bagged, bagged_mean, col = "#D55E00") +
    labs(title = "Cement production in Australia",
         y="Tonnes ('000)")
