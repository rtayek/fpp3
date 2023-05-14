library(fpp2)
library(fpp3)

my_dcmp_spec <- decomposition_model(
  STL(Barrels),
  ETS(season_adjust ~ season("N"))
)
us_gasoline |>
  model(stl_ets = my_dcmp_spec) |>
  forecast(h = "2 years") |>
  autoplot(us_gasoline) +
  labs(y = "Millions of barrels per day",
       title = "Weekly US gasoline production")

gas_dhr <- us_gasoline |>
    model(dhr = ARIMA(Barrels ~ PDQ(0, 0, 0) + fourier(K = 6)))

# The STL approach is preferable when the seasonality changes over time.
# The dynamic harmonic regression approach is preferable if there are covariates that are useful predictors as these can be added as additional regressors.

#https://otexts.com/fpp3/weekly.html

gas_dhr |>
    forecast(h = "2 years") |>
    autoplot(us_gasoline) +
    labs(y = "Millions of barrels per day",
         title = "Weekly US gasoline production")


auscafe <- aus_retail |>
    filter(stringr::str_detect(Industry, "Takeaway")) |>
    summarise(Turnover = sum(Turnover))
train <- auscafe |>
    filter(year(Month) <= 2013)
STLF <- decomposition_model(
    STL(log(Turnover) ~ season(window = Inf)),
    ETS(season_adjust ~ season("N"))
)
cafe_models <- train |>
    model(
        ets = ETS(Turnover),
        stlf = STLF,
        arima = ARIMA(log(Turnover))
    ) |>
    mutate(combination = (ets + stlf + arima) / 3)
cafe_fc <- cafe_models |>
    forecast(h = "5 years")

cafe_fc |>
    autoplot(auscafe |> filter(year(Month) > 2008),
             level = NULL) +
    labs(y = "$ billion",
         title = "Australian monthly expenditure on eating out")

cafe_fc |>
    accuracy(auscafe) |>
    arrange(RMSE)

cafe_fc |> filter(Month == min(Month))

cafe_futures <- cafe_models |>
    # Generate 1000 future sample paths
    generate(h = "5 years", times = 1000) |>
    # Compute forecast distributions from future sample paths
    as_tibble() |>
    group_by(Month, .model) |>
    summarise(
        dist = distributional::dist_sample(list(.sim))
    ) |>
    ungroup() |>
    # Create fable object
    as_fable(index = Month, key = .model,
             distribution = dist, response = "Turnover")

cafe_futures |>
    filter(.model == "combination") |>
    autoplot(auscafe |> filter(year(Month) > 2008)) +
    labs(y = "$ billion",
         title = "Australian monthly expenditure on eating out")

cafe_futures |>
    accuracy(auscafe, measures = interval_accuracy_measures,
             level = 95) |>
    arrange(winkler)

auscafe <- aus_retail |>
    filter(stringr::str_detect(Industry, "Takeaway")) |>
    summarise(Turnover = sum(Turnover))
train <- auscafe |>
    filter(year(Month) <= 2013)
STLF <- decomposition_model(
    STL(log(Turnover) ~ season(window = Inf)),
    ETS(season_adjust ~ season("N"))
)
cafe_models <- train |>
    model(
        ets = ETS(Turnover),
        stlf = STLF,
        arima = ARIMA(log(Turnover))
    ) |>
    mutate(combination = (ets + stlf + arima) / 3)
cafe_fc <- cafe_models |>
    forecast(h = "5 years")

cafe_fc |>
    autoplot(auscafe |> filter(year(Month) > 2008),
             level = NULL) +
    labs(y = "$ billion",
         title = "Australian monthly expenditure on eating out")

cafe_fc |>
    accuracy(auscafe) |>
    arrange(RMSE)

cafe_futures <- cafe_models |>
    # Generate 1000 future sample paths
    generate(h = "5 years", times = 1000) |>
    # Compute forecast distributions from future sample paths
    as_tibble() |>
    group_by(Month, .model) |>
    summarise(
        dist = distributional::dist_sample(list(.sim))
    ) |>
    ungroup() |>
    # Create fable object
    as_fable(index = Month, key = .model,
             distribution = dist, response = "Turnover")

cafe_futures |> filter(Month == min(Month))
cafe_futures |>
    filter(.model == "combination") |>
    autoplot(auscafe |> filter(year(Month) > 2008)) +
    labs(y = "$ billion",
         title = "Australian monthly expenditure on eating out")

cafe_futures |>
    accuracy(auscafe, measures = interval_accuracy_measures,
             level = 95) |>
    arrange(winkler)
