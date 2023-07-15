# Chapter 11 Forecasting hierarchical and grouped time series
# 11.1 Hierarchical and grouped time series

tourism <- tsibble::tourism |>
    mutate(State = recode(State,
                          `New South Wales` = "NSW",
                          `Northern Territory` = "NT",
                          `Queensland` = "QLD",
                          `South Australia` = "SA",
                          `Tasmania` = "TAS",
                          `Victoria` = "VIC",
                          `Western Australia` = "WA"
    ))

tourism_hts <- tourism |>
    aggregate_key(State / Region, Trips = sum(Trips))
tourism_hts

tourism_hts |>
    filter(is_aggregated(Region)) |>
    autoplot(Trips) +
    labs(y = "Trips ('000)",
         title = "Australian tourism: national and states") +
    facet_wrap(vars(State), scales = "free_y", ncol = 3) +
    theme(legend.position = "none")

# 11.2 Single level approaches

tourism_states <- tourism |>
    aggregate_key(State, Trips = sum(Trips))

fcasts_state <- tourism_states |>
    filter(!is_aggregated(State)) |>
    model(ets = ETS(Trips)) |>
    forecast()

# Sum bottom-level forecasts to get top-level forecasts
fcasts_national <- fcasts_state |>
    summarise(value = sum(Trips), .mean = mean(value))

tourism_states |>
    model(ets = ETS(Trips)) |>
    reconcile(bu = bottom_up(ets)) |>
    forecast()

# workflow for forecasting aggregate structures

data |> aggregate_key() |> model() |>
    reconcile() |> forecast()

# top down

# middle out

# 11.3 Forecast reconciliation

# 11.4 Forecasting Australian domestic tourism

tourism_full <- tourism |>
    aggregate_key((State/Region) * Purpose, Trips = sum(Trips))

fit <- tourism_full |>
    filter(year(Quarter) <= 2015) |>
    model(base = ETS(Trips)) |>
    reconcile(
        bu = bottom_up(base),
        ols = min_trace(base, method = "ols"),
        mint = min_trace(base, method = "mint_shrink")
    )
fc <- fit |> forecast(h = "2 years")

fc |>
    filter(is_aggregated(Region), is_aggregated(Purpose)) |>
    autoplot(
        tourism_full |> filter(year(Quarter) >= 2011),
        level = NULL
    ) +
    labs(y = "Trips ('000)") +
    facet_wrap(vars(State), scales = "free_y")

fc |>
    filter(is_aggregated(State), !is_aggregated(Purpose)) |>
    autoplot(
        tourism_full |> filter(year(Quarter) >= 2011),
        level = NULL
    ) +
    labs(y = "Trips ('000)") +
    facet_wrap(vars(Purpose), scales = "free_y")

fc |>
    filter(is_aggregated(State), is_aggregated(Purpose)) |>
    accuracy(
        data = tourism_full,
        measures = list(rmse = RMSE, mase = MASE)
    ) |>
    group_by(.model) |>
    summarise(rmse = mean(rmse), mase = mean(mase))

# 11.5 Reconciled distributional forecasts

# 11.6 Forecasting Australian prison population

prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv") # from chapter 2
# fails
#fit <- prison_gts |>
fit <- prison |>
    filter(year(Quarter) <= 2014) |>
    model(base = ETS(Count)) |>
    reconcile(
        bottom_up = bottom_up(base),
        MinT = min_trace(base, method = "mint_shrink")
    )
fc <- fit |> forecast(h = 8)

fc |>
    filter(is_aggregated(State), is_aggregated(Gender),
           is_aggregated(Legal)) |>
    autoplot(prison_gts, alpha = 0.7, level = 90) +
    labs(y = "Number of prisoners ('000)",
         title = "Australian prison population (total)")
