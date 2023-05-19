# 5 The forecaster’s toolbox
# 5.1 A tidy forecasting workflow
require(fpp3)
gdppc <- global_economy |>
    mutate(GDP_per_capita = GDP / Population)

gdppc |>
    filter(Country == "Sweden") |>
    autoplot(GDP_per_capita) +
    labs(y = "$US", title = "GDP per capita for Sweden")

TSLM(GDP_per_capita ~ trend())

fit <- gdppc |>
    model(trend_model = TSLM(GDP_per_capita ~ trend()))

fit

fit |> forecast(h = "3 years")

fit |>
    forecast(h = "3 years") |>
    filter(Country == "Sweden") |>
    autoplot(gdppc) +
    labs(y = "$US", title = "GDP per capita for Sweden")

# 5.2 Some simple forecasting methods

bricks <- aus_production |>
    filter_index("1970 Q1" ~ "2004 Q4") |>
    select(Bricks)

bricks |> model(MEAN(Bricks))
bricks |> model(SNAIVE(Bricks ~ lag("year")))
bricks |> model(RW(Bricks ~ drift()))

# Set training data from 1992 to 2006
train <- aus_production |>
    filter_index("1992 Q1" ~ "2006 Q4")
# Fit the models
beer_fit <- train |>
    model(
        Mean = MEAN(Beer),
        `Naïve` = NAIVE(Beer),
        `Seasonal naïve` = SNAIVE(Beer)
    )
# Generate forecasts for 14 quarters
beer_fc <- beer_fit |> forecast(h = 14)
# Plot forecasts against actual values
beer_fc |>
    autoplot(train, level = NULL) +
    autolayer(
        filter_index(aus_production, "2007 Q1" ~ .),
        colour = "black"
    ) +
    labs(
        y = "Megalitres",
        title = "Forecasts for quarterly beer production"
    ) +
    guides(colour = guide_legend(title = "Forecast"))

# Re-index based on trading days
google_stock <- gafa_stock |>
    filter(Symbol == "GOOG", year(Date) >= 2015) |>
    mutate(day = row_number()) |>
    update_tsibble(index = day, regular = TRUE)
# Filter the year of interest
google_2015 <- google_stock |> filter(year(Date) == 2015)
# Fit the models
google_fit <- google_2015 |>
    model(
        Mean = MEAN(Close),
        `Naïve` = NAIVE(Close),
        Drift = NAIVE(Close ~ drift())
    )
# Produce forecasts for the trading days in January 2016
google_jan_2016 <- google_stock |>
    filter(yearmonth(Date) == yearmonth("2016 Jan"))
google_fc <- google_fit |>
    forecast(new_data = google_jan_2016)
# Plot the forecasts
google_fc |>
    autoplot(google_2015, level = NULL) +
    autolayer(google_jan_2016, Close, colour = "black") +
    labs(y = "$US",
         title = "Google daily closing stock prices",
         subtitle = "(Jan 2015 - Jan 2016)") +
    guides(colour = guide_legend(title = "Forecast"))

# 5.3 Fitted values and residuals

augment(beer_fit)

# 5.4 Residual diagnostics

autoplot(google_2015, Close) +
    labs(y = "$US",
         title = "Google daily closing stock prices in 2015")

fit<-google_2015 |> model(NAIVE(Close))
augment(fit)

aug <- google_2015 |>
    model(NAIVE(Close)) |>
    augment()
autoplot(aug, .innov) +
    labs(y = "$US",
         title = "Residuals from the naïve method")

aug |>
    ggplot(aes(x = .innov)) +
    geom_histogram() +
    labs(title = "Histogram of residuals")

aug |>
    ACF(.innov) |>
    autoplot() +
    labs(title = "Residuals from the naïve method")

gg_tsresiduals(fit,lag_max =200) # naive,  no augment

# portmaneau - box pierce?
# ljung-box test l=10 more non seasonal otherwisre 2*period

aug |> features(.innov, box_pierce, lag = 10)

aug |> features(.innov, ljung_box, lag = 10)

fit <- google_2015 |> model(RW(Close ~ drift()))
tidy(fit)

augment(fit) |> features(.innov, ljung_box, lag=10)

# 5.5 Distributional forecasts and prediction intervals

# video

aus_production |> filter(!is.na(Bricks)) |>
    model(seasonal_naive=SNAIVE(Bricks)) |>
    forecast(h="5 years")

aus_production |> filter(!is.na(Bricks)) |>
    model(seasonal_naive=SNAIVE(Bricks)) |>
    forecast(h="5 years") |>
    hilo(level=95)
    # mutata(...)

#book

google_2015 |>
    model(NAIVE(Close)) |>
    forecast(h = 10) |>
    hilo()

google_2015 |>
    model(NAIVE(Close)) |>
    forecast(h = 10) |>
    autoplot(google_2015) +
    labs(title="Google daily closing stock price", y="$US" )

fit <- google_2015 |>
    model(NAIVE(Close))
sim <- fit |> generate(h = 30, times = 5, bootstrap = TRUE)
sim

google_2015 |>
    ggplot(aes(x = day)) +
    geom_line(aes(y = Close)) +
    geom_line(aes(y = .sim, colour = as.factor(.rep)),
              data = sim) +
    labs(title="Google daily closing stock price", y="$US" ) +
    guides(colour = "none")

# 5.6 Forecasting using transformations

prices |>
    filter(!is.na(eggs)) |>
    model(RW(log(eggs) ~ drift())) |>
    forecast(h = 50) |>
    autoplot(prices |> filter(!is.na(eggs)),
             level = 80, point_forecast = lst(mean, median)
    ) +
    labs(title = "Annual egg prices",
         y = "$US (in cents adjusted for inflation) ")

eggs<-prices |> filter(!is.na(eggs))
fit<-eggs|>model(RW(log(eggs)~drift()))
fit
fc<-fit|>forecast(h=50)
fc
fc|>autoplot(eggs)+labs(y="$")
# back tranfors?
fc|>autoplot(eggs,
         level = 80, point_forecast = lst(mean, median)
) +
    labs(title = "xxxAnnual egg prices",
         y = "xxx$US (in cents adjusted for inflation) ")

# 5.7 Forecasting with decomposition

us_retail_employment <- us_employment |>
    filter(year(Month) >= 1990, Title == "Retail Trade")
#model(STL(Employed)) |>
dcmp <- us_retail_employment |>
    model(STL(Employed ~ trend(window = 7), robust = TRUE)) |>
    components() |>
    select(-.model)
dcmp
dcmp |>
    model(NAIVE(season_adjust)) |>
    forecast() |>
    autoplot(dcmp) +
    labs(y = "Number of people",
         title = "US retail employment")

fit_dcmp <- us_retail_employment |>
    model(stlf = decomposition_model(
        STL(Employed ~ trend(window = 7), robust = TRUE),
        NAIVE(season_adjust)
    ))
fit_dcmp |>
    forecast() |>
    autoplot(us_retail_employment)+
    labs(y = "Number of people",
         title = "US retail employment")


fit_dcmp |> gg_tsresiduals()

# 5.8 Evaluating point forecast accuracy

aus_production |> filter(year(Quarter) >= 1995)

aus_production |> filter_index("1995 Q1" ~ .)

aus_production |>  slice(n()-19:0) # last 20 observations

aus_retail |>  group_by(State, Industry) |>  slice(1:12)

# books says mae and mse are scale independent.
# and that mape is not.
# what if we standardized so starting stokc price is 1?
# or if diff lag?

# scaled erroe: mase

beer_production <- aus_production |>
    filter(year(Quarter)>=1992)
train <- beer_production |>
    filter(year(Quarter)>=2007)
beer_fit<- train |>  model(
        Mean = MEAN(Beer),
        `Naïve` = NAIVE(Beer),
        Seasonal_naive=SNAIVE(Beer),
        Drift = NAIVE(Beer ~ drift())
    )
beer_fc<-beer_fit |> forecast(h=10)
beer_fc |> autoplot()
names(beer_fc)
# how to plot this?

# Evaluating distributional forecast accuracy (as opposed to just a point)

# guy says stock prices are not seasonal!

google_fc |>
    filter(.model == "Naïve") |>
    autoplot(bind_rows(google_2015, google_jan_2016), level=80)+
    labs(y = "$US",
         title = "Google closing stock prices")

google_fc |>
    filter(.model == "Naïve", Date == "2016-01-04") |>
    accuracy(google_stock, list(qs=quantile_score), probs=0.10)
#> # A tibble: 1 × 4
#>   .model Symbol .type    qs
#>   <chr>  <chr>  <chr> <dbl>
#> 1 Naïve  GOOG   Test   4.86
#> 

google_fc |>
filter(.model == "Naïve", Date == "2016-01-04") |>
    accuracy(google_stock,
             list(winkler = winkler_score), level = 80)
#> # A tibble: 1 × 4
#>   .model Symbol .type winkler
#>   <chr>  <chr>  <chr>   <dbl>
#> 1 Naïve  GOOG   Test     55.7
#> 

google_fc |>
    accuracy(google_stock, list(crps = CRPS))
#> # A tibble: 3 × 4
#>   .model Symbol .type  crps
#>   <chr>  <chr>  <chr> <dbl>
#> 1 Drift  GOOG   Test   33.5
#> 2 Mean   GOOG   Test   76.7
#> 3 Naïve  GOOG   Test   26.5
#> 

google_fc |>
    accuracy(google_stock, list(skill = skill_score(CRPS)))
# any function can be inside the skill sore
#> # A tibble: 3 × 4
#>   .model Symbol .type  skill
#>   <chr>  <chr>  <chr>  <dbl>
#> 1 Drift  GOOG   Test  -0.266
#> 2 Mean   GOOG   Test  -1.90 
#> 3 Naïve  GOOG   Test   0

#  Time series cross-validation

# Time series cross-validation accuracy
google_2015_tr <- google_2015 |>
    stretch_tsibble(.init = 3, .step = 1) |>
    relocate(Date, Symbol, .id)
google_2015_tr

# TSCV accuracy
google_2015_tr |>
    model(RW(Close ~ drift())) |>
    forecast(h = 1) |>
    accuracy(google_2015)
# Training set accuracy
google_2015 |>
    model(RW(Close ~ drift())) |>
    accuracy()
