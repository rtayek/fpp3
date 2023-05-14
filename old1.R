# https://otexts.com/fpp3/
# Load required packages
library(fpp2)
library(fpp3)

# Plot one time series
aus_retail |>
    filter(`Series ID`=="A3349640L") |>
    autoplot(Turnover)

# Produce some forecasts
aus_retail |>
    filter(`Series ID`=="A3349640L") |>
    model(ETS(Turnover)) |>
    forecast(h = "2 years")

autoplot(a10, Cost) +
    labs(y = "$ (millions)",
         title = "Australian antidiabetic drug sales")

melsyd_economy <- ansett |>
    filter(Airports == "MEL-SYD", Class == "Economy") |>
    mutate(Passengers = Passengers/1000)
autoplot(melsyd_economy, Passengers) +
    labs(title = "Ansett airlines economy class",
         subtitle = "Melbourne-Sydney",
         y = "Passengers ('000)")

a10 |>
    gg_season(Cost, labels = "both") +
    labs(y = "$ (millions)",
         title = "Seasonal plot: Antidiabetic drug sales")

ic_elec |>
    filter(year(Time) == 2014) |>
    autoplot(Demand) +
    labs(y = "GW",
         title = "Half-hourly electricity demand: Victoria")

us_retail_employment |>
    model(
        STL(Employed ~ trend(window = 7) +
                season(window = "periodic"),
            robust = TRUE)) |>
    components() |>
    autoplot()

tourism |>
    features(Trips, list(mean = mean)) |>
    arrange(mean)
tourism |> features(Trips, quantile)

tt<-tourism |> features(Trips, feat_acf)

tourism |>
    features(Trips, feat_stl) |>
    filter(
        seasonal_strength_year == max(seasonal_strength_year)
    ) |>
    left_join(tourism, by = c("State", "Region", "Purpose"), multiple = "all") |>
    ggplot(aes(x = Quarter, y = Trips)) +
    geom_line() +
    facet_grid(vars(State, Region, Purpose))

tourism_features <- tourism |>
    features(Trips, feature_set(pkgs = "feasts"))
tourism_features 

library(glue)
tourism_features |>
    select_at(vars(contains("season"), Purpose)) |>
    mutate(
        seasonal_peak_year = seasonal_peak_year +
            4*(seasonal_peak_year==0),
        seasonal_trough_year = seasonal_trough_year +
            4*(seasonal_trough_year==0),
        seasonal_peak_year = glue("Q{seasonal_peak_year}"),
        seasonal_trough_year = glue("Q{seasonal_trough_year}"),
    ) |>
    GGally::ggpairs(mapping = aes(colour = Purpose))

library(broom)
pcs <- tourism_features |>
    select(-State, -Region, -Purpose) |>
    prcomp(scale = TRUE) |>
    augment(tourism_features)
pcs |>
    ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Purpose)) +
    geom_point() +
    theme(aspect.ratio = 1)

gdppc <- global_economy |>
    mutate(GDP_per_capita = GDP / Population)

gdppc |>
    filter(Country == "Sweden") |>
    autoplot(GDP_per_capita) +
    labs(y = "$US", title = "GDP per capita for Sweden")

TSLM(GDP_per_capita ~ trend()).

fit <- gdppc |>
    model(trend_model = TSLM(GDP_per_capita ~ trend()))
fit

fit |> forecast(h = "3 years")

fit |>
    forecast(h = "3 years") |>
    filter(Country == "Sweden") |>
    autoplot(gdppc) +
    labs(y = "$US", title = "GDP per capita for Sweden")

bricks <- aus_production |>
    filter_index("1970 Q1" ~ "2004 Q4") |>
    select(Bricks)

prices |>
    filter(!is.na(eggs)) |>
    model(RW(log(eggs) ~ drift())) |>
    forecast(h = 50) |>
    autoplot(prices |> filter(!is.na(eggs)),
             level = 80, point_forecast = lst(mean, median)
    ) +
    labs(title = "Annual egg prices",
         y = "$US (in cents adjusted for inflation) ")

google_fit <- google_2015 |>
    model(
        Mean = MEAN(Close),
        `Naïve` = NAIVE(Close),
        Drift = RW(Close ~ drift())
    )

google_fc <- google_fit |>
    forecast(google_jan_2016)

google_fc |>
    autoplot(bind_rows(google_2015, google_jan_2016),
             level = NULL) +
    labs(y = "$US",
         title = "Google closing stock prices from Jan 2015") +
    guides(colour = guide_legend(title = "Forecast"))

recent_production <- aus_production |>
    filter(year(Quarter) >= 1992)
beer_train <- recent_production |>
    filter(year(Quarter) <= 2007)

beer_fit <- beer_train |>
    model(
        Mean = MEAN(Beer),
        `Naïve` = NAIVE(Beer),
        `Seasonal naïve` = SNAIVE(Beer),
        Drift = RW(Beer ~ drift())
    )

beer_fc <- beer_fit |>
    forecast(h = 10)

beer_fc |>
    autoplot(
        aus_production |> filter(year(Quarter) >= 1992),
        level = NULL
    ) +
    labs(
        y = "Megalitres",
        title = "Forecasts for quarterly beer production"
    ) +
    guides(colour = guide_legend(title = "Forecast"))

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

google_2015_tr <- google_2015 |>
    stretch_tsibble(.init = 3, .step = 1)
fc <- google_2015_tr |>
    model(RW(Close ~ drift())) |>
    forecast(h = 8) |>
    group_by(.id) |>
    mutate(h = row_number()) |>
    ungroup() |>
    as_fable(response = "Close", distribution = Close)
fc |>
    accuracy(google_2015, by = c("h", ".model")) |>
    ggplot(aes(x = h, y = RMSE)) +
    geom_point()
    
us_change |>
    pivot_longer(c(Consumption, Income), names_to="Series") |>
    autoplot(value) +
    labs(y = "% change")

us_change |>
    ggplot(aes(x = Income, y = Consumption)) +
    labs(y = "Consumption (quarterly % change)",
         x = "Income (quarterly % change)") +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)

us_change |>
    model(TSLM(Consumption ~ Income)) |>
    report()

us_change |>
    select(-Consumption, -Income) |>
    pivot_longer(-Quarter) |>
    ggplot(aes(Quarter, value, colour = name)) +
    geom_line() +
    facet_grid(name ~ ., scales = "free_y") +
    guides(colour = "none") +
    labs(y="% change")

us_change |>
    GGally::ggpairs(columns = 2:6)

fit_consMR <- us_change |>
    model(tslm = TSLM(Consumption ~ Income + Production +
                          Unemployment + Savings))
report(fit_consMR)

augment(fit_consMR) |>
    ggplot(aes(x = Quarter)) +
    geom_line(aes(y = Consumption, colour = "Data")) +
    geom_line(aes(y = .fitted, colour = "Fitted")) +
    labs(y = NULL,
         title = "Percent change in US consumption expenditure"
    ) +
    scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
    guides(colour = guide_legend(title = NULL))

augment(fit_consMR) |>
    ggplot(aes(x = Consumption, y = .fitted)) +
    geom_point() +
    labs(
        y = "Fitted (predicted values)",
        x = "Data (actual values)",
        title = "Percent change in US consumption expenditure"
    ) +
    geom_abline(intercept = 0, slope = 1)

fit_consMR |> gg_tsresiduals()

augment(fit_consMR) |>
    features(.innov, ljung_box, lag = 10)

us_change |>
    left_join(residuals(fit_consMR), by = "Quarter") |>
    pivot_longer(Income:Unemployment,
                 names_to = "regressor", values_to = "x") |>
    ggplot(aes(x = x, y = .resid)) +
    geom_point() +
    facet_wrap(. ~ regressor, scales = "free_x") +
    labs(y = "Residuals", x = "")

augment(fit_consMR) |>
    ggplot(aes(x = .fitted, y = .resid)) +
    geom_point() + labs(x = "Fitted", y = "Residuals")

fit <- aus_airpassengers |>
    filter(Year <= 2011) |>
    left_join(guinea_rice, by = "Year") |>
    model(TSLM(Passengers ~ Production))
report(fit)

fit |> gg_tsresiduals()

recent_production <- aus_production |>
    filter(year(Quarter) >= 1992)
recent_production |>
    autoplot(Beer) +
    labs(y = "Megalitres",
         title = "Australian quarterly beer production")

fit_beer <- recent_production |>
    model(TSLM(Beer ~ trend() + season()))
report(fit_beer)

augment(fit_beer) |>
    ggplot(aes(x = Quarter)) +
    geom_line(aes(y = Beer, colour = "Data")) +
    geom_line(aes(y = .fitted, colour = "Fitted")) +
    scale_colour_manual(
        values = c(Data = "black", Fitted = "#D55E00")
    ) +
    labs(y = "Megalitres",
         title = "Australian quarterly beer production") +
    guides(colour = guide_legend(title = "Series"))

augment(fit_beer) |>
    ggplot(aes(x = Beer, y = .fitted,
               colour = factor(quarter(Quarter)))) +
    geom_point() +
    labs(y = "Fitted", x = "Actual values",
         title = "Australian quarterly beer production") +
    geom_abline(intercept = 0, slope = 1) +
    guides(colour = guide_legend(title = "Quarter"))

fourier_beer <- recent_production |>
    model(TSLM(Beer ~ trend() + fourier(K = 2)))
report(fourier_beer)

glance(fit_consMR) |>
    select(adj_r_squared, CV, AIC, AICc, BIC)

recent_production <- aus_production |>
    filter(year(Quarter) >= 1992)
fit_beer <- recent_production |>
    model(TSLM(Beer ~ trend() + season()))
fc_beer <- forecast(fit_beer)
fc_beer |>
    autoplot(recent_production) +
    labs(
        title = "Forecasts of beer production using regression",
        y = "megalitres"
    )

fit_cons <- us_change |>
    model(TSLM(Consumption ~ Income))
new_cons <- scenarios(
    "Average increase" = new_data(us_change, 4) |>
        mutate(Income = mean(us_change$Income)),
    "Extreme increase" = new_data(us_change, 4) |>
        mutate(Income = 12),
    names_to = "Scenario"
)
fcast <- forecast(fit_cons, new_cons)

us_change |>
    autoplot(Consumption) +
    autolayer(fcast) +
    labs(title = "US consumption", y = "% change")

boston_men <- boston_marathon |>
    filter(Year >= 1924) |>
    filter(Event == "Men's open division") |>
    mutate(Minutes = as.numeric(Time)/60)

fit_trends <- boston_men |>
    model(
        linear = TSLM(Minutes ~ trend()),
        exponential = TSLM(log(Minutes) ~ trend()),
        piecewise = TSLM(Minutes ~ trend(knots = c(1950, 1980)))
    )
fc_trends <- fit_trends |> forecast(h = 10)

boston_men |>
    autoplot(Minutes) +
    geom_line(data = fitted(fit_trends),
              aes(y = .fitted, colour = .model)) +
    autolayer(fc_trends, alpha = 0.5, level = 95) +
    labs(y = "Minutes",
         title = "Boston marathon winning times")

aus_economy <- global_economy |>
    filter(Code == "AUS") |>
    mutate(Pop = Population / 1e6)
autoplot(aus_economy, Pop) +
    labs(y = "Millions", title = "Australian population")

fit <- aus_economy |>
    model(
        AAN = ETS(Pop ~ error("A") + trend("A") + season("N"))
    )
fc <- fit |> forecast(h = 10)

aus_economy |>
    model(
        `Holt's method` = ETS(Pop ~ error("A") +
                                  trend("A") + season("N")),
        `Damped Holt's method` = ETS(Pop ~ error("A") +
                                         trend("Ad", phi = 0.9) + season("N"))
    ) |>
    forecast(h = 15) |>
    autoplot(aus_economy, level = NULL) +
    labs(title = "Australian population",
         y = "Millions") +
    guides(colour = guide_legend(title = "Forecast"))

www_usage <- as_tsibble(WWWusage)
www_usage |> autoplot(value) +
    labs(x="Minute", y="Number of users",
         title = "Internet usage per minute")

www_usage |>
    stretch_tsibble(.init = 10) |>
    model(
        SES = ETS(value ~ error("A") + trend("N") + season("N")),
        Holt = ETS(value ~ error("A") + trend("A") + season("N")),
        Damped = ETS(value ~ error("A") + trend("Ad") +
                         season("N"))
    ) |>
    forecast(h = 1) |>
    accuracy(www_usage)

aus_holidays <- tourism |>
    filter(Purpose == "Holiday") |>
    summarise(Trips = sum(Trips)/1e3)
fit <- aus_holidays |>
    model(
        additive = ETS(Trips ~ error("A") + trend("A") +
                           season("A")),
        multiplicative = ETS(Trips ~ error("M") + trend("A") +
                                 season("M"))
    )
fc <- fit |> forecast(h = "3 years")
fc |>
    autoplot(aus_holidays, level = NULL) +
    labs(title="Australian domestic tourism",
         y="Overnight trips (millions)") +
    guides(colour = guide_legend(title = "Forecast"))

sth_cross_ped <- pedestrian |>
    filter(Date >= "2016-07-01",
           Sensor == "Southern Cross Station") |>
    index_by(Date) |>
    summarise(Count = sum(Count)/1000)
sth_cross_ped |>
    filter(Date <= "2016-07-31") |>
    model(
        hw = ETS(Count ~ error("M") + trend("Ad") + season("M"))
    ) |>
    forecast(h = "2 weeks") |>
    autoplot(sth_cross_ped |> filter(Date <= "2016-08-14")) +
    labs(title = "Daily traffic: Southern Cross",
         y="Pedestrians ('000)")

fit |>
    forecast(h = 8) |>
    autoplot(aus_holidays)+
    labs(title="Australian domestic tourism",
         y="Overnight trips (millions)")

global_economy |>
    filter(Code == "EGY") |>
    autoplot(Exports) +
    labs(y = "% of GDP", title = "Egyptian exports")

fit <- global_economy |>
    filter(Code == "EGY") |>
    model(ARIMA(Exports))
report(fit)

fit |> forecast(h=10) |>
    autoplot(global_economy) +
    labs(y = "% of GDP", title = "Egyptian exports")

global_economy |>
    filter(Code == "EGY") |>
    ACF(Exports) |>
    autoplot()

global_economy |>
    filter(Code == "EGY") |>
    PACF(Exports) |>
    autoplot()

fit2 <- global_economy |>
    filter(Code == "EGY") |>
    model(ARIMA(Exports ~ pdq(4,0,0)))
report(fit2)

global_economy |>
    filter(Code == "CAF") |>
    autoplot(Exports) +
    labs(title="Central African Republic exports",
         y="% of GDP")

global_economy |>
    filter(Code == "CAF") |>
    gg_tsdisplay(difference(Exports), plot_type='partial')

caf_fit <- global_economy |>
    filter(Code == "CAF") |>
    model(arima210 = ARIMA(Exports ~ pdq(2,1,0)),
          arima013 = ARIMA(Exports ~ pdq(0,1,3)),
          stepwise = ARIMA(Exports),
          search = ARIMA(Exports, stepwise=FALSE))

caf_fit |> pivot_longer(!Country, names_to = "Model name",
                        values_to = "Orders")

augment(caf_fit) |>
    filter(.model=='search') |>
    features(.innov, ljung_box, lag = 10, dof = 3)

caf_fit |>
    forecast(h=5) |>
    filter(.model=='search') |>
    autoplot(global_economy)

leisure <- us_employment |>
    filter(Title == "Leisure and Hospitality",
           year(Month) > 2000) |>
    mutate(Employed = Employed/1000) |>
    select(Month, Employed)
autoplot(leisure, Employed) +
    labs(title = "US employment: leisure and hospitality",
         y="Number of people (millions)")

h02 <- PBS |>
    filter(ATC2 == "H02") |>
    summarise(Cost = sum(Cost)/1e6)
h02 |>
    mutate(log(Cost)) |>
    pivot_longer(-Month) |>
    ggplot(aes(x = Month, y = value)) +
    geom_line() +
    facet_grid(name ~ ., scales = "free_y") +
    labs(y="", title="Corticosteroid drug scripts (H02)")

us_change |>
    pivot_longer(c(Consumption, Income),
                 names_to = "var", values_to = "value") |>
    ggplot(aes(x = Quarter, y = value)) +
    geom_line() +
    facet_grid(vars(var), scales = "free_y") +
    labs(title = "US consumption and personal income",
         y = "Quarterly % change")

fit <- us_change |>
    model(ARIMA(Consumption ~ Income))
report(fit)

bind_rows(
    `Regression residuals` =
        as_tibble(residuals(fit, type = "regression")),
    `ARIMA residuals` =
        as_tibble(residuals(fit, type = "innovation")),
    .id = "type"
) |>
    mutate(
        type = factor(type, levels=c(
            "Regression residuals", "ARIMA residuals"))
    ) |>
    ggplot(aes(x = Quarter, y = .resid)) +
    geom_line() +
    facet_grid(vars(type))

fit |> gg_tsresiduals()

augment(fit) |>
    features(.innov, ljung_box, dof = 3, lag = 8)

us_change_future <- new_data(us_change, 8) |>
    mutate(Income = mean(us_change$Income))
forecast(fit, new_data = us_change_future) |>
    autoplot(us_change) +
    labs(y = "Percentage change")

vic_elec_daily <- vic_elec |>
    filter(year(Time) == 2014) |>
    index_by(Date = date(Time)) |>
    summarise(
        Demand = sum(Demand) / 1e3,
        Temperature = max(Temperature),
        Holiday = any(Holiday)
    ) |>
    mutate(Day_Type = case_when(
        Holiday ~ "Holiday",
        wday(Date) %in% 2:6 ~ "Weekday",
        TRUE ~ "Weekend"
    ))
vic_elec_daily |>
    ggplot(aes(x = Temperature, y = Demand, colour = Day_Type)) +
    geom_point() +
    labs(y = "Electricity demand (GW)",
         x = "Maximum daily temperature")

vic_elec_daily |>
    pivot_longer(c(Demand, Temperature)) |>
    ggplot(aes(x = Date, y = value)) +
    geom_line() +
    facet_grid(name ~ ., scales = "free_y") + ylab("")

fit <- vic_elec_daily |>
    model(ARIMA(Demand ~ Temperature + I(Temperature^2) +
                    (Day_Type == "Weekday")))
fit |> gg_tsresiduals()

augment(fit) |>
    features(.innov, ljung_box, dof = 6, lag = 14)

aus_airpassengers |>
    autoplot(Passengers) +
    labs(y = "Passengers (millions)",
         title = "Total annual air passengers")

fit_deterministic <- aus_airpassengers |>
    model(deterministic = ARIMA(Passengers ~ 1 + trend() +
                                    pdq(d = 0)))
report(fit_deterministic)

fit_stochastic <- aus_airpassengers |>
    model(stochastic = ARIMA(Passengers ~ pdq(d = 1)))
report(fit_stochastic)

aus_airpassengers |>
    autoplot(Passengers) +
    autolayer(fit_stochastic |> forecast(h = 20),
              colour = "#0072B2", level = 95) +
    autolayer(fit_deterministic |> forecast(h = 20),
              colour = "#D55E00", alpha = 0.65, level = 95) +
    labs(y = "Air passengers (millions)",
         title = "Forecasts from trend models")

aus_cafe <- aus_retail |>
    filter(
        Industry == "Cafes, restaurants and takeaway food services",
        year(Month) %in% 2004:2018
    ) |>
    summarise(Turnover = sum(Turnover))

fit <- model(aus_cafe,
             `K = 1` = ARIMA(log(Turnover) ~ fourier(K=1) + PDQ(0,0,0)),
             `K = 2` = ARIMA(log(Turnover) ~ fourier(K=2) + PDQ(0,0,0)),
             `K = 3` = ARIMA(log(Turnover) ~ fourier(K=3) + PDQ(0,0,0)),
             `K = 4` = ARIMA(log(Turnover) ~ fourier(K=4) + PDQ(0,0,0)),
             `K = 5` = ARIMA(log(Turnover) ~ fourier(K=5) + PDQ(0,0,0)),
             `K = 6` = ARIMA(log(Turnover) ~ fourier(K=6) + PDQ(0,0,0))
)

fit |>
    forecast(h = "2 years") |>
    autoplot(aus_cafe, level = 95) +
    facet_wrap(vars(.model), ncol = 2) +
    guides(colour = "none", fill = "none", level = "none") +
    geom_label(
        aes(x = yearmonth("2007 Jan"), y = 4250,
            label = paste0("AICc = ", format(AICc))),
        data = glance(fit)
    ) +
    labs(title= "Total monthly eating-out expenditure",
         y="$ billions")

insurance |>
    pivot_longer(Quotes:TVadverts) |>
    ggplot(aes(x = Month, y = value)) +
    geom_line() +
    facet_grid(vars(name), scales = "free_y") +
    labs(y = "", title = "Insurance advertising and quotations")

fit <- insurance |>
    # Restrict data so models use same fitting period
    mutate(Quotes = c(NA, NA, NA, Quotes[4:40])) |>
    # Estimate models
    model(
        lag0 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts),
        lag1 = ARIMA(Quotes ~ pdq(d = 0) +
                         TVadverts + lag(TVadverts)),
        lag2 = ARIMA(Quotes ~ pdq(d = 0) +
                         TVadverts + lag(TVadverts) +
                         lag(TVadverts, 2)),
        lag3 = ARIMA(Quotes ~ pdq(d = 0) +
                         TVadverts + lag(TVadverts) +
                         lag(TVadverts, 2) + lag(TVadverts, 3))
    )

fit_best <- insurance |>
    model(ARIMA(Quotes ~ pdq(d = 0) +
                    TVadverts + lag(TVadverts)))
report(fit_best)

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

# Forecasts from STL+ETS decomposition
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

fit <- calls |>
    model(
        dhr = ARIMA(sqrt(Calls) ~ PDQ(0, 0, 0) + pdq(d = 0) +
                        fourier(period = 169, K = 10) +
                        fourier(period = 5*169, K = 5)))

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

fit <- elec |>
    model(
        ARIMA(Demand ~ PDQ(0, 0, 0) + pdq(d = 0) +
                  Temperature + Cooling + Working_Day +
                  fourier(period = "day", K = 10) +
                  fourier(period = "week", K = 5) +
                  fourier(period = "year", K = 3))
    )

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