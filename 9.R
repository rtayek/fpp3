require(fpp3)
# Chapter 9 ARIMA models
# 9.1 Stationarity and differencing
# starionary === distribution of yt does not depend on t
# staionary ==> roughly horizonyal (no tred)
# constant variance
# no patterns predictablle in the long term (no seasonality)
# tnasforms help to stabvilize the variance
# for ARIMA modeling, we need to stabilize the mean.
# differncing makes staionary
# while revering everythin is called "integrating".
# stationary ==> acf decays relativley quickly
# non stationary ==> act decreases slowly
# non stationary ==> r1 us usually large and positive

google_2018<-gafa_stock|>filter(Symbol=="GOOG",year(Date)==2018)
google_2018|>ACF(Close)|>autoplot()
google_2018|>autoplot(difference(Close))
google_2018|>ACF(difference(Close))|>autoplot()

# take the sasonal difference first. this might be all we need.
# he clains that taking 3 differences for yearly data 
# results in a model that can not be sensible interpreted.
# why not?
# investigate using 2nd and 3rd order difference whne we get to multivariate.

# unit root tests
google_2015 <- gafa_stock |> filter(Symbol=="GOOG",year(Date)>=2015) |> select(Date,Close)
google_2015 |> features(Close, unitroot_kpss)
google_2015 |>
    mutate(diff_close = difference(Close)) |>
    features(diff_close, unitroot_kpss)
google_2015 |> features(Close, unitroot_ndiffs)

aus_total_retail <- aus_retail |>
    summarise(Turnover = sum(Turnover))
aus_total_retail |>
    mutate(log_turnover = log(Turnover)) |>
    features(log_turnover, unitroot_nsdiffs)
# 1 indcare we need a diff

aus_total_retail |>
    mutate(log_turnover = difference(log(Turnover), 12)) |>
    features(log_turnover, unitroot_ndiffs)
# 1 indicated we need another diff

# withou the log

aus_total_retail |>  features(Turnover, unitroot_nsdiffs)
#d<-aus_total_retail |>  difference(Turnover,12)
#|> features(Turnover, unitroot_nsdiffs)

# 9.2 Backshift notation
# 9.3 Autoregressive models
# 9.4 Moving average models

# phi on the left, theta on the right?
# maybe not?
# pacf estimates ths phi's

# any stationary AR(p) model can be written as an MA(infinity) process.
# if -1<phi1<1

# Invertable
# with some constraints on the MA parameters (theta's)
# any invertable MA process can be written as an AR(infinity) process.

# 9.5 Non-seasonal ARIMA models

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
gg_tsresiduals(fit)

global_economy |>
    filter(Code == "EGY") |>
    ACF(Exports) |>
    autoplot()

global_economy |>
    filter(Code == "EGY") |>
    PACF(Exports) |>
    autoplot()

global_economy |>
    filter(Code == "EGY") |>
    gg_tsdisplay(plot_type = "partial")

# 9.6 Estimation and order selection

# CLS = Conditional Least Squares

# MLE's and Information Criteria (AIC,BIC ...)

# 9.7 ARIMA modelling in fable

# manual method

global_economy|>filter(Code=="CAF")|>
    gg_tsdisplay(difference(Exports),plot_type="partial")

# ACF suggests arima(0,1,3)
# PACF suggests arima(2,1,0)

# arima function in  fable

caf_fit <- global_economy |>
    filter(Code == "CAF") |>
    model(arima210 = ARIMA(Exports ~ pdq(2,1,0)),
          arima013 = ARIMA(Exports ~ pdq(0,1,3)),
          stepwise = ARIMA(Exports),
          search = ARIMA(Exports, stepwise=FALSE))

caf_fit |> pivot_longer(!Country, names_to = "Model name",
                        values_to = "Orders")
glance(caf_fit)|>arrange(AICc)|>select(.model:BIC)

caf_fit|>select(search)|>gg_tsresiduals()

# use l=K degrees of freedon where K=p+q from arima
augment(caf_fit) |>
    filter(.model=='search') |>
    features(.innov, ljung_box, lag = 10, dof = 3)
# pvalue is large ().569

caf_fit |>
    forecast(h=5) |>
    filter(.model=='search') |>
    autoplot(global_economy)
# plot inverseroots
gg_arma(caf_fit |> select(Country, search))
# all inside the unit circlse so stationary andinvertible.

# 9.8 Forecasting

# 9.9 Seasonal ARIMA models

leisure <- us_employment |>
    filter(Title == "Leisure and Hospitality",
           year(Month) > 2000) |>
    mutate(Employed = Employed/1000) |>
    select(Month, Employed)
autoplot(leisure, Employed) +
    labs(title = "US employment: leisure and hospitality",
         y="Number of people (millions)")
# try lag 12
leisure |>
    gg_tsdisplay(difference(Employed, 12),
                 plot_type='partial', lag=36) +
    labs(title="Seasonally differenced", y="")

# select d via KPSS test, and D using seasonal strength
# select p,q,P,Q and c by minimizing AICc
# use stepwise search.

# we need more, try a non seasonal difference
leisure |>
    gg_tsdisplay(difference(Employed, 12) |> difference(),
                 plot_type='partial', lag=36) +
    labs(title = "Double differenced", y="")

# we hava choices

fit <- leisure |>
    model(
        arima012011 = ARIMA(Employed ~ pdq(0,1,2) + PDQ(0,1,1)),
        arima210011 = ARIMA(Employed ~ pdq(2,1,0) + PDQ(0,1,1)),
        auto = ARIMA(Employed, stepwise = FALSE, approx = FALSE)
        #  stepwise = FALSE ==> search a larger area
        # approx=False ==> slower
    )
fit |> pivot_longer(everything(), names_to = "Model name",
                    values_to = "Orders")

glance(fit) |> arrange(AICc) |> select(.model:BIC)
# check the residuals on the auto model
fit |> select(auto) |> gg_tsresiduals(lag=36)
#only one spike around 11, so lets check
augment(fit) |>
    filter(.model == "auto") |>
    # model has 4 parameters
    # 2+1+1
    # looks like we only cound p's and q's
    features(.innov, ljung_box, lag=24, dof=4) # why 24?
# ok, let's forcast
forecast(fit, h=36) |>
    filter(.model=='auto') |>
    autoplot(leisure) +
    labs(title = "US employment: leisure and hospitality",
         y="Number of people (millions)")

# second example

h02 <- PBS |>
    filter(ATC2 == "H02") |>
    summarise(Cost = sum(Cost)/1e6)
h02|>autoplot(Cost)
h02 |>
    mutate(log(Cost)) |>
    pivot_longer(-Month) |>
    ggplot(aes(x = Month, y = value)) +
    geom_line() +
    facet_grid(name ~ ., scales = "free_y") +
    labs(y="", title="Corticosteroid drug scripts (H02)")
h02|>autoplot(log(Cost)|>difference(12))
# sort looks ok, but subjective - might beed a nother difference
h02|>gg_tsdisplay(difference(log(Cost),12),
    lag_max=36,plot_type="partial")
# this suggests
# d=0,D=1
# spike in PACF has lags at 12 and 24 --> seasonal AR(2)
# spike in PACF suggests possible non seasonal AR(3) term
# so initial candidate is ARIMA(3,0,3)(2,1,0)12

fit <- h02 |>
    model(ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2)))
fit |> gg_tsresiduals(lag_max=36)
augment(fit) |>
    features(.innov, ljung_box, lag = 36, dof = 6)
# fails the ljung-box test

# try to make table 9.2
fit2 <- h02 |>
    model(
        arima301102 = ARIMA(log(Cost) ~ pdq(3,0,1) + PDQ(1,0,2)),
        auto = ARIMA(log(Cost), stepwise = FALSE, approx = FALSE)
        #  stepwise = FALSE ==> search a larger area
        # approx=False ==> slower
    )
glance(fit2) |> arrange(AICc) |> select(.model:BIC)

# 9.10 ARIMA vs ETS

