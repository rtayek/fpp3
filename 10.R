rm(list=ls()) 
library(fpp3)
# Chapter 10 Dynamic regression models
# 10.1 estimation
# 10.2 Regression with ARIMA errors using fable

# Example: US Personal Consumption and Income

us_change |>
    pivot_longer(c(Consumption, Income),
                 names_to = "var", values_to = "value") |>
    ggplot(aes(x = Quarter, y = value)) +
    geom_line() +
    facet_grid(vars(var), scales = "free_y") +
    labs(title = "US consumption and personal income",
         y = "Quarterly % change")


fit <- us_change |>
    model(ARIMA(Consumption ~ Income)) # fable will pick pdq
report(fit)

# aic between models with different levels of differencing is not comparable!

#Series: Consumption 
#Model: LM w/ ARIMA(1,0,2) errors 
#
#Coefficients:
#    ar1      ma1     ma2  Income  intercept
# 0.7070  -0.6172  0.2066  0.1976     0.5949
# s.e.  0.1068   0.1218  0.0741  0.0462     0.0850
#
# sigma^2 estimated as 0.3113:  log likelihood=-163.04
# AIC=338.07   AICc=338.51   BIC=357.8

# we g3t Yt=0.5949+0.1976*Xt
# and (1-0.7070)Yt=(1-0.6172B+0.2066B^2)Et
# and Et is N(0,0.3113) # has variance, iirc should be sd?
# The data are clearly already stationary (as we are considering percentage changes rather than raw expenditure and income), so there is no need for any differencing.

# video

# regression errors 

residuals(fit,type="regression")|>
    gg_tsdisplay(.resid,plot_type="partial")+
    labs(title="Regression errors") # has a few sig acf and pacf!
# above does not look random!
# the ACF and PACF above can help us  choose order of AR and MA.
# Or we can let fable do this for us.

# innovation errors 

residuals(fit,type="innovation")|>
    gg_tsdisplay(.resid,plot_type="partial")+
    labs(title="ARIMA errors")
# arima erors do not seem to have any sig spiked in the ACFs.

augment(fit) |>
    features(.innov, ljung_box, dof = 3, lag = 12)

# text

# We can recover estimates of both the  ηt and  εtseries using the residuals() function.

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

fit |> gg_tsresiduals() # innov (est arima) looks like white noise.

# 10.3 Forecasting

us_change_future <- new_data(us_change, 8) |>
    mutate(Income = mean(us_change$Income))
forecast(fit, new_data = us_change_future) |>
    autoplot(us_change) +
    labs(y = "Percentage change")

# daily

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

report(fit)

augment(fit) |>
    features(.innov, ljung_box, dof = 3, lag = 12)

# again with stepwise=F
gc()
fit <- vic_elec_daily |>
    model(arima=ARIMA(
            Demand ~ Temperature + I(Temperature^2) +
            (Day_Type == "Weekday"),
         stepwise=FALSE,
         order_constraint=(p+q<=8 & P+Q<=5)) # reached elapsed time limit
         #order_constraint=(p+q<=4 & P+Q<=3)) # works fine
)

fit |> gg_tsresiduals()

report(fit)

augment(fit) |>
    features(.innov, ljung_box, dof = 6, lag = 14)

vic_elec_future <- new_data(vic_elec_daily, 14) |>
    mutate(
        Temperature = 26,
        Holiday = c(TRUE, rep(FALSE, 13)),
        Day_Type = case_when(
            Holiday ~ "Holiday",
            wday(Date) %in% 2:6 ~ "Weekday",
            TRUE ~ "Weekend"
        )
    )
forecast(fit, vic_elec_future) |>
    autoplot(vic_elec_daily) +
    labs(title="Daily electricity demand: Victoria",
         y="GW")

# 10.4 Stochastic and deterministic trends

aus_airpassengers |>
    autoplot(Passengers) +
    labs(y = "Passengers (millions)",
         title = "Total annual air passengers")

fit_deterministic <- aus_airpassengers |>
    model(deterministic = ARIMA(Passengers ~ 1 + trend() +
                                    pdq(d = 0)))
report(fit_deterministic)
#> Series: Passengers 
#> Model: LM w/ ARIMA(1,0,0) errors 
#> 
#> Coefficients:
#>          ar1  trend()  intercept
#>       0.9564   1.4151     0.9014
#> s.e.  0.0362   0.1972     7.0751
#> 
#> sigma^2 estimated as 4.343:  log likelihood=-100.88
#> AIC=209.77   AICc=210.72   BIC=217.17
#> 
#>

fit_stochastic <- aus_airpassengers |>
model(stochastic = ARIMA(Passengers ~ pdq(d = 1)))
report(fit_stochastic)
#> Series: Passengers 
#> Model: ARIMA(0,1,0) w/ drift 
#> 
#> Coefficients:
#>       constant
#>         1.4191
#> s.e.    0.3014
#> 
#> sigma^2 estimated as 4.271:  log likelihood=-98.16
#> AIC=200.31   AICc=200.59   BIC=203.97
#> 
#> 
aus_airpassengers |>
    autoplot(Passengers) +
    autolayer(fit_stochastic |> forecast(h = 20),
              colour = "#0072B2", level = 95) +
    autolayer(fit_deterministic |> forecast(h = 20),
              colour = "#D55E00", alpha = 0.65, level = 95) +
    labs(y = "Air passengers (millions)",
         title = "Forecasts from trend models")

# 10.5 Dynamic harmonic regression
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

# 10.6 Lagged predictors

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

glance(fit)
#> # A tibble: 4 × 8
#>   .model sigma2 log_lik   AIC  AICc   BIC ar_roots  ma_roots 
#>   <chr>   <dbl>   <dbl> <dbl> <dbl> <dbl> <list>    <list>   
#> 1 lag0    0.265   -28.3  66.6  68.3  75.0 <cpl [2]> <cpl [0]>
#> 2 lag1    0.209   -24.0  58.1  59.9  66.5 <cpl [1]> <cpl [1]>
#> 3 lag2    0.215   -24.0  60.0  62.6  70.2 <cpl [1]> <cpl [1]>
#> 4 lag3    0.206   -22.2  60.3  65.0  73.8 <cpl [1]> <cpl [1]>

fit_best <- insurance |>
  model(ARIMA(Quotes ~ pdq(d = 0) +
              TVadverts + lag(TVadverts)))
report(fit_best)
#> Series: Quotes 
#> Model: LM w/ ARIMA(1,0,2) errors 
#> 
#> Coefficients:
#>          ar1     ma1     ma2  TVadverts  lag(TVadverts)  intercept
#>
#>       0.5123  0.9169  0.4591     1.2527          0.1464     2.1554
#> s.e.  0.1849  0.2051  0.1895     0.0588          0.0531     0.8595
#> 
#> sigma^2 estimated as 0.2166:  log likelihood=-23.94
#> AIC=61.88   AICc=65.38   BIC=73.7 

insurance_future <- new_data(insurance, 20) |>
    mutate(TVadverts = 8)
fit_best |>
    forecast(insurance_future) |>
    autoplot(insurance) +
    labs(
        y = "Quotes",
        title = "Forecast quotes with future advertising set to 8"
    )

# 