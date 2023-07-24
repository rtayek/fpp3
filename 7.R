rm(list = ls())
# Chapter 7 Time series regression models
# 7.1 the linear model
require(fpp3)

# video



# text

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
us_change |> # time series linear model
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

# 7.2 Least squares estimation

fit_consMR <- us_change |>
    model(tslm = TSLM(Consumption ~ Income + Production +
                          Unemployment + Savings))
report(fit_consMR)
a<-augment(fit_consMR)
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

# 7.3 Evaluating the regression model

fit_consMR |> gg_tsresiduals()
augment(fit_consMR) |>
    features(.innov, ljung_box, lag = 10)
#> # A tibble: 1 × 3
#>   .model lb_stat lb_pvalue
#>   <chr>    <dbl>     <dbl>
#> 1 tslm      18.9    0.0420

us_change |> # plot residuals against each predictor
    left_join(residuals(fit_consMR), by = "Quarter") |>
    pivot_longer(Income:Unemployment,
                 names_to = "regressor", values_to = "x") |>
    ggplot(aes(x = x, y = .resid)) +
    geom_point() +
    facet_wrap(. ~ regressor, scales = "free_x") +
    labs(y = "Residuals", x = "")

augment(fit_consMR) |>  # plot residuals against fitted values
    ggplot(aes(x = .fitted, y = .resid)) +
    geom_point() + labs(x = "Fitted", y = "Residuals")

# spurious regression 

fit <- aus_airpassengers |>
    filter(Year <= 2011) |>
    left_join(guinea_rice, by = "Year") |>
    model(TSLM(Passengers ~ Production))
report(fit)

fit |> gg_tsresiduals()

# 7.4 Some useful predictors

recent_production <- aus_production |>
    filter(year(Quarter) >= 1992)
recent_production |>
    autoplot(Beer) +
    labs(y = "Megalitres",
         title = "Australian quarterly beer production")

fit_beer <- recent_production |> model(TSLM(Beer ~ trend() + season()))
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

fit_beer |> gg_tsresiduals()
fit_beer |> forecast() |> autoplot(recent_production)

# same model
fourier_beer <- recent_production |>
    model(TSLM(Beer ~ trend() + fourier(K = 2)))
report(fourier_beer)

aus_cafe<-aus_retail |> filter(Industry=="Cafes, restaurants and catering services",year(Month) %in% 2004:2018) |>
    summarise(Turnover=sum(Turnover))    
aus_cafe |> autoplot(Turnover)

fit<-aus_cafe |>
    model(
        K1=TSLM(log(Turnover) ~ trend() + fourier(K = 1)),
        K2=TSLM(log(Turnover) ~ trend() + fourier(K = 2)),
        K3=TSLM(log(Turnover) ~ trend() + fourier(K = 3)),
        K4=TSLM(log(Turnover) ~ trend() + fourier(K = 4)),
        K5=TSLM(log(Turnover) ~ trend() + fourier(K = 5)),
        K6=TSLM(log(Turnover) ~ trend() + fourier(K = 6))
    )

fit |> select(K1) |> gg_tsresiduals()
aMable<-fit |> select(K1) # let's see what this is?
fit |> select(K6) |> gg_tsresiduals()

# how to plot the harmonic regression?
# the stuff below does not work.

library(pryr )
otype(fit) # just says: "S3".
# ftype(fit) # function not defined as one would expect

glance(fit) |>
    select(.model,r_squared,adj_r_squared, CV, AICc, BIC)

#k1<-fit |> select(K1)
#k1 |> forecast() |> autoplot(Turnover)
#k1 |> forecast()

#library(tseries)
#adf.test(k1)

# 7.5 Selecting predictors
# R^2 correlation is not useful for selecting preditors.
# R^2 does not allow for degrees of freedom.
# adding variablestends to increase R^2 even if it is not rrelaveant.

# Akaikes information criterion
# AIC, BI (SBIC) 
# minimuing BIC is assymototically asymtequivalent to  leave some out out  cross validation.

glance(fit_consMR) |> # different from video
    select(adj_r_squared, CV, AIC, AICc, BIC)

# this is good for forecasting, but casn not be used for inference.

# backward stepwise regression.  no guarantees.


# 7.6 Forecasting with regression

# ex ante (only use knon info known in davance)
# ex post (use existing from future)

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


fit_consBest <- us_change |>
    model(
        lm = TSLM(Consumption ~ Income + Savings + Unemployment)
    )

future_scenarios <- scenarios(
    Increase = new_data(us_change, 4) |>
        mutate(Income=1, Savings=0.5, Unemployment=0),
    Decrease = new_data(us_change, 4) |>
        mutate(Income=-1, Savings=-0.5, Unemployment=0),
    names_to = "Scenario")

fc <- forecast(fit_consBest, new_data = future_scenarios)

us_change |>
    autoplot(Consumption) +
    autolayer(fc) +
    labs(title = "US consumption", y = "% change")

# 7.7 Nonlinear regression

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

fit_trends |> select(piecewise) |> gg_tsresiduals()

# correlations that are not causal can be useful
# better to find causal relationships

# 7.8 Correlation, causation and forecasting

# 7.9 Matrix formulation

# 


