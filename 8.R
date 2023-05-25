# Chapter 8 Exponential smoothing
# 8.1 Simple exponential smoothing
library(fpp3)
algeria_economy <- global_economy |>
    filter(Country == "Algeria")
algeria_economy |>
    autoplot(Exports) +
    labs(y = "% of GDP", title = "Exports: Algeria")

# Estimate parameters

# ETS exponential trend smoothing
fit <- algeria_economy |>
    model(ETS(Exports ~ error("A") + trend("N") + season("N")))
report(fit)
fc <- fit |>
    forecast(h = 5)

fc |>
    autoplot(algeria_economy) +
    geom_line(aes(y = .fitted), col="#D55E00",
              data = augment(fit)) +
    labs(y="% of GDP", title="Exports: Algeria") +
    guides(colour = "none")

components(fit) |> autoplot()

# 8.2 Methods with trend
# holts linear trend

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
    forecast(h = 1) |> accuracy(www_usage)

fit <- www_usage |>
    model(
        Damped = ETS(value ~ error("A") + trend("Ad") +
                         season("N"))
    )
# Estimated parameters:
tidy(fit)
# hangs around here. order is probably bad.
fit |>
    forecast(h = 10) |>
    autoplot(www_usage) +
    labs(x="Minute", y="Number of users",
         title = "Internet usage per minute")

# 8.3 Methods with seasonality

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

# 8.4 A taxonomy of exponential smoothing methods
# ETS error trend season
# 8.5 Innovations state space models for exponential smoothing
# 8.6 Estimation and model selection

aus_holidays <- tourism |>
    filter(Purpose == "Holiday") |>
    summarise(Trips = sum(Trips)/1e3)
fit <- aus_holidays |>
    model(ETS(Trips))
report(fit)

components(fit) |>
    autoplot() +
    labs(title = "ETS(M,N,A) components")
# errir here us rekative error
fit<-global_economy|>mutate(Pop=Population/1e6)|>model(ets=ETS(Pop))
fit|>forecast(h=5)
residuals(fit)
residuals(fit,type="response")

# response residuals 
# innovation residuals (differ between additive and multiplicative)

# 8.7 Forecasting with ETS models

fit |>
    forecast(h = 8) |>
    autoplot(aus_holidays)+
    labs(title="Australian domestic tourism",
         y="Overnight trips (millions)")

h02<-PBS|>filter(ATC2=="H02")|>
    summarize(Cost=sum(Cost))
h02|>autoplot(Cost)
h02|>model(ETS(Cost))|>report()
# fable chooses the better using smallest AICc
h02|>model(ETS(Cost~error("A")+trend("A")+season("A")))|>report()
# the above model is not as good.
h02|>model(ETS(Cost))|>forecast()|>autoplot(h02)
#combine both
require(forecast)
cc<-h02|>model(
    ETS(Cost),
    ETS(Cost~error("A")+trend("A")+season("A"))
    ) |> accuracy() # fails!
