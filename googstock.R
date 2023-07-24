rm(list = ls())
require(fpp3)
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
        #tslm=TSLM(Close ~ trend() + season()),              
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

