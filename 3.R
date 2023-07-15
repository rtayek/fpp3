require(fpp3)

# 3 decomposition

# trend/cycle components

# 3.1 transforms and adjustments

# trend and cycles usually get combined into a trend-cycle comnponent

# box-cox log lambda (bickel digstroum)

# features guerrro

# video

# text?

global_economy |>
    filter(Country == "Australia") |>
    autoplot(GDP) +
    labs(title= "GD", y = "$US")

global_economy |>
    filter(Country == "Australia") |>
    autoplot(GDP/Population) +
    labs(title= "GDP per capita", y = "$US")

print_retail <- aus_retail |>
    filter(Industry == "Newspaper and book retailing") |>
    group_by(Industry) |>
    index_by(Year = year(Month)) |>
    summarise(Turnover = sum(Turnover))
aus_economy <- global_economy |>
    filter(Code == "AUS")

print_retail |>
    left_join(aus_economy, by = "Year") |>
    mutate(Adjusted_turnover = Turnover / CPI * 100) |>
    pivot_longer(c(Turnover, Adjusted_turnover),
                 values_to = "Turnover") |>
    mutate(name = factor(name,
                         levels=c("Turnover","Adjusted_turnover"))) |>
    ggplot(aes(x = Year, y = Turnover)) +
    geom_line() +
    facet_grid(name ~ ., scales = "free_y") +
    labs(title = "Turnover: Australian print media industry",
         y = "$AU")

require(latex2exp)
# https://en.wikipedia.org/wiki/Power_transform
lambda <- aus_production |>
    features(Gas, features = guerrero) |>
    pull(lambda_guerrero)
aus_production |>
    autoplot(box_cox(Gas, lambda)) +
    labs(y = "",
         title = latex2exp::TeX(paste0(
             "Transformed gas production with $\\lambda$ = ",
             round(lambda,2))))

# 3.2 time series components

us_retail_employment <- us_employment |>
    filter(year(Month) >= 1990, Title == "Retail Trade") |>
    select(-Series_ID)
autoplot(us_retail_employment, Employed) +
    labs(y = "Persons (thousands)",
         title = "Total employment in US retail")

dcmp <- us_retail_employment |>  model(stl = STL(Employed))
components(dcmp)

# a dable is a decompositioned model.

components(dcmp) |>
    as_tsibble() |>
    autoplot(Employed, colour="gray") +
    geom_line(aes(y=trend), colour = "#D55E00") +
    labs(
        y = "Persons (thousands)",
        title = "Total employment in US retail"
    )

components(dcmp) |> autoplot()

components(dcmp) |> gg_subseries(season_year)

components(dcmp) |>
    as_tsibble() |>
    autoplot(Employed, colour = "gray") +
    geom_line(aes(y=season_adjust), colour = "#0072B2") +
    labs(y = "Persons (thousands)",
         title = "Total employment in US retail")

# 3.3 moving averages

global_economy |>
    filter(Country == "Australia") |>
    autoplot(Exports) +
    labs(y = "% of GDP", title = "Total Australian exports")

aus_exports <- global_economy |>
    filter(Country == "Australia") |>
    mutate(
        `5-MA` = slider::slide_dbl(Exports, mean,
                                   .before = 2, .after = 2, .complete = TRUE)
    )

aus_exports |>
    autoplot(Exports) +
    geom_line(aes(y = `5-MA`), colour = "#D55E00") +
    labs(y = "% of GDP",
         title = "Total Australian exports") +
    guides(colour = guide_legend(title = "series"))

beer <- aus_production |>
    filter(year(Quarter) >= 1992) |>
    select(Quarter, Beer)
beer_ma <- beer |>
    mutate(
        `4-MA` = slider::slide_dbl(Beer, mean,
                                   .before = 1, .after = 2, .complete = TRUE),
        `2x4-MA` = slider::slide_dbl(`4-MA`, mean,
                                     .before = 1, .after = 0, .complete = TRUE)
    )

us_retail_employment_ma <- us_retail_employment |>
    mutate(
        `12-MA` = slider::slide_dbl(Employed, mean,
                                    .before = 5, .after = 6, .complete = TRUE),
        `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
                                      .before = 1, .after = 0, .complete = TRUE)
    )

us_retail_employment_ma |>
    autoplot(Employed, colour = "gray") +
    geom_line(aes(y = `2x12-MA`), colour = "#D55E00") +
    labs(y = "Persons (thousands)",
         title = "Total employment in US retail")

# 3.4 classical decomposition

us_retail_employment |>
    model(
        classical_decomposition(Employed, type = "additive")
    ) |>
    components() |>
    autoplot() +
    labs(title = "Classical additive decomposition of total
                  US retail employment")

# 3.5 Methods used by official statistics agencies

# also stl and TRAMO/SEATS
# stl is always additive

x11_dcmp <- us_retail_employment |>
    model(x11 = X_13ARIMA_SEATS(Employed ~ x11())) |>
    components()
autoplot(x11_dcmp) +
    labs(title =
             "Decomposition of total US retail employment using X-11.")

x11_dcmp |>
    ggplot(aes(x = Month)) +
    geom_line(aes(y = Employed, colour = "Data")) +
    geom_line(aes(y = season_adjust,
                  colour = "Seasonally Adjusted")) +
    geom_line(aes(y = trend, colour = "Trend")) +
    labs(y = "Persons (thousands)",
         title = "Total employment in US retail") +
    scale_colour_manual(
        values = c("gray", "#0072B2", "#D55E00"),
        breaks = c("Data", "Seasonally Adjusted", "Trend")
    )
# skipped a few here

# 3.6 stl decomposition

# seasonal and trend decomposition using loes.
# additie only
# no trading day or ckendar adjustments.

us_retail_employment |>
    model(
        STL(Employed ~ trend(window = 9) +
                season(window = "periodic"),
            robust = TRUE)) |>
    components() |>
    autoplot()

us_retail_employment |>
    model(
        STL(Employed ~ trend(window = 15) +
                # periodic is infinite window
                # season(window = "periodic"), # best?
                season(window = 15), # different from periodic
            robust = TRUE)) |>
    components() |>
    autoplot()



# 3.7 exercises

