require(fpp3)
y <- tsibble(
    Year = 2015:2019,
    Observation = c(123, 39, 78, 52, 110),
    index = Year
)

PBS |>
    filter(ATC2 == "A10") |>
    select(Month, Concession, Type, Cost)

PBS |>
    filter(ATC2 == "A10") |>
    select(Month, Concession, Type, Cost) |>
    summarise(TotalC = sum(Cost))

PBS |>
    filter(ATC2 == "A10") |>
    select(Month, Concession, Type, Cost) |>
    summarise(TotalC = sum(Cost)) |>
    mutate(Cost = TotalC/1e6)

PBS |> # get a10
    filter(ATC2 == "A10") |>
    select(Month, Concession, Type, Cost) |>
    summarise(TotalC = sum(Cost)) |>
    mutate(Cost = TotalC / 1e6) -> a10

a10 |> autoplot(cost)

autoplot(a10, Cost) +
    labs(y = "$ (millions)",
         title = "Australian antidiabetic drug sales")

prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")

prison <- prison |>
    mutate(Quarter = yearquarter(Date)) |>
    select(-Date) |>
    as_tsibble(key = c(State, Gender, Legal, Indigenous),
               index = Quarter)

melsyd_economy <- ansett |>
    filter(Airports == "MEL-SYD", Class == "Economy") |>
    mutate(Passengers = Passengers/1000)
autoplot(melsyd_economy, Passengers) +
    labs(title = "Ansett airlines economy class",
         subtitle = "Melbourne-Sydney",
         y = "Passengers ('000)")

ansett |> autoplot(Passengers)

us_employment |> autoplot(Employed)

gafa_stock |> filter(Symbol=="AMZN",year(Date)>=2018) |>
    autoplot(Close) +labs(x="us")

pelt |> autoplot(Lynx)+labs(y="y")

# 2.4a

a10 |> autoplot(Cost)

a10 |> gg_season(Cost,labels="both")+ labs(y="y")


vic_elec |> autoplot()

vic_elec |> gg_season(Demand)
    
vic_elec |> gg_season(Demand, period = "day") +
    theme(legend.position = "none") +
    labs(y="MWh", title="Electricity demand: Victoria")


vic_elec |> gg_season(Demand, period = "week") +
    theme(legend.position = "none") +
    labs(y="MWh", title="Electricity demand: Victoria")

# 2.5

beer <- aus_production |>
    select(Quarter,Beer) |>
    filter(year(Quarter) >= 1992)
beer |> autoplot(Beer)
beer |> gg_subseries(Beer)

# watch the above again

holidays <- tourism |>
    filter(Purpose == "Holiday") |>
    group_by(State) |>
    summarise(Trips = sum(Trips))

autoplot(holidays, Trips) +
    labs(y = "Overnight trips ('000)",
         title = "Australian domestic holidays")

gg_season(holidays, Trips) +
    labs(y = "Overnight trips ('000)",
         title = "Australian domestic holidays")

holidays |>
    gg_season(Trips) +
    facet_wrap(vars(State), nrow=2, scales = "free_y") +
    labs(y = "Trips ('000)",
         title = "Australian domestic holiday nights")

# 2.6 scatterplots

vic_elec |>
    filter(year(Time) == 2014) |>
    autoplot(Demand) +
    labs(y = "GW",
         title = "Half-hourly electricity demand: Victoria")
vic_elec |>
    filter(year(Time) == 2014) |>
    autoplot(Temperature) +
    labs(y = "GW",
         title = "Temperature: Victoria")

vic_elec_day_type <- vic_elec |>
    filter(year(Time) == 2014) |>
    mutate(Day_type=case_when(
        Holiday~"Holiday",
        wday(Date) %in% 2:6~"Weekday",
        TRUE~"Weekend"
    ))
vic_elec_day_type

vic_elec |>
    filter(year(Time) == 2014) |>
    ggplot(aes(x = Temperature, y = Demand)) +
    geom_point() +
    labs(x = "Temperature (degrees Celsius)",
         y = "Electricity demand (GW)")

vic_elec |> # fails with: object 'Day_Type' not found
    ggplot(aes(x = Temperature, y = Demand, colour=Day_Type)) +
    geom_point() +
    labs(x = "Temperature (degrees Celsius)",
         y = "Electricity demand (GW)")

us_change |> GGally::ggpairs(columns = 2:6)

a10 |>
    gg_subseries(Cost) +
    labs(
        y = "$ (millions)",
        title = "Australian antidiabetic drug sales"
    )
 
# 2.7 lag plots

recent_production <- aus_production |>
    filter(year(Quarter) >= 2000)
recent_production |>
    gg_lag(Beer, geom = "point") +
    labs(x = "lag(Beer, k)")

# try the above with some stocks

# 2.8 auto correlation

recent_production |> ACF(Beer, lag_max = 9)

# formula for correlation is different from the usual formula!

recent_production |>
    ACF(Beer) |>
    autoplot() + labs(title="Australian beer production")

a10 |>
    ACF(Cost, lag_max = 48) |>
    autoplot() +
    labs(title="Australian antidiabetic drug sales")

retail <- us_employment |> filter(Title=="Retail Trade",year(Month) >= 1980)
retail |> autoplot(Employed)

google_2015 <- gafa_stock |> filter(Symbol=="GOOG",year(Date)>=2015) |> select(Date,Close)

# [!] - means irregular time spacing

google_2015 |> autoplot(Close)

google_2015 |> ACF(Close,lag_max=100) |> autoplot()

# doing this with residuals is more common!

a10 |>
    ACF(Cost, lag_max = 48) |>
    autoplot() +
    labs(title="Australian antidiabetic drug sales")

# 2.9 white noise

set.seed(30)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)
y |> autoplot(wn) + labs(title = "White noise", y = "")

y |> ACF(wn) |>  autoplot() + labs(title = "White noise")

pigs <- aus_livestock |> filter(State=="Victoria",Animal=="Pigs",year(Month)>=2014)
pigs |> autoplot(Count/1e3)+labs(y="y")
pigs |> ACF() |> autoplot()

# 2.10 Exercises
