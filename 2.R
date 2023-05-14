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


a10 |>
    gg_subseries(Cost) +
    labs(
        y = "$ (millions)",
        title = "Australian antidiabetic drug sales"
    )
