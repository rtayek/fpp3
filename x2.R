#aus_production
#Lynx from pel
# Close from gafa_stock
#Demand from vic_elec

goog<-gafa_stock |> filter(Symbol=="GOOG",year(Date)>=2000) |> select(Date,Close)
str(goog)

goog|>gg_season()

goog|>ACF(Close)

goog|>gg_subseries(Close) # takes a long time


dgoog <- gafa_stock |>
    filter(Symbol == "GOOG", year(Date) >= 2018) |>
    mutate(trading_day = row_number()) |>
    update_tsibble(index = trading_day, regular = TRUE) |>
    mutate(diff = difference(Close))
