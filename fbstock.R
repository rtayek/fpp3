rm(list = ls())
require(fpp3)
fb_stock<-gafa_stock|>filter(Symbol=="FB") |>
    mutate(trading_day=row_number()) |> # missing days
    # this works, probably due to row number.
    update_tsibble(index=trading_day,regular=TRUE)
fb_stock|>model(
    #snaive=SNAIVE(Close),
    naive=NAIVE(Close), # could be just RW()
    drift=RW(Close~drift()), # use NAIVE(Close ~ drift())?
    mean=MEAN(Close)
) |>
    forecast(h=10)|>
    autoplot(fb_stock,level=NULL)+
    labs(title="FB",y="$US")+
    guides(color=guide_legend(title="forecast"))

