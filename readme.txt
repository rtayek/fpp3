in 5.10, he:

google_stock <- gafa_stock |>
    filter(Symbol == "GOOG", year(Date) >= 2015) |>
    mutate(day = row_number()) |>
    update_tsibble(index = day, regular = TRUE)

maybe if we removed feb 29 from the leap years,
we could get some more seasonality/fourier? 

a fit class looks like: mdl_df,tbl_df,tbl, data.frame.
an augmented fit looks the same:

try: a10 |> gg_season(Cost,labels="both")+ labs(y="y")
with stock data also subseries: holidays|>gg_subseries(Trips)
goog

https://nickpoison.github.io/

feature: coef_hurst , feat_spectral


