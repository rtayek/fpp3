library(tsibble)
library(dplyr)
tsibbledata::aus_retail %>%
    filter(
        State == "Victoria",
        Industry == "Cafes, restaurants and catering services"
    ) %>%
    gg_lag(Turnover)
