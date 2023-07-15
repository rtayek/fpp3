rm(list = ls())
# Fourier series (from chapter 7 of fpp3)
library(fpp3)

# Example: Australian quarterly beer production

recent_production <- aus_production |>
    filter(year(Quarter) >= 1992)
fourier_beer <- recent_production |>
    model(TSLM(Beer ~ trend() + fourier(K = 2)))
report(fourier_beer)
#> -42.90  -7.60  -0.46   7.99  21.79 

# 7.5 Selecting predictors

# from chaper 10
# Dynamic harmonic regression with multiple seasonal periods