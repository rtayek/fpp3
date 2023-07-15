# seems it's a bug: https://github.com/tidyverts/fable/issues/262
rm(list = ls())
#options(warn=1)
require(fpp3)
gdppc <- global_economy |>
    mutate(GDP_per_capita = GDP / Population)
fit <- gdppc |>
    model(trend_model = TSLM(GDP_per_capita ~ trend()))
# Warning: 7 errors (1 unique) encountered for trend_model
# [7] 0 (non-NA) cases
fit
