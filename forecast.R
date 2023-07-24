rm(list = ls())
library(tidyverse)
library(tsibble)
library(fable)
library(lubridate)

monthly_qty_by_cat2 <- 
    sweep::bike_sales %>%
    mutate(order.month = yearmonth(order.date)) %>%
    group_by(category.secondary, order.month) %>%
    summarise(total.qty = sum(quantity)) %>% 
    as_tsibble(index=order.month, key=category.secondary) %>%
    mutate(x = rnorm(length(total.qty)))
#> `summarise()` regrouping output by 'category.secondary' (override with `.groups` argument)

future_x <- new_data(monthly_qty_by_cat2) %>%
    mutate(x = 2)

monthly_qty_by_cat2 %>%
    model(tslm = TSLM(total.qty ~ trend() + x)) %>%
    forecast(new_data=future_x)
#> # A fable: 9 x 6 [1M]
#> # Key:     category.secondary, .model [9]
#>   category.secondary .model order.month      total.qty  .mean     x
#>   <chr>              <chr>        <mth>         <dist>  <dbl> <dbl>
#> 1 Cross Country Race tslm      2016 Jan N(369, 187840) 369.       2
#> 2 Cyclocross         tslm      2016 Jan N(-2.5, 75604)  -2.50     2
#> 3 Elite Road         tslm      2016 Jan N(784, 322470) 784.       2
#> 4 Endurance Road     tslm      2016 Jan N(159, 117760) 159.       2
#> 5 Fat Bike           tslm      2016 Jan   N(95, 66320)  94.6      2
#> 6 Over Mountain      tslm      2016 Jan  N(194, 57732) 194.       2
#> 7 Sport              tslm      2016 Jan  N(120, 81568) 120.       2
#> 8 Trail              tslm      2016 Jan  N(214, 56269) 214.       2
#> 9 Triathalon         tslm      2016 Jan  N(102, 94449) 102.       2