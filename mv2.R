library(zoo)
y1 <- rnorm(1000, mean = 0, sd = 1)
y2 <- rnorm(1000, mean = 0, sd = 10)
y<-c(y1,y2)
sd(y1)
sd(y2)
sd(y)
#dates <- seq(as.Date("1970-01-01"), length = length(y), by = "day")
# zv <- rollapplyr(z, 3, var, partial = TRUE)
ts=ts(y)
v=rollapplyr(y, 100, by=100,var, partial = TRUE)
str(v)
sqrt(v)
mean(ts)
var(ts)
# do a log transform and see it it reduces the variance?


