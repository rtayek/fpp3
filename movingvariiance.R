rzoo <- function(x,n) rollapplyr(x, n, sd, fill=NA)
rttr <- function(x,n) runSD(x, n)
library(rbenchmark)
set.seed(21)
x <- rnorm(1e4)
all.equal(rzoo(x,250), rttr(x,250))
# [1] TRUE
benchmark(rzoo(x,250), rttr(x,250))[,1:6]
#           test replications elapsed relative user.self sys.self
# 2 rttr(x, 250)          100    0.58    1.000      0.58     0.00
# 1 rzoo(x, 250)          100   54.53   94.017     53.85     0.06