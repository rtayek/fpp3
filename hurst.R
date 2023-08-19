#========================================================#
# Quantitative ALM, Financial Econometrics & Derivatives 
# ML/DL using R, Python, Tensorflow by Sang-Heon Lee 
#
# https://kiandlee.blogspot.com
#——————————————————–#
# Hurst Exponent
#========================================================#

graphics.off(); rm(list = ls())

library(pracma) # hurstexp
library(quantmod) # getSymbols
library(xts)

#————————————————
# read S&P 500 index and calculate daily returns
#————————————————

sdate <- as.Date("1999-12-31")
edate <- as.Date("2022-08-01")
getSymbols("^GSPC", from=sdate, to=edate)

price <-  GSPC[,6]
return <- dailyReturn(price)
df.stock <- cbind(time=time(price), 
                  as.data.frame(price), 
                  as.data.frame(return))
rownames(df.stock) <- NULL
colnames(df.stock) <- c("time", "SPC.price", "SPC.return")

#=================================================
# calculation of Hurst exponent
#=================================================
#
# time (t)  : [1,2,……………………….,L]
# range s   :      1         2             Ns
# time(i,s) : [1,..,L/Ns][1,..,L/Ns]…[1,..,L/Ns]
#
# sub series
# number of sub series = s
# length of sub series = L/Ns = Ls
#
# For each value of Ls, R/S is calculated
#
# When (R/S)_Ls and Ls pairs are obtained, 
# linear regression is estimated on log of that pairs.
# Hurst exponent is the slope coefficient.
#
#=================================================

#————————————————
# calculation of Rescaled Ranges
#————————————————

# delete first zero
x <- df.stock$SPC.return[-1] 
L <- length(x)

# minimum length of sub series = around 8
vLs <- round(L/2^(0:9)) # length of sub series

# output container
df.rs <- data.frame(RS=NA, Ls = vLs)

# each the length of sub series (Ls)
for(k in 1:length(vLs)) {
    
    # select length of sub series, Ls
    Ls <- vLs[k]
    
    # number of returns or length of sub series
    Ns <- round(L/Ls,0)
    
    # rescaled range container
    rs <- rep(NA, Ns)
    
    # each range (sub series)
    for(i in 1:Ns) {
        
        # ith sub series
        x_is <- x[(Ls*(i-1)+1):(Ls*i)] 
        
        # when i == Ns, some NA will occur.
        x_is <- x_is[!is.na(x_is)]
        
        # mean, stdev, deviation, cumulative sum
        ms <- mean(x_is)
        ss <- sd(x_is)   
        ds <- x_is - ms
        zs <- cumsum(ds)
        
        # rescaled range
        rs[i] <- (max(zs)-min(zs))/ss
    }
    
    # save Ls and average of RS pairs
    df.rs[k,] <- c(mean(rs), Ls)
}

df.rs

#————————————————
# estimation of Hurst exponent
#————————————————

log.fit <- lm(log(RS)~log(Ls), data=df.rs)
summary(log.fit)
log.fit$coefficients[2]

#————————————————
# Using R package simply
# see Empirical Hurst exponent
#————————————————
hurstexp(x)
