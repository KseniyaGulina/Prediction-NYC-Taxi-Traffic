dis <- read.csv("taxi.csv")
dis
plot(dis)

n <- nrow(dis)
date <- as.Date(strptime(dis$timestamp[1:(n - 96)], '%Y-%m-%d'))
date <- date


library("xts")
data_ts <- xts(dis$value[1:(n - 96)], date)        # Convert data frame to time series
class(data_ts)
data_ts
data_ts <- ts(data_ts, frequency = 24)
plot(data_ts)
length(data_ts)
decompose_ts <- decompose(data_ts) 
plot(decompose_ts)

library(tseries)
library(forecast)
adf.test(data_ts, alternative=c('stationary'))


g.series <- log(data_ts)

n.obs <- length(g.series)
n.obs

plot(1:n.obs, g.series, type = "l")


g.2 <- matrix(rep(0, (n.obs - 24)*25), nrow = (n.obs - 24), ncol = 25)


for (i in 1:(n.obs - 24))
{
  g.2[i, ] <- g.series[i:(24 + i)]
}


i.seed <- 2


library(nnet)

set.seed(12345+i.seed)


# подобрать параметры
g.net <- nnet(x = g.2[, 1:24], g.2[, 25], size = 6, 
              linout = TRUE, rang=0.1, decay=0.001, maxit = 1000)


plot(1:n.obs, g.series, type = "l")
lines(25:n.obs, g.net$fitted.values, col = "red")

g.forecast <- g.2[nrow(g.2), -1] 

pred.n = 48
pred.1 <- rep(-9999, pred.n)

for (i in 1:pred.n)
{
  pred.1[i] <- predict(g.net, g.forecast, type = "raw")
  g.forecast <- c( g.forecast[-1], pred.1[i])
}


plot(10000:n.obs, exp(g.series)[10000:length(g.series)], type = "l", xlim = c(10000, n.obs+pred.n), ylim = c(0, 30000))
lines(10000:n.obs, exp(g.net$fitted.values)[10000:length(g.series)], col = "red")
lines((n.obs+1):(n.obs+pred.n), exp(pred.1), col = "blue")
lines(n.obs:(n.obs+48), dis$value[n.obs:(n.obs + 48)], col = "red")

plot(exp(pred.1), type='l')
test <- dis$value[(n.obs + 1):(n.obs + 48)]
res <- exp(pred.1)
test
res
length(test)

mse <- sum((res - test) ** 2) / pred.n
mse

library(MLmetrics)
R2_Score(res, test)
