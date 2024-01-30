LakeHuron
plot(LakeHuron)
air<- AirPassengers
plot(AirPassengers)
x <- decompose(air, type="multiplicative")
plot(x)
plot(x$trend)

# LakeHuron не имеет сезонности

library("xts")
data_ts <- xts(close, date)        # Convert data frame to time series
class(data_ts)
data_ts
data_ts <- ts(data_ts, frequency = 12)
plot(data_ts)
decompose_ts <- decompose(data_ts) # почему такой type
plot(decompose_ts)



# пользование такси
# декомпозиция на тренд, сезонность и случайные отклонения
dis <- read.csv("taxi.csv")
close <- ts(dis$value[1:1000], frequency = 24, start=date[1])
dis
n <- nrow(dis)
n
date <- as.Date(strptime(dis$timestamp[1:1000], '%Y-%m-%d'))
date <- date
class(date[1])
date[which(is.na(date))]
close <- ts(dis$Close, frequency = 24, start=date[1]) # по какому принципу выбирается frequency
test <- close[(n - 7):n]
close <- close[1:(n - 7)]
class(close)
length(close)
length(date)
plot(date, close)
decompose_close <- decompose(close) # почему такой type
plot(decompose_close) # поменять обозначени x (даты) на грфаике
length(date)
decompose_close

#decompose_close <- decompose(log(close))








# обучение с лекции

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

pred.n = 96
pred.1 <- rep(-9999, pred.n)

for (i in 1:pred.n)
{
  pred.1[i] <- predict(g.net, g.forecast, type = "raw")
  g.forecast <- c( g.forecast[-1], pred.1[i])
}


plot(1:n.obs, exp(g.series), type = "l", xlim = c(0, n.obs+pred.n), ylim = c(0, 30000))
lines(25:n.obs, exp(g.net$fitted.values), col = "red")
lines((n.obs+1):(n.obs+pred.n), exp(pred.1), col = "blue")
lines(1000:1096, dis$value[1000:1096], col = "red")


plot(1:n.obs, g.series, type = "l", xlim = c(0, n.obs+pred.n))
lines(8:n.obs, g.net$fitted.values, col = "red")
lines((n.obs+1):(n.obs+pred.n), pred.1, col = "blue")

plot(exp(pred.1)[20:68], type='l')
test
max(close)
plot(close[1:47], type='l')






















# проверка на стационарность
library(tseries)
library(forecast)
adf.test(close, alternative=c('stationary'))
# p-value больше 0.05 поэтому мы не можем опровергнуть нулевую гипотезу(нестационарность), значит ряд нестационарный
# он имеет некоторую структуру, зависящую от времени, и не имеет постоянной дисперсии во времени
# стационарные процессы проще анализировать

# убираем тренд, чтобы сделать ряд стационарным
diff <- close[2:length(close)] - close[1:length(close) - 1] # разница между close[i] и close[i - 1]
length(diff)
length(close) # length(diff) > length(close) на 1

inverted <- close[1:length(close) - 1] + diff # инвертируем обратно для наглядности

plot(close)
plot(diff, type='l')
plot(inverted, type='l')


# обучение с сайта
fit <- nnetar(data_ts, p = 12)
fc <- forecast(fit, h = 12)
plot(fc, include = 200, showgap = FALSE, ylim = c(50, 200))

# обучение с лекции

g.series <- log(data_ts)

n.obs <- length(g.series)
n.obs

plot(1:n.obs, g.series, type = "l")


g.2 <- matrix(rep(0, (n.obs - 12)*13), nrow = (n.obs - 12), ncol = 13)


for (i in 1:(n.obs - 12))
{
  g.2[i, ] <- g.series[i:(12 + i)]
}


i.seed <- 2


library(nnet)

set.seed(12345+i.seed)


# подобрать параметры
g.net <- nnet(x = g.2[, 1:12], g.2[, 13], size = 6, 
              linout = TRUE, rang=0.1, decay=0.001, maxit = 100)


plot(1:n.obs, g.series, type = "l")
lines(13:n.obs, g.net$fitted.values, col = "red")


g.forecast <- g.2[nrow(g.2), -1] 

pred.n = 20
pred.1 <- rep(-9999, pred.n)

for (i in 1:pred.n)
{
  pred.1[i] <- predict(g.net, g.forecast, type = "raw")
  g.forecast <- c( g.forecast[-1], pred.1[i])
}

ts_series <- xts(exp(g.series), date)
plot.ts(ts_series)

plot(1:n.obs, exp(g.series), type = "l", xlim = c(0, n.obs+pred.n), ylim = c(50, 200))
info <- exp(g.series)
plot((n.obs - 365):n.obs, info[(length(info) - 365):length(info)], type = "l", xlim = c((n.obs - 365), n.obs+pred.n), ylim = c(50, 200))
lines(13:n.obs, exp(g.net$fitted.values), col = "red")
lines((n.obs+1):(n.obs+pred.n), exp(pred.1), col = "blue")

exp(pred.1)
test
max(close)
