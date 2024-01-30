# считывание данных
dis <- read.csv("taxi.csv")
dis
class(dis)
plot(dis)
sum(is.na(dis))
class()

n <- nrow(dis)
# приведение типво данных

#as.POSIXct(dis$timestamp[length(dis$timestamp)], "%Y-%m-%d %H:%M:%S", tz="UTC")
#date <- as.Date(strptime(dis$timestamp[1:(n - 48)], '%Y-%m-%d'))
date <- as.POSIXct(dis$timestamp[1:(n - 48)], "%Y-%m-%d %H:%M:%S", tz="UTC")
date <- date
str(dis)
class(date)

library("xts")
# создание time series с периодом 48
data_ts <- xts(dis$value[1:(n - 48)], date)
plot(data_ts)
class(data_ts)
data_ts

data_ts <- ts(data_ts, frequency = 48)
plot(data_ts, ylab="Taxi traffic")
length(data_ts)
# декомпозиция
decompose_ts <- decompose(data_ts) 
plot(decompose_ts)

# построение гистограммы временного ряда по количеству данных с определенными значениями
hist(data_ts, breaks = 60, xlab="Taxi traffic", main="Histogram of Taxi traffic")
# выделение первого и второго среднеквадратичного отклонения
abline(v = mean(data_ts) + (-3:3) * sqrt(var(data_ts)), col = abs((-3:3)) + 2)
# некоторые данные находятся за границами второго отклонения (выбросы/ошибки)
extremum <- ifelse(data_ts > mean(data_ts) + 2 * sd(data_ts), 1, 
                   ifelse(data_ts < mean(data_ts) - 2 * sd(data_ts), -1, 0))
# заменим ошибочные данные на границы отклонения
data_ts_corrected <- ifelse(extremum == 1, mean(data_ts) + 2 * sd(data_ts), 
                            ifelse(extremum == -1, mean(data_ts) - 2 * sd(data_ts), data_ts))

data_ts_corrected
plot(data_ts_corrected, ylab="Taxi traffic")

hist(data_ts_corrected, breaks = 60, xlab="Taxi traffic", main="Histogram of Taxi traffic")
abline(v = mean(data_ts_corrected) + (-3:3) * sqrt(var(data_ts_corrected)), col = abs((-3:3)) + 2)

decompose_ts_corrected <- decompose(data_ts_corrected) 
plot(decompose_ts_corrected)

library(tseries)
library(forecast)
# проверка на стационарность
adf.test(data_ts, alternative=c('stationary'))

help(adf.test)
help(MSE)
# логарифмирование для упрощения работы
g.series <- log(data_ts_corrected)

n.obs <- length(g.series)
n.obs

plot(1:n.obs, g.series, type = "l", xlab="Time", ylab="log Taxi traffic")

# матрица для прогнозирования
g.2 <- matrix(rep(0, (n.obs - 48)*49), nrow = (n.obs - 48), ncol = 49)
nrow(g.2)
ncol(g.2)
g.2

# заполнение матрицы
for (i in 1:(n.obs - 48))
{
  g.2[i, ] <- g.series[i:(48 + i)]
}

# зерно, чтобы значения прогноза не менялись
i.seed <- 2


library(nnet)

set.seed(12345+i.seed)


# подобрать параметры
# сюда вставить еще цикл, который смотрит какая модель лучше
# в цикле проверить size = [4, 6, 8, 10] и посмотреть, где лучше score и MSE

# 12 первых значений в строке - входные значения, 13-ое - выходное
# Прогнозирование на 1 наблюдение вперед
g.net_4 <- nnet(x = g.2[, 1:48], g.2[, 49], size = 4, 
              linout = TRUE, rang=0.1, decay=0.001, maxit = 1000)


g.net_6 <- nnet(x = g.2[, 1:48], g.2[, 49], size = 6, 
              linout = TRUE, rang=0.1, decay=0.001, maxit = 1000)


g.net_8 <- nnet(x = g.2[, 1:48], g.2[, 49], size = 8, 
              linout = TRUE, rang=0.1, decay=0.001, maxit = 1000)

g.net_10 <- nnet(x = g.2[, 1:48], g.2[, 49], size = 10, 
                linout = TRUE, rang=0.1, decay=0.001, maxit = 1000)


plot(1:n.obs, g.series, type = "l")
lines(49:n.obs, g.net_8$fitted.values, col = "red")

mse <- c()
mse[1] <- MSE(g.series[49:length(g.series)], g.net_4$fitted.values)
mse[2] <- MSE(g.series[49:length(g.series)], g.net_6$fitted.values)
mse[3] <- MSE(g.series[49:length(g.series)], g.net_8$fitted.values)
mse[4] <- MSE(g.series[49:length(g.series)], g.net_10$fitted.values)
plot(c(4, 6, 8, 10), mse, type='l', xlab="Nnet size", ylab="MSE", main="MSE(history, model)")
lines(c(4, 6, 8, 10), rep(min(mse), 4), col='red')
mse
lines(mse)
plot(score, type='l')
lines(c(1:4), rep(max(score), 4), col='red')

score <- c()
score[1] <- R2_Score(g.series[49:length(g.series)], g.net_4$fitted.values)
score[2] <- R2_Score(g.series[49:length(g.series)], g.net_6$fitted.values)
score[3] <- R2_Score(g.series[49:length(g.series)], g.net_8$fitted.values)
score[4] <- R2_Score(g.series[49:length(g.series)], g.net_10$fitted.values)

plot(c(4, 6, 8, 10), score, type='l', xlab="Nnet size", ylab="R2_Score", main="R2_Score(history, model)")
lines(c(1:4), rep(max(score), 4), col='red')

new_mse = c()
new_score = c()

#nnet_4

g.forecast <- g.2[nrow(g.2), -1] 
g.forecast

pred.n = 48
pred.1 <- rep(-9999, pred.n)

for (i in 1:pred.n)
{
  pred.1[i] <- predict(g.net_4, g.forecast, type = "raw")
  g.forecast <- c( g.forecast[-1], pred.1[i])
}

g.forecast[-1]
pred.1[i]

plot(10000:n.obs, exp(g.series)[10000:length(g.series)], type = "l", xlim = c(10000, n.obs+pred.n), ylim = c(0, 30000), xlab = "Time", ylab="Taxi traffic", main="NNET_4")
#lines(10000:n.obs, exp(g.net$fitted.values)[10000:length(g.series)], col = "red")
lines((n.obs+1):(n.obs+pred.n), exp(pred.1), col = "blue")
#lines(10000:n.obs, exp(g.net_4$fitted.values[9962:length(g.series)]), col = "green")
lines(n.obs:(n.obs+48), dis$value[n.obs:(n.obs + 48)], col = "red")
legend(x = "topleft", legend = c("history", "test", "predict"), lty = c(1, 1, 1), col = c(1, "red", "blue"), lwd = 2)
length(exp(g.net_4$fitted.values[9962:length(g.series)]))
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
R2_Score(c(g.net_6$fitted.values, res), dis$value)
length(c(g.net_6$fitted.values, res))
length(dis$value)

new_mse[1] <- mse
new_score[1] <- R2_Score(res, test)

#nnet_6

g.forecast <- g.2[nrow(g.2), -1] 

pred.n = 48
pred.1 <- rep(-9999, pred.n)

for (i in 1:pred.n)
{
  pred.1[i] <- predict(g.net_6, g.forecast, type = "raw")
  g.forecast <- c( g.forecast[-1], pred.1[i])
}


plot(10000:n.obs, exp(g.series)[10000:length(g.series)], type = "l", xlim = c(10000, n.obs+pred.n), ylim = c(0, 30000), xlab = "Time", ylab="Taxi traffic", main="NNET_6")
#lines(10000:n.obs, exp(g.net$fitted.values)[10000:length(g.series)], col = "red")
lines((n.obs+1):(n.obs+pred.n), exp(pred.1), col = "blue")
#lines(10000:n.obs, exp(g.net_4$fitted.values[9962:length(g.series)]), col = "green")
lines(n.obs:(n.obs+48), dis$value[n.obs:(n.obs + 48)], col = "red")
legend(x = "topleft", legend = c("history", "test", "predict"), lty = c(1, 1, 1), col = c(1, "red", "blue"), lwd = 2)
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

new_mse[2] <- mse
new_score[2] <- R2_Score(res, test)

#nnet_8

g.forecast <- g.2[nrow(g.2), -1] 

pred.n = 48
pred.1 <- rep(-9999, pred.n)

for (i in 1:pred.n)
{
  pred.1[i] <- predict(g.net_8, g.forecast, type = "raw")
  g.forecast <- c( g.forecast[-1], pred.1[i])
}


plot(10000:n.obs, exp(g.series)[10000:length(g.series)], type = "l", xlim = c(10000, n.obs+pred.n), ylim = c(0, 30000), xlab = "Time", ylab="Taxi traffic", main="NNET_8")
#lines(10000:n.obs, exp(g.net$fitted.values)[10000:length(g.series)], col = "red")
lines((n.obs+1):(n.obs+pred.n), exp(pred.1), col = "blue")
#lines(10000:n.obs, exp(g.net_4$fitted.values[9962:length(g.series)]), col = "green")
lines(n.obs:(n.obs+48), dis$value[n.obs:(n.obs + 48)], col = "red")
legend(x = "topleft", legend = c("history", "test", "predict"), lty = c(1, 1, 1), col = c(1, "red", "blue"), lwd = 2)


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

new_mse[3] <- mse
new_score[3] <- R2_Score(res, test)

#nnet_10

g.forecast <- g.2[nrow(g.2), -1] 

pred.n = 48
pred.1 <- rep(-9999, pred.n)

for (i in 1:pred.n)
{
  pred.1[i] <- predict(g.net_10, g.forecast, type = "raw")
  g.forecast <- c( g.forecast[-1], pred.1[i])
}


plot(10000:n.obs, exp(g.series)[10000:length(g.series)], type = "l", xlim = c(10000, n.obs+pred.n), ylim = c(0, 30000), xlab = "Time", ylab="Taxi traffic", main="NNET_10")
#lines(10000:n.obs, exp(g.net$fitted.values)[10000:length(g.series)], col = "red")
lines((n.obs+1):(n.obs+pred.n), exp(pred.1), col = "blue")
#lines(10000:n.obs, exp(g.net_4$fitted.values[9962:length(g.series)]), col = "green")
lines(n.obs:(n.obs+48), dis$value[n.obs:(n.obs + 48)], col = "red")
legend(x = "topleft", legend = c("history", "test", "predict"), lty = c(1, 1, 1), col = c(1, "red", "blue"), lwd = 2)


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

new_mse[4] <- mse
new_score[4] <- R2_Score(res, test)

new_mse
new_score

plot(c(4, 6, 8, 10), new_mse, type='l', xlab="Nnet size", ylab="MSE", main="MSE(predict, test)")
lines(c(4, 6, 8, 10), rep(min(new_mse), 4), col='red')

plot(c(4, 6, 8, 10), new_score, type='l', xlab="Nnet size", ylab="R2_Score", main="R2_Score(predict, test)")
lines(c(4, 6, 8, 10), rep(max(new_score), 4), col='red')



# ПРОГНОЗ НА ДЕНЬ ВПЕРЕД

g <- dis$value 
extremum <- ifelse(g > mean(g) + 2 * sd(g), 1, 
                   ifelse(g < mean(g) - 2 * sd(g), -1, 0))
g <- ifelse(extremum == 1, mean(g) + 2 * sd(g), 
                            ifelse(extremum == -1, mean(g) - 2 * sd(g), g))
g <- log(g)

g.forecast <- g

pred.n = 48
pred.1 <- rep(-9999, pred.n)

for (i in 1:pred.n)
{
  pred.1[i] <- predict(g.net_6, g.forecast, type = "raw")
  g.forecast <- c( g.forecast[-1], pred.1[i])
}
pred.1

f_date <- c(as.POSIXct(dis$timestamp[length(dis$timestamp)], "%Y-%m-%d %H:%M:%S", tz="UTC")) + 30 * 60
for (i in 2:48) {
  f_date[i] <- f_date[i - 1] + 30 * 60
}

f_date

f_ts <- xts(exp(pred.1), f_date)
f_ts

plot(f_ts, xlab="Time", ylab="Taxi Traffic", main="Taxi traffic forecast")

