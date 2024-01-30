
pred.n <- 12

plot(AirPassengers)


class(AirPassengers)

g.series <- log(as.numeric(AirPassengers))

n.obs <- length(g.series)
n.obs

plot(1:n.obs, g.series, type = "l")


g.2 <- matrix(rep(0, 132*13), nrow = 132, ncol = 13)


for (i in 1:132)
{
  g.2[i, ] <- g.series[i:(12 + i)]
}


i.seed <- 2


library(nnet)

set.seed(12345+i.seed)


g.net <- nnet(g.2[, 1:12], g.2[, 13], size = 6, 
              linout = TRUE, rang=0.1, decay=0.001, maxit = 1000)


plot(1:144, g.series, type = "l")
lines(13:144, g.net$fitted.values, col = "red")


g.forecast <- g.2[nrow(g.2), -1] 


pred.1 <- rep(-9999, pred.n)

for (i in 1:pred.n)
{
  pred.1[i] <- predict(g.net, g.forecast, type = "raw")
  g.forecast <- c( g.forecast[-1], pred.1[i])
}


plot(1:144, exp(g.series), type = "l", xlim = c(0, 144+1), ylim = c(100, 700))
lines(13:144, exp(g.net$fitted.values), col = "red")
lines((144+1):(144+pred.n), exp(pred.1), col = "blue")

exp(pred.1)



