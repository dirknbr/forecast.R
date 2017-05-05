
# which model gives the best out sample mape
# auto.arima, arima with known order or one that optimises mape on small test

library(forecast)

auto.arima2 <- function(x, h1 = 1, h2 = 1, pq.max = 4) {
  # grid search in arima that has the lowest mape on test horizon
  # Args:
  #  x: vector
  #  h1: test horizon over which to choose best model
  #  h2: true out of sample horizon
  #  pq.max: max number of p and q to use in grid
  n <- length(x)
  train <- 1:(n - h1)
  test <- (max(train) + 1):n
  
  grid <- expand.grid(p = 0:pq.max, d = 0:1, q = 0:pq.max, drift = c(T, F), mean = c(T, F))
  
  best <- 100
  m3 <- list()
  for (i in 1:nrow(grid)) {
    cur <- unlist(c(grid[i, 1:3]))
    d <- unlist(c(grid[i, 4:5]))
    tryCatch({
        m2 <- Arima(x[train], order = cur, include.drift = d[1], include.mean = d[2])
        fm2 <- forecast(m2, h1)$mean
        if (mape(x[test], fm2) < best) {
          # only accept if extended model doesn't fail
          m3 <- Arima(x[c(train, test)], cur, include.drift = d[1], include.mean = d[2])
          best <- mape(x[test], fm2)
          cat(cur, d, best, '\n')
        }
      }, error = function(err) {
        m3 <- list()
      }, finally = {
      })
    }
  # if all failed use the last obs plus noise so stan won't fail
  if (length(m3) > 0) {
    fm3 <- forecast(m3, h2)$mean
  } else {
    fm3 <- rep(x[n], h2) + rnorm(h2, 0, .1) 
  }
  return(fm3)
}

# # simulate some data and test it
# set.seed(16)
# ord <- c(2, 1, 1)
# x <- 200 + arima.sim(list(order = ord, ar = c(.7, -.4), ma = .3), 100)
# plot(x)
# train <- 1:90
# test <- 91:100
# h <- length(test)
# aa <- auto.arima(x[train], trace = T)
# summary(aa)
# 
# m <- Arima(x[train], order = ord)
# summary(m)
# 
# faa <- forecast(aa, h)$mean
# fm <- forecast(m, h)$mean
# fm3 <- auto.arima2(x[train], 3, h)
# 
# mape(x[test], faa)
# mape(x[test], fm)
# mape(x[test], fm3)
