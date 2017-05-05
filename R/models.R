
# 3 simple forecast methods: moving average, trend model, extrapolate

library(forecast)

trendForecast <- function(x, h = 1) {
  # uses a trend model of degree 3
  # Args:
  #  x: vector
  #  h: forecast horizon
  n <- length(x)
  df <- data.frame(x = x, t1 = 1:n, t2 = (1:n) ^ 2)
  m <- lm(x ~ t1 + t2, df)
  df2 <- data.frame(t1 = (n + 1):(n + h))
  df2$t2 <- df2$t1 ^ 2
  p <- predict(m, newdata = df2)
  return(p)
}

# test
x <- 1:10 + rnorm(10, 0, .1)
trendForecast(x, 2)

movAvg <- function(x, m = 2, weight = F, plot = F) {
  # average of the last m including current
  # x[i] = mean(x[i], x[i - 1]) for m = 2
  # Args:
  #  x: vector
  #  m: integer parameter
  #  weight: T for weight last most
  n <- length(x)
  stopifnot(m < n)
  if (weight) {
    w <- m:1
    w <- w / sum(w)
  } else {
    w <- rep(1 / m, m)
  }
  x2 <- filter(x, w, sides = 1)
  if (plot) matplot(cbind(x, x2), type = 'l')
  return(x2)
}

# test and compare with ma()
movAvg(x, 3)
movAvg(x, 3, weight = T)
ma(x, 3, centre = F)

movAvg(x, 4, plot = T)
movAvg(x, 4, weight = T)
ma(x, 4, centre = F)


extrapolate <- function(x, h = 1) {
  # extrapolate with difference between the last 2 obs
  # Args:
  #  x: vector
  #  h: forecast horizon
  n <- length(x)
  diff <- x[n] - x[n - 1]
  x2 <- c(x, rep(NA, h))
  for (i in 1:h) {
    x2[n + i] <- x2[n + i - 1] + diff
  }
  return(x2[(n + 1):(n + h)])
}

maForecast <- function(x, h = 1, m = 3, method = 'last', weight = F) {
  # Args:
  #  x: vector
  #  h: forecast horizon
  #  m: MA parameter (int)
  #  method: last or extrapolate
  #  weight: T for weight last most 
  ma1 <- movAvg(x, m, weight)
  n <- length(x)
  if (method == 'last') {
    return(rep(ma1[n], h))
  } else {
    return(extrapolate(ma1, h))
  }
}

maForecastOptim <- function(x, h = 1, m = 1:20, fun = maForecast) {
  # grid search to find best parameters
  # Args:
  #  x: vector
  #  h: forecast horizon
  #  m: MA parameter (int)
  #  fun: which forecast function
  best <- Inf
  p <- list()
  n <- length(x)
  x2 <- x[1:(n - h)]
  for (weight in c(T, F)) {
    for (meth in c('last', 'extrap')) {
      for (mi in m) {
        f <- fun(x2, h, mi, meth, weight)
        err <- mape(tail(x, h), f)
        if (err < best) {
          best <- err
          p <- list(m = mi, method = meth, weight = weight)
        }
      }
    }
  }
  f2 <- fun(x, h, p$m, p$method, p$weight)
  cat(best, p$m, p$method, p$weight, '\n')
  return(f2)
}

maDiffForecast <- function(x, h = 1, m = 20, method = 'last', weight = F) {
  # use mov avg forecast and add ARMA modeled diff (x - movavg)
  # Args:
  #  x: vector
  #  h: forecast horizon
  #  m: MA parameter (int)
  #  method: last or extrapolate
  #  weight: T for weight last most 
  ma <- movAvg(x, m, weight)
  d <- x - ma
  aa <- auto.arima(d)
  faa <- forecast(aa, h)$mean
  return(maForecast(x, h, m, method, weight) + faa)
}

# test
extrapolate(x, 2)
maForecast(x, 2)
maForecast(x, 2, method = 'extrap')
maDiffForecast(1:100 + rnorm(100, 0, .1), 10)
maForecastOptim(x, 2, m = 1:3)
maForecastOptim(x, 2, m = 1:3, fun = maDiffForecast)
