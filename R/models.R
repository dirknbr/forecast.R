
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

movAvg <- function(x, m = 2) {
  # average of the last m including current
  # x[i] = mean(x[i], x[i - 1]) for m = 2
  # Args:
  #  x: vector
  #  m: integer parameter
  n <- length(x)
  stopifnot(m < n)
  x2 <- rep(NA, n)
  for (i in m:n) {
    x2[i] <- mean(x[(i - m + 1):i])
  }
  return(x2)
}

# test and compare with ma()
movAvg(x, 3)
ma(x, 3)

movAvg(x, 4)
ma(x, 4)


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

maForecast <- function(x, h = 1, m = 3, method = 'last') {
  # Args:
  #  x: vector
  #  h: forecast horizon
  #  m: MA parameter (int)
  #  method: last or extrapolate
  ma1 <- movAvg(x, m)
  n <- length(x)
  if (method == 'last') {
    return(rep(ma1[n], h))
  } else {
    return(extrapolate(ma1, h))
  }
}

# test
extrapolate(x, 2)
maForecast(x, 2)
maForecast(x, 2, method = 'extrap')