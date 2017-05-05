
library(rstan)
library(forecast)

# setwd('/usr/local/google/home/dnachbar/R/forecast_ens')
# setar_stan <- stan_model('stan/setar.stan')
lstar_stan <- stan_model('stan/lstar.stan')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# set.seed(18)
# n <- 100
# h <- 10
# y <- rnorm(n)
# for (i in 2:n) {
#   if (y[i - 1] > 0) {
#     y[i] <- .5 + .7 * y[i - 1] + rnorm(1)
#   } else {
#     y[i] <- -.5 + .3 * y[i - 1] + rnorm(1)
#   }
# }
# y <- 10 + y
# 
# acf(y)
# plot(y, type = 'l')
# 
# data <- list(T = n - h, h = h, y = y[1:(n - h)], d = 1)
# fit <- sampling(setar_stan, data, iter = 3000, chains = 3)
# print(fit, c('phi', 'mu', 'th', 'sigma'))
# stan_trace(fit)
# e <- extract(fit)
# yhat <- apply(e$yhat, 2, mean)
# 
# fit2 <- sampling(lstar_stan, data, iter = 2000, chains = 3)
# print(fit2, c('phi', 'mu', 'th', 'sigma', 'gam'))
# stan_trace(fit2)
# e2 <- extract(fit2)
# yhat2 <- apply(e2$yhat, 2, mean)
# p <- apply(e2$p, 2, mean)
# plot(p, type = 'l')
# 
# aa <- auto.arima(y[1:(n - h)])
# summary(aa)
# faa <- forecast(aa, h)$mean
# 
# matplot(cbind(y, yhat, yhat2, c(rep(NA, n - h), faa)), type = 'l')
# legend('bottomleft', c('actual', 'setar', 'lstar', 'arima'), col = 1:4, lty = 1)
# 
# sqrt(mean((tail(y, h) - tail(yhat, h)) ^ 2))
# sqrt(mean((tail(y, h) - tail(yhat2, h)) ^ 2))
# sqrt(mean((tail(y, h) - faa) ^ 2))
