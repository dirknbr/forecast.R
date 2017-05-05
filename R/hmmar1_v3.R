
library(rstan)
library(forecast)

hmmar1 <- stan_model('stan/hmmar1_v4.stan')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# set.seed(14)
# T <- 120
# h <- 10
# y <- 10 + c(arima.sim(list(ar = .8), T / 3), arima.sim(list(ar = .3), T / 3), arima.sim(list(ar = .6), T / 3))
# plot(y, type = 'l')
# 
# data <- list(T = T - h, y = y[1:(T - h)], h = h, K = 3)
# fit2 <- sampling(hmmar1, data = data, iter = 400, chains = 3)
# print(fit2, c('phi', 'alpha', 'sigma', 'prob[1,1]', 'p[1,1]', 'yhat[111]'))
# 
# stan_trace(fit2)
# 
# e <- extract(fit2)
# prob <- apply(e$prob, c(2, 3), mean)
# matplot(prob, type = 'l')
# yhat <- apply(e$yhat, 2, mean)
# 
# aa <- auto.arima(y[1:(T - h)])
# summary(aa)
# yhat2 <- c(fitted(aa), forecast(aa, h)$mean)
# 
# matplot(cbind(y, yhat, yhat2), type = 'l')
# 
# mape(tail(y, h), tail(yhat, h))
# mape(tail(y, h), tail(yhat2, h))
