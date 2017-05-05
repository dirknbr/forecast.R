
# stan arma
library(rstan)
stan_arma <- stan_model('stan/arma.stan')
stan_arma_w <- stan_model('stan/arma_weight.stan')
stan_llt <- stan_model('stan/llt.stan')

# test with simulated ARMA(2, 1)
# set.seed(3)
# y <- 10 + arima.sim(list(ar = c(.6, .3), ma = .3), 100)
# plot(y)
# train <- 1:95
# h <- 5
# 
# fit <- sampling(stan_arma, data = list(T = max(train), y = y[train], bo = c(-1, 1), h = h), iter = 1000, chains = 3)
# print(fit, c('mu', 'theta', 'phi', 'yhat', 'sigma'))
# stan_dens(fit, c('theta', 'phi'))
# yhat <- apply(extract(fit)$yhat, 2, mean)
# plotPred(y, yhat)
# 
# w <- length(train) * train / sum(train)
# summary(w)
# fit3 <- sampling(stan_arma_w, data = list(T = max(train), y = y[train], bo = c(-1, 1), w = w, h = h), iter = 1000, chains = 3)
# print(fit3, c('mu', 'theta', 'phi', 'yhat', 'sigma'))
# yhat3 <- apply(extract(fit3)$yhat, 2, mean)
# plotPred(y, yhat3)
# 
# fit4 <- sampling(stan_llt, data = list(T = max(train), y = y[train], h = h), iter = 1000, chains = 3)
# print(fit4, c('mu[1]', 'yhat', 'sigma_y'))
# yhat4 <- apply(extract(fit4)$yhat, 2, mean)
# plotPred(y, yhat4)
# 
# mape(y[-train], yhat)
# mape(y[-train], yhat3)
# mape(y[-train], yhat4)
# mape(y[-train], forecast(auto.arima(y[train]), h)$mean)
