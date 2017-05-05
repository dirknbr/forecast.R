
# combine many different time series forecasts using a simple weighted model
# train the weight model on length horizon
# split data: train, test, forecast
# after weights learned, retrain the models
# ignore dates, trend, frequency, holidays

library(forecast)
library(bsts)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

mape <- function(act, pred) {
  # mean avg percent error
  # have at least 1 positive
  # Args:
  #  act: actual vector
  #  pred: predicted vector
  stopifnot(length(act) == length(pred), any(act > 0))
  valid <- act > 0
  return(mean(abs(act[valid] - pred[valid]) / act[valid]))  
}

# test
mape(1, 1.1) # = .1
mape(c(1, 0), c(1.1, Inf)) # = .1

plotPred <- function(act, pred, ahead = F, main = '') {
  # plot actual and pred
  # Args:
  #  act: actual vector
  #  pred: predicted vector
  if (!ahead) {
    n <- length(act) - length(pred)
    temp <- cbind(act, c(rep(NA, n), pred))
  } else {
    n <- length(act)  
    n2 <- length(pred)
    temp <- cbind(c(act, rep(NA, n2)), c(rep(NA, n), pred))
  }
  matplot(temp, type = 'l', main = main)
}

# test
plotPred(1:10, 11:12)
plotPred(1:10, 11:12, ahead = T)

# stan_weight <- stan_model('stan/weight.stan')

forecastEns <- function(y, train, test, iter = 800, chains = 3, plot = T) {
	# function to create 12 different forecasts
	# then build a model of weighted forecasts
	# retrain the forecasts and predict out of sample
	# Args:
	#  y: vector
	#  train: vector of indices to train first forecasts
	#  test: vector of indices to test forecasts on (length horizon) and build model of forecasts
	#  iter: number of iterations in MCMC
	#  chains: number of chains
  	#  plot: if TRUE plot charts
	# Returns:
	#  final forecasts

	stopifnot(length(y) >= max(test))
	stopifnot(max(train) + 1 == min(test))

	h <- length(test)

	periods <- list(train, c(train, test))

	for (i in 1:2) {
	 	per <- periods[[i]]

		aa <- auto.arima(y[per])
		summary(aa)
		paa <- forecast(aa, h)

		et <- ets(y[per])
		pet <- forecast(et, h)
		
		pth <- thetaf(y[per], h)
		
		ss <- AddLocalLinearTrend(list(), y[per]) 
		bs <- bsts(y[per], state.specification = ss, niter = iter)
		pbs <- predict(bs, horizon = h, burn = 100)

		ptr <- trendForecast(y[per], h)

		pex <- extrapolate(y[per], h)

		pma1 <- maForecastOptim(y[per], h, fun = maForecast)

		pma2 <- maForecastOptim(y[per], h, fun = maDiffForecast)
		
		pfe <- tail(fourierExtrap(y[per], h), h)
		
		paa2 <- auto.arima2(y[per], 5, h)
		
		fitarma21 <- sampling(stan_arma, data = list(y = y[per], T = max(per), bo = c(-1, 1), h = h), 
		                      iter = iter, chains = chains)
		paa3 <- apply(extract(fitarma21)$yhat, 2, mean)
		
		w <- pmax(.01, length(per) * per / sum(per))
		fitarma21w <- sampling(stan_arma_w, data = list(y = y[per], T = max(per), bo = c(-1, 1), w = w, h = h), 
		                      iter = iter, chains = chains)
		paa4 <- apply(extract(fitarma21w)$yhat, 2, mean)
		
		fitllt <- sampling(stan_llt, data = list(y = y[per], T = max(per), h = h), iter = iter, chains = chains)
		pllt <- apply(extract(fitllt)$yhat, 2, mean)
		
		fitlstar <- sampling(lstar_stan, data = list(y = y[per], T = max(per), h = h, d = 1), iter = iter, chains = chains)
		plstar <- tail(apply(extract(fitlstar)$yhat, 2, mean), h)
		
		fithmm <- sampling(hmmar1, data = list(y = y[per], T = max(per), h = h, K = 3), iter = iter, chains = chains)
		phmm <- tail(apply(extract(fithmm)$yhat, 2, mean), h)
		
		x <- as.matrix(cbind(paa$mean, pet$mean, pth$mean, pbs$mean, ptr, pex, pma1, pma2, pfe, paa2, 
		                     paa3, paa4, pllt, plstar, phmm))
    k <- ncol(x)
    
		if (i == 1) {
			beta <- weight_opt(y[test], x)
			cat('weights', beta, '\n')
			cat('in sample test\n')
		} else {
		  cat('out of sample\n')
		}

		# combined forecast
		yp <- x %*% beta
		act <- y[(max(per) + 1):(max(per) + h)]
		
		desc <- c('1. auto.arima', '2. ets', '3. theta', '4. bsts', '5. trend',
			'6. extrapolate', '7. MA optim', '8. MA Diff optim', '9. fourier',
			'10. auto.arima2', '11. ARMA(2, 1) bayes', '12. ARMA(2, 1) bayes weighted',
			'13. Local linear trend', '14. LSTAR', '15. HMM AR(1)', 'Ensembled forecast')
		x2 <- cbind(x, yp)
		if (i == 2 & plot) {
		  for (j in 1:(k + 1)) {
		    plotPred(y[per], x2[, j], ahead = T, main = desc[j])
		  }
		}		
		mapes <- sapply(1:(k + 1), function(i) mape(act, x2[, i]))
		print(cbind(mapes, rank(mapes)))
	}
	ret <- list(x = x2, mape = mapes, desc = desc)
	return(ret)
}


