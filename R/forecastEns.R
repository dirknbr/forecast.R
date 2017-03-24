
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


forecastEns <- function(y, train, test, iter = 800, chains = 3, plot = T) {
	# function to create 13 different forecasts
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

		pma1 <- maForecast(y[per], h)

		pma2 <- maForecast(y[per], h, method = 'ex')
		
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
		
		x <- as.matrix(cbind(paa$mean, pet$mean, pth$mean, pbs$mean, ptr, pex, pma1, pma2, pfe, paa2, 
		                     paa3, paa4, pllt))
    	k <- ncol(x)
    
		if (i == 1) {
			cpp <- stan_model('stan/weight.stan')
			datal <- list(N = h, y = y[test], k = ncol(x), x = x)
			fit <- sampling(cpp, data = datal, chains = chains, iter = iter)
			print(fit)
			beta <- apply(extract(fit)$beta, 2, mean)
			cat('in sample test\n')
		} else {
		  cat('out of sample\n')
		}

		# combined forecast
		yp <- x %*% beta
		act <- y[(max(per) + 1):(max(per) + h)]
		
		if (plot) {
		  plotPred(y[per], paa$mean, ahead = T, main = '1. auto.arima')
		  plotPred(y[per], pet$mean, ahead = T, main = '2. ets')
		  plotPred(y[per], pth$mean, ahead = T, main = '3. theta')
		  plotPred(y[per], pbs$mean, ahead = T, main = '4. bsts')
		  plotPred(y[per], ptr, ahead = T, main = '5. trend')
		  plotPred(y[per], pex, ahead = T, main = '6. extrapolate')
		  plotPred(y[per], pma1, ahead = T, main = '7. MA')
		  plotPred(y[per], pma2, ahead = T, main = '8. MA extrap')
		  plotPred(y[per], pfe, ahead = T, main = '9. fourier')
		  plotPred(y[per], paa2, ahead = T, main = '10. auto.arima2')
		  plotPred(y[per], paa3, ahead = T, main = '11. ARMA(2,1) bayes')
		  plotPred(y[per], paa4, ahead = T, main = '12. ARMA(2,1) bayes weighted')
		  plotPred(y[per], paa4, ahead = T, main = '13. Local linear trend')
		  # matplot(cbind(act, yp), type = 'l')
		  plotPred(y[per], yp, ahead = T, main = 'Ensembled forecast')
		}
		
		mapes <-sapply(1:k, function(i) mape(act, x[, i]))
		print(cbind(mapes, rank(mapes)))
		print(mape(act, yp))
	}
	return(yp)
}


