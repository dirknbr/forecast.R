# forecast.R
This introduces a few known and a few new forecast functions. It then builds an **ensemble forecast** out of 13 models. It has the following steps:
1. Learn all models over training period
2. Predict h periods ahead and build a weighted Bayesian models of the forecasts
3. Retrain the model on training + h to give new forecasts beyond this period (using previous weights)

It introduces four Bayesian models in stan
1. ARMA(2, 1)
2. ARMA(2, 1) with weighting of obs
3. Local linear trend
4. Weight model (eg it can model 13 weights on 13 X variables and 10 time steps which is not possible in frequentist setup)

Note that most code has tests around the functions. You need to load all scripts to get the `ForecastEns()` to run.
