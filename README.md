# forecast.R
This introduces a few known and a few new forecast functions. It then builds an **ensemble forecast** out of 15 models. It has the following steps:
1. Learn all models over training period
2. Predict h periods ahead and build a weighted (optimised) model of the forecasts
3. Retrain the models on training + h to give new forecasts beyond this period (using previous weights)

It introduces five Bayesian models in stan
1. ARMA(2, 1)
2. ARMA(2, 1) with weighting of obs
3. Local linear trend
4. Logistic Self-exciting Threshold AR (LSTAR) model
5. HMM AR(1)

The form of the HMM is the following:
$y_t = \sum_i^K w_{it} (\alpha_i + \phi_i y_{t-1}) + \epsilon_t$ where $i = 1,2,...K$ and 
$p_{it} = N(p_{it-1}, .1)$ and $w_{it} = Dirichlet(p_{it})$ and $\sum_i w_i = 1$ for all $t$

Note that HMM is not a true Markov model as it doesn't use a transition matrix, but models the probabilities as random walks.

Note that most code has tests around the functions. You need to load all scripts to get the `forecastEns()` to run.