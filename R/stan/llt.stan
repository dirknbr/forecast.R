# local linear trend
data {
  int<lower=1> T;       // number of observations
  real y[T];            // observed outputs
  int<lower=2> h;
}
parameters {
  real mu[T];
  real beta[T];
  real<lower=0> sigma_y;
  real<lower=0> sigma_mu;
  real<lower=0> sigma_beta;
}
model {
  sigma_y ~ gamma(.5, 1);
  sigma_mu ~ gamma(.5, 1);
  sigma_beta ~ gamma(.5, 1);
  mu[1] ~ normal(mean(y), sigma_mu);
  beta[1] ~ normal(0, sigma_beta);
  for (t in 2:T) {
    beta[t] ~ normal(beta[t - 1], sigma_beta);
    mu[t] ~ normal(beta[t - 1] + mu[t - 1], sigma_mu);
  }
  y ~ normal(mu, sigma_y);
}
generated quantities {
 real yhat[h];
 real muhat[h];
 real betahat[h];
 betahat[1] = normal_rng(beta[T], sigma_beta);
 muhat[1] = normal_rng(mu[T], sigma_mu);
 // yhat[1] = beta[T] + mu[T]; // actual
 for (t in 2:h) {
    betahat[t] = normal_rng(betahat[t - 1], sigma_beta);
    muhat[t] = normal_rng(betahat[t - 1] + muhat[t - 1], sigma_mu);
 }
 yhat = muhat;
}