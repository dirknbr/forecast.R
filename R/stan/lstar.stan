
# https://www.rdocumentation.org/packages/tsDyn/versions/0.9-44/topics/LSTAR

data {
  int<lower=1> T;       // number of observations
  real y[T];            // observed outputs
  int<lower=2> h;       // horizon
  int d;
}
parameters {
  real<lower=0> mu[2];              // mean term
  real<lower=-1, upper=1> phi[2];          // autoregression coeff
  real<lower=0> sigma;  // noise scale
  real<lower=min(y), upper=max(y)> th;    // threshold
  real<lower=0> gam;
}
transformed parameters {
  real yy[T];
  real p[T];
  for (t in (d + 1):T) {
    p[t] = logistic_cdf(y[t - d], th, 1 / gam);
    yy[t] = (mu[1] + phi[1] * y[t - 1]) * p[t] + (mu[2] + phi[2] * y[t - 1]) * (1 - p[t]);
  }
}
model {
  // priors
  mu ~ normal(mean(y), 10);
  phi ~ normal(0.5, .5);
  th ~ normal(mean(y), 10);
  sigma ~ gamma(2, 1);
  gam ~ gamma(1, 1);
  for (t in (d + 1):T) {
    y[t] ~ normal(yy[t], sigma);
  }
}
generated quantities {
  real yhat[T + h];
  real p2;
  yhat[1:T] = y;
  for (t in (T + 1):(T + h)) {
    p2 = logistic_cdf(yhat[t - d], th, 1 / gam);
    yhat[t] = (mu[1] + phi[1] * yhat[t - 1]) * p2 + (mu[2] + phi[2] * yhat[t - 1]) * (1 - p2);
  }
}