
# original https://github.com/stan-dev/example-models/blob/master/misc/moving-avg/arma11.stan

data {
  int<lower=1> T;       // number of observations
  real y[T];            // observed outputs
  real bo[2];           // lower and upper bound for phi and theta
  int<lower=3> h;       // horizon
}
parameters {
  real<lower=0> mu;              // mean term
  real<lower=bo[1], upper=bo[2]> phi[2];          // autoregression coeff
  real<lower=bo[1], upper=bo[2]> theta;           // moving avg coeff
  real<lower=0> sigma;  // noise scale
}
transformed parameters {
  vector[T] err;        // error for time t
  vector[T] nu;         // prediction for time t
  nu[1] = mu + phi[1] * mu + phi[2] * mu;   // assume err[0] == 0
  err[1] = y[1] - nu[1];
  nu[2] = mu + phi[1] * y[1] + phi[2] * mu;   
  err[2] = y[2] - nu[2];
  for (t in 3:T) {
    nu[t] = mu + phi[1] * y[t-1] + phi[2] * y[t-2] + theta * err[t-1];
    err[t] = y[t] - nu[t];
  }
}
model {
  // priors
  mu ~ normal(mean(y), 1);
  phi ~ normal(0, 1);
  theta ~ normal(0, 1);
  sigma ~ gamma(1, 1);

  // likelihood
  err ~ normal(0, sigma);
}
generated quantities {
  vector[h] yhat;
  yhat[1] = mu + phi[1] * y[T] + phi[2] * y[T - 1] + theta * err[T];
  yhat[2] = mu + phi[1] * yhat[1] + phi[2] * y[T];
  for (t in 3:h) {
    yhat[t] = mu + phi[1] * yhat[t - 1] + phi[2] * yhat[t - 2];
  }
}
