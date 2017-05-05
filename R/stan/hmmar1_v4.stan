data {
  int T;
  int<lower=2> K;  // states
  real y[T];
  int<lower=2> h;
}
parameters {
  real<lower=-1, upper=1> phi[K];
  real<lower=0> alpha[K];
  simplex[K] prob[T];  // state prob
  matrix<lower=0>[T, K] p;
  real<lower=0> sigma;
}
transformed parameters {
  vector[T] mu;
  for (t in 2:T) {
    mu[t] = 0.0;
    for (i in 1:K) {
      mu[t] = mu[t] + prob[t, i] * (alpha[i] + phi[i] * y[t - 1]);
    }
  }
}
model {
  alpha ~ normal(mean(y), 10);
  // phi ~ normal(.1, .5);
  sigma ~ gamma(1, 1);
  p[1, ] ~ gamma(1, 1);
  for (t in 2:T) {
    for (i in 1:K) {
      p[t, i] ~ normal(p[t - 1, i], .1);
    }
    prob[t] ~ dirichlet(to_vector(p[t, ]));
    y[t] ~ normal(mu[t], sigma);
  }
}
generated quantities {
  vector[T + h] yhat;
  yhat[1:T] = mu;
  for (t in (T + 1):(T + h)) {
    yhat[t] = 0.0;
    for (i in 1:K) {
      yhat[t] = yhat[t] + prob[T, i] * (alpha[i] + phi[i] * yhat[t - 1]);
    }
  }
}