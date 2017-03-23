
data {
  int N;
  int k; // forecast vectors
  matrix[N, k] x;
  vector[N] y;
}

parameters {
  vector<lower=0, upper=1>[k] beta;
  real<lower=0> sigma;
}

model {
  beta ~ normal(1.0 / k, .1);
  sigma ~ gamma(mean(y) / 100, 1);
  y ~ normal(x * beta, sigma); // no intercept
}
