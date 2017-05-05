
norm_weight <- function(w) {
  w <- w / sum(w)
  return(w)
}

weight_opt <- function(y, x) {
  # function to find the weight w s.t. (y - x * w)^2 is minimised
  
  stopifnot(length(y) == nrow(x))
  k <- ncol(x)
  w <- rep(1 / k, k)
  
  sse <- function(w, y, x) {
    w <- norm_weight(w)
    se <- (y - x %*% w) ^ 2
    return(sum(se))
  }
  
  wopt <- optim(w, sse, y = y, x = x, method = 'L-BFGS-B', lower = 0.001, upper = .999)
  cat('rsse', sqrt(wopt$value), '\n')
  return(norm_weight(wopt$par))
}

# y <- rnorm(10, 10, 1)
# x <- matrix(rnorm(100, 10, 1), nrow = 10)
# wopt <- weight_opt(y, x)
# cbind(y, x %*% wopt)
# 
# x <- cbind(x, y)
# wopt <- weight_opt(y, x)

# datal <- list(N = 10, y = y, k = ncol(x), x = x)
# fit <- sampling(stan_weight, data = datal, chains = 3, iter = 200)
# print(fit)
