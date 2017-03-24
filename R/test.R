
library(quantmod)

# test on some daily share data

n <- 5 * 50 * 1 + 90 # 5 days a week 50 weeks per year
h <- 3
getSymbols('TGT')
plot(TGT)
df <- as.data.frame(TGT)
date <- row.names(df)
y <- df[, 1]
train <- 1:(n - 2 * h)
test <- (max(train) + 1):(n - h)
hor <- (max(test) + 1):n
# visualise with moving avg and fourier
matplot(cbind(y[train], movAvg(y[train], 10)), type = 'l')
matplot(cbind(c(y[train], rep(NA, 10)), fourierExtrap(y[train], 10)), type = 'l')

yp <- forecastEns(y, train, test)
