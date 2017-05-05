
# save the data for Rmd file as it errors 

library(quantmod)
stocks <- c('TGT', 'WMT', 'DIS', 'KO')

for (s in stocks) {
  gs <- getSymbols(s, auto.assign = F)
  save(gs, file = paste0(s, '.Rdata'))
}