library(quantmod)
library(Quandl)

# TECHNICAL NOTES: 
##   1: The S&P 100 Index includes 102 tickers as a result of [relatively] recent corporate actions at
##      Google and 21st Century Fox.
##        Read more on GOOG/GOOGL here: http://prn.to/2fE6Hns
##        Read more on Fox/FOXA here: http://lat.ms/2eDxXlp
##   2: My final list of tickers includes 101 as I manually removed BRK.B [Berkshire Hathaway] from
##      the quantmod query as Yahoo didn't appear to handle this ticker correctly and returned no data

# Read in list of data for S&P 100 Index constituents 
# CSV obtained from: http://www.cboe.com/products/indexcomponents.aspx
sp100 <- read.csv("C:/Users/jtryker/Documents/R/FIN643/sp100.csv", stringsAsFactors = FALSE)

# Set date parameters and ticker list
start_date = "2006-09-30"
end_date = "2016-09-30"
tickers <- as.character(sp100$TICKER)

# Use quantmod to query Yahoo finance for historical prices on each ticker and merge all xts tables
returns <- lapply(tickers, function(sym){ monthlyReturn(na.omit(getSymbols(sym, 
                                                                           from = start_date, 
                                                                           to = end_date, 
                                                                           auto.assign = FALSE)))})
returns_merged <- do.call(merge, returns)

# Use Quandl to obtain monthly market returns and risk-free rates from Ken French's data library
market <- Quandl("KFRENCH/FACTORS_M", start_date = start_date, end_date = end_date)
market <- as.xts(market[,-1], order.by = market[,1]) / 100
rf <- market[-1,4, drop = FALSE]
mkt <- market[-1,1, drop = FALSE]

# Ensure index placement is correct between xts tables
index(returns_merged) <- index(rf)
index(mkt) <- index(rf)

# Create matrix to store stock returns in excess of risk-free rates and convert back to xts
returns_rf <- matrix(nrow = nrow(returns_merged), ncol = ncol(returns_merged))

for (i in 1:ncol(returns_merged)) {
                returns_rf[,i] <- returns_merged[,i] - rf
}

returns_rf_xts <- xts(returns_rf, order.by = index(returns_merged))

# Compute index averages based on annualized average monthly returns and standard devations 
# as well as Sharpe ratios and market betas for each ticker
avg_er <- mean((1 + sapply(returns_rf_xts, mean, na.rm = TRUE))^12 - 1)
avg_sigma <- mean(sapply(returns_rf_xts, sd, na.rm = TRUE) * sqrt(12))
avg_sharpe <- mean(er / sigma)
avg_beta <- mean(sapply(1:ncol(returns_rf_xts), function(x) { coef(lm(returns_rf_xts[,x] ~ mkt)) } ))

# Print results
avg_er
avg_sigma
avg_sharpe
avg_beta

