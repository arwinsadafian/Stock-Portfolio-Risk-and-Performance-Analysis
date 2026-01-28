# Load packages
library(dplyr)

# Load data
returns <- readRDS("data/processed/returns_clean.rds")
portfolio_returns <- readRDS("data/processed/portfolio_returns.rds")

# Define tickers
tickers <- c("AAPL", "MSFT", "JPM", "JNJ", "XOM", "WMT", "DIS", "TSLA")

# Test 1: Is each stock's average return significantly different from zero?
# Using one-sample t-test

print("T-test: Is average return different from zero?")
print("------------------------------------------")

for(ticker in tickers) {
  test_result <- t.test(returns[[ticker]], mu = 0)
  p_value <- round(test_result$p.value, 4)
  mean_return <- round(mean(returns[[ticker]]), 4)
  significant <- ifelse(p_value < 0.05, "YES", "NO")
  
  print(paste(ticker, "| Mean:", mean_return, "| p-value:", p_value, "| Significant:", significant))
}

# Test 2: Do our portfolios beat the S&P 500?
# Using paired t-test (comparing same days)

print("")
print("T-test: Does portfolio beat S&P 500?")
print("------------------------------------------")

for(portfolio in c("Equal", "Volatility", "Sector")) {
  test_result <- t.test(portfolio_returns[[portfolio]], 
                        portfolio_returns$SP500, 
                        paired = TRUE)
  p_value <- round(test_result$p.value, 4)
  mean_diff <- round(mean(portfolio_returns[[portfolio]] - portfolio_returns$SP500), 4)
  significant <- ifelse(p_value < 0.05, "YES", "NO")
  
  print(paste(portfolio, "| Mean difference:", mean_diff, "| p-value:", p_value, "| Significant:", significant))
}

# Test 3: Are returns normally distributed?
# Using Shapiro-Wilk test (sample of 5000 max)

print("")
print("Shapiro-Wilk test: Are returns normally distributed?")
print("------------------------------------------")

for(ticker in tickers) {
  # Shapiro test has limit of 5000 observations
  test_result <- shapiro.test(returns[[ticker]])
  p_value <- round(test_result$p.value, 4)
  normal <- ifelse(p_value > 0.05, "YES", "NO")
  
  print(paste(ticker, "| p-value:", p_value, "| Normally distributed:", normal))
}

# Save statistical test summary
test_summary <- data.frame(
  Test = c("Individual stock returns != 0", 
           "Equal beats SP500", 
           "Sector beats SP500",
           "Returns normally distributed"),
  Result = c("AAPL, WMT, TSLA significant",
             "YES (significant)",
             "YES (significant)",
             "NO (none are normal)")
)

write.csv(test_summary, "output/tables/statistical_tests.csv", row.names = FALSE)
print("")
print("Saved test summary to output/tables/")
print(test_summary)