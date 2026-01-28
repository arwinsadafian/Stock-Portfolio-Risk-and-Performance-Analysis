# Load packages
library(dplyr)
library(tidyr)
library(lubridate)

# Define tickers (same as before)
tickers <- c("AAPL", "MSFT", "JPM", "JNJ", "XOM", "WMT", "DIS", "TSLA")

# Load all stock data from files
stock_data <- list()
for(ticker in tickers) {
  stock_data[[ticker]] <- readRDS(paste0("data/raw/", ticker, ".rds"))
}

# Load S&P 500
sp500 <- readRDS("data/raw/SP500.rds")

# Extract only the Adjusted Close price from each stock
# Adjusted Close accounts for dividends and stock splits
prices <- data.frame(Date = index(stock_data[[1]]))

for(ticker in tickers) {
  adjusted_col <- paste0(ticker, ".Adjusted")
  prices[[ticker]] <- as.numeric(stock_data[[ticker]][, adjusted_col])
}

# Add S&P 500
prices$SP500 <- as.numeric(sp500[, "GSPC.Adjusted"])

# Check for missing values
print("Missing values per column:")
print(colSums(is.na(prices)))

# Calculate daily returns (percentage change from previous day)
returns <- data.frame(Date = prices$Date[-1])

for(col in c(tickers, "SP500")) {
  price_col <- prices[[col]]
  returns[[col]] <- diff(price_col) / price_col[-length(price_col)] * 100
}

# Check the first few rows
print("First 5 rows of returns:")
print(head(returns, 5))

# Check for missing values in returns
print("Missing values in returns:")
print(colSums(is.na(returns)))

# Remove rows with any missing values
prices_clean <- na.omit(prices)
returns_clean <- na.omit(returns)

# Print summary
print(paste("Original price rows:", nrow(prices)))
print(paste("Clean price rows:", nrow(prices_clean)))
print(paste("Original return rows:", nrow(returns)))
print(paste("Clean return rows:", nrow(returns_clean)))

# Save cleaned data
saveRDS(prices_clean, "data/processed/prices_clean.rds")
saveRDS(returns_clean, "data/processed/returns_clean.rds")

print("Saved cleaned data to data/processed/")