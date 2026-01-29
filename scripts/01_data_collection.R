# Load package
library(quantmod)

# Define stock tickers (20 stocks across different sectors)
tickers <- c(
  # Technology
  "AAPL", "MSFT", "GOOGL", "NVDA",
  # Finance
  "JPM", "V", "BAC",
  # Healthcare
  "JNJ", "PFE", "UNH",
  # Energy
  "XOM", "CVX",
  # Retail
  "WMT", "AMZN", "COST",
  # Entertainment
  "DIS", "NFLX",
  # Automotive
  "TSLA", "F",
  # Industrial
  "CAT"
  )

# Set date range (10 years of data)
start_date <- "2015-01-01"
end_date <- Sys.Date()

# Create empty list to store stock data
stock_data <- list()

# Download data for each stock
for(ticker in tickers) {
  stock_data[[ticker]] <- getSymbols(ticker, 
                                     src = "yahoo",
                                     from = start_date,
                                     to = end_date,
                                     auto.assign = FALSE)
  print(paste("Downloaded:", ticker))
}

# Download S&P 500 index (benchmark to compare against)
sp500 <- getSymbols("^GSPC", 
                    src = "yahoo",
                    from = start_date,
                    to = end_date,
                    auto.assign = FALSE)
print("Downloaded: S&P 500")

# Save each stock to a file
for(ticker in names(stock_data)) {
  saveRDS(stock_data[[ticker]], 
          file = paste0("data/raw/", ticker, ".rds"))
  print(paste("Saved:", ticker))
}

# Save S&P 500
saveRDS(sp500, "data/raw/SP500.rds")
print("Saved: S&P 500")