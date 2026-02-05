# =============================================================================
# 02_data_cleaning.R - Clean and Prepare Data for Analysis
# =============================================================================
# This script extracts adjusted close prices and calculates returns.
# It also splits data into training and testing periods.
# =============================================================================

# Load configuration
source("scripts/00_config.R")

# Load packages
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

# -----------------------------------------------------------------------------
# Load Raw Data
# -----------------------------------------------------------------------------
cat("=== Loading Raw Data ===\n")

# Load valid tickers (in case some failed during download)
if (file.exists(paste0(paths$processed_data, "valid_tickers.rds"))) {
  tickers <- readRDS(paste0(paths$processed_data, "valid_tickers.rds"))
}

stock_data <- list()
for (ticker in tickers) {
  file_path <- paste0(paths$raw_data, ticker, ".rds")
  if (file.exists(file_path)) {
    stock_data[[ticker]] <- readRDS(file_path)
    cat(paste("Loaded:", ticker, "\n"))
  } else {
    cat(paste("WARNING: File not found for", ticker, "\n"))
  }
}

# Load S&P 500
sp500 <- readRDS(paste0(paths$raw_data, "SP500.rds"))
cat("Loaded: S&P 500\n")

# -----------------------------------------------------------------------------
# Extract Adjusted Close Prices
# -----------------------------------------------------------------------------
cat("\n=== Extracting Adjusted Close Prices ===\n")

# Create prices dataframe
prices <- data.frame(Date = index(stock_data[[1]]))

for (ticker in tickers) {
  adjusted_col <- paste0(ticker, ".Adjusted")
  if (adjusted_col %in% colnames(stock_data[[ticker]])) {
    prices[[ticker]] <- as.numeric(stock_data[[ticker]][, adjusted_col])
  } else {
    cat(paste("WARNING: No adjusted close for", ticker, "\n"))
  }
}

# Add S&P 500
prices$SP500 <- as.numeric(sp500[, "GSPC.Adjusted"])

# Check for missing values
cat("\nMissing values per column:\n")
missing_counts <- colSums(is.na(prices))
print(missing_counts[missing_counts > 0])

# -----------------------------------------------------------------------------
# Calculate Daily Returns
# -----------------------------------------------------------------------------
cat("\n=== Calculating Daily Returns ===\n")

returns <- data.frame(Date = prices$Date[-1])

for (col in c(tickers, "SP500")) {
  price_col <- prices[[col]]
  # Calculate percentage returns
  returns[[col]] <- diff(price_col) / price_col[-length(price_col)] * 100
}

# Check for missing values in returns
cat("Missing values in returns:\n")
missing_returns <- colSums(is.na(returns))
print(missing_returns[missing_returns > 0])

# -----------------------------------------------------------------------------
# Remove Missing Values
# -----------------------------------------------------------------------------
prices_clean <- na.omit(prices)
returns_clean <- na.omit(returns)

cat("\n=== Data Summary ===\n")
cat(paste("Original price rows:", nrow(prices), "\n"))
cat(paste("Clean price rows:", nrow(prices_clean), "\n"))
cat(paste("Original return rows:", nrow(returns), "\n"))
cat(paste("Clean return rows:", nrow(returns_clean), "\n"))

# -----------------------------------------------------------------------------
# Split into Training and Testing Periods
# -----------------------------------------------------------------------------
cat("\n=== Splitting Data for Walk-Forward Validation ===\n")

# Training period: start_date to train_end_date
prices_train <- prices_clean %>% 
  filter(Date <= as.Date(train_end_date))

returns_train <- returns_clean %>% 
  filter(Date <= as.Date(train_end_date))

# Testing period: test_start_date to end_date
prices_test <- prices_clean %>% 
  filter(Date >= as.Date(test_start_date))

returns_test <- returns_clean %>% 
  filter(Date >= as.Date(test_start_date))

cat(paste("Training period:", min(prices_train$Date), "to", max(prices_train$Date), "\n"))
cat(paste("Training days:", nrow(returns_train), "\n"))
cat(paste("Testing period:", min(prices_test$Date), "to", max(prices_test$Date), "\n"))
cat(paste("Testing days:", nrow(returns_test), "\n"))

# -----------------------------------------------------------------------------
# Save Cleaned Data
# -----------------------------------------------------------------------------
cat("\n=== Saving Cleaned Data ===\n")

# Create directories if needed
dir.create(paths$processed_data, recursive = TRUE, showWarnings = FALSE)

# Full dataset
saveRDS(prices_clean, paste0(paths$processed_data, "prices_clean.rds"))
saveRDS(returns_clean, paste0(paths$processed_data, "returns_clean.rds"))

# Training dataset
saveRDS(prices_train, paste0(paths$processed_data, "prices_train.rds"))
saveRDS(returns_train, paste0(paths$processed_data, "returns_train.rds"))

# Testing dataset
saveRDS(prices_test, paste0(paths$processed_data, "prices_test.rds"))
saveRDS(returns_test, paste0(paths$processed_data, "returns_test.rds"))

cat("Saved all cleaned data to", paths$processed_data, "\n")

# -----------------------------------------------------------------------------
# Save Data Summary
# -----------------------------------------------------------------------------
data_summary <- data.frame(
  Dataset = c("Full", "Training", "Testing"),
  Start_Date = c(min(prices_clean$Date), min(prices_train$Date), min(prices_test$Date)),
  End_Date = c(max(prices_clean$Date), max(prices_train$Date), max(prices_test$Date)),
  Trading_Days = c(nrow(returns_clean), nrow(returns_train), nrow(returns_test)),
  Stocks = rep(length(tickers), 3)
)

saveRDS(data_summary, paste0(paths$processed_data, "data_summary.rds"))
print(data_summary)

cat("\n=== Data Cleaning Complete ===\n")
