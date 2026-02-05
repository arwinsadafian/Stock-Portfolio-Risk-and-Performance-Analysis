# =============================================================================
# 00_config.R - Shared Configuration for Portfolio Analysis
# =============================================================================
# This file contains all shared variables and settings used across scripts.
# Source this file at the beginning of each script.
# =============================================================================

# -----------------------------------------------------------------------------
# Stock Tickers by Sector
# -----------------------------------------------------------------------------
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

# Sector mappings for sector-balanced portfolio
sector_mapping <- list(
  Technology = c("AAPL", "MSFT", "GOOGL", "NVDA"),
  Finance = c("JPM", "V", "BAC"),
  Healthcare = c("JNJ", "PFE", "UNH"),
  Energy = c("XOM", "CVX"),
  Retail = c("WMT", "AMZN", "COST"),
  Entertainment = c("DIS", "NFLX"),
  Automotive = c("TSLA", "F"),
  Industrial = c("CAT")
)

# Sector allocation percentages
sector_allocations <- c(
  Technology = 0.25,
  Finance = 0.15,
  Healthcare = 0.15,
  Energy = 0.10,
  Retail = 0.15,
  Entertainment = 0.10,
  Automotive = 0.05,
  Industrial = 0.05
)

# -----------------------------------------------------------------------------
# Date Configuration (Fixed for Reproducibility)
# -----------------------------------------------------------------------------
start_date <- "2015-01-01"
end_date <- "2025-01-27"  # Fixed date for reproducibility

# Train/Test Split for Walk-Forward Validation
train_end_date <- "2020-12-31"
test_start_date <- "2021-01-01"

# -----------------------------------------------------------------------------
# Portfolio Parameters
# -----------------------------------------------------------------------------
# Rolling window for volatility calculation (trading days)
rolling_window <- 252  # 1 year

# Rebalancing frequency (trading days)
rebalance_frequency <- 63  # Quarterly (~63 trading days)

# Transaction cost per trade (as decimal, e.g., 0.001 = 0.1%)
transaction_cost <- 0.001

# Risk-free rate for Sharpe ratio (annualized)
risk_free_rate <- 0.02  # 2% annual

# -----------------------------------------------------------------------------
# Statistical Testing Parameters
# -----------------------------------------------------------------------------
# Number of bootstrap samples
n_bootstrap <- 10000

# Significance level
alpha <- 0.05

# -----------------------------------------------------------------------------
# Helper Function: Calculate Sector Weights
# -----------------------------------------------------------------------------
calculate_sector_weights <- function(sector_mapping, sector_allocations) {
  weights <- numeric(length(unlist(sector_mapping)))
  names(weights) <- unlist(sector_mapping)
  
  for (sector in names(sector_mapping)) {
    stocks_in_sector <- sector_mapping[[sector]]
    weight_per_stock <- sector_allocations[[sector]] / length(stocks_in_sector)
    weights[stocks_in_sector] <- weight_per_stock
  }
  
  return(weights)
}

# Pre-calculate sector weights
sector_weights <- calculate_sector_weights(sector_mapping, sector_allocations)

# -----------------------------------------------------------------------------
# File Paths
# -----------------------------------------------------------------------------
paths <- list(
  raw_data = "data/raw/",
  processed_data = "data/processed/",
  figures = "output/figures/",
  tables = "output/tables/"
)

# -----------------------------------------------------------------------------
# Print Configuration Summary
# -----------------------------------------------------------------------------
print_config <- function() {
  cat("=== Portfolio Analysis Configuration ===\n")
  cat(paste("Stocks:", length(tickers), "\n"))
  cat(paste("Date Range:", start_date, "to", end_date, "\n"))
  cat(paste("Training Period:", start_date, "to", train_end_date, "\n"))
  cat(paste("Testing Period:", test_start_date, "to", end_date, "\n"))
  cat(paste("Rolling Window:", rolling_window, "days\n"))
  cat(paste("Rebalance Frequency:", rebalance_frequency, "days\n"))
  cat(paste("Transaction Cost:", transaction_cost * 100, "%\n"))
  cat(paste("Bootstrap Samples:", n_bootstrap, "\n"))
  cat("=========================================\n")
}
