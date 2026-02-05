# =============================================================================
# 04_portfolio_analysis.R - Portfolio Construction and Analysis
# =============================================================================
# This script implements proper portfolio analysis with:
# - Rolling window volatility (no look-ahead bias)
# - Periodic rebalancing with transaction costs
# - Walk-forward validation (train on 2015-2020, test on 2021-2025)
# =============================================================================

# Load configuration
source("scripts/00_config.R")

# Load packages
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
})

# -----------------------------------------------------------------------------
# Load Data
# -----------------------------------------------------------------------------
cat("=== Loading Data ===\n")

# Load full dataset
prices <- readRDS(paste0(paths$processed_data, "prices_clean.rds"))
returns <- readRDS(paste0(paths$processed_data, "returns_clean.rds"))

# Load train/test splits
returns_train <- readRDS(paste0(paths$processed_data, "returns_train.rds"))
returns_test <- readRDS(paste0(paths$processed_data, "returns_test.rds"))

cat(paste("Full period:", nrow(returns), "days\n"))
cat(paste("Training period:", nrow(returns_train), "days\n"))
cat(paste("Testing period:", nrow(returns_test), "days\n"))

n_stocks <- length(tickers)

# Create output directories
dir.create(paths$figures, recursive = TRUE, showWarnings = FALSE)
dir.create(paths$tables, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Strategy 1: Equal Weight Portfolio (Static)
# -----------------------------------------------------------------------------
cat("\n=== Strategy 1: Equal Weight ===\n")

equal_weights <- rep(1/n_stocks, n_stocks)
names(equal_weights) <- tickers

cat("Equal Weight Portfolio: ", round(equal_weights[1] * 100, 2), "% per stock\n")

# -----------------------------------------------------------------------------
# Strategy 2: Inverse Volatility with Rolling Window
# -----------------------------------------------------------------------------
cat("\n=== Strategy 2: Inverse Volatility (Rolling Window) ===\n")

# Function to calculate inverse volatility weights
calc_inverse_vol_weights <- function(returns_data, tickers) {
  volatilities <- sapply(returns_data[, tickers], sd, na.rm = TRUE)
  inverse_vol <- 1 / volatilities
  weights <- inverse_vol / sum(inverse_vol)
  names(weights) <- tickers
  return(weights)
}

# Calculate initial weights from training period
initial_vol_weights <- calc_inverse_vol_weights(returns_train, tickers)
cat("Initial weights based on training period (2015-2020):\n")
print(round(sort(initial_vol_weights, decreasing = TRUE), 4))

# -----------------------------------------------------------------------------
# Strategy 3: Sector-Balanced (from config)
# -----------------------------------------------------------------------------
cat("\n=== Strategy 3: Sector-Balanced ===\n")
cat("Sector weights:\n")
for (sector in names(sector_allocations)) {
  cat(paste(" ", sector, ":", sector_allocations[[sector]] * 100, "%\n"))
}

# -----------------------------------------------------------------------------
# Function: Calculate Portfolio Returns with Rebalancing and Transaction Costs
# -----------------------------------------------------------------------------
calc_portfolio_returns <- function(returns_data, weights_function, rebalance_freq, 
                                    trans_cost, tickers, initial_weights = NULL) {
  
  n_days <- nrow(returns_data)
  returns_matrix <- as.matrix(returns_data[, tickers])
  
  # Initialize
  portfolio_returns <- numeric(n_days)
  current_weights <- if (!is.null(initial_weights)) initial_weights else weights_function(returns_data[1:min(rolling_window, n_days), ], tickers)
  
  # Track weights and turnover
  weights_history <- list()
  turnover <- numeric(n_days)
  
  last_rebalance <- 1
  
  for (i in 1:n_days) {
    # Calculate daily return with current weights
    daily_return <- sum(returns_matrix[i, ] * current_weights)
    
    # Check if we need to rebalance
    days_since_rebalance <- i - last_rebalance
    
    if (days_since_rebalance >= rebalance_freq && i > rolling_window) {
      # Calculate new weights using rolling window
      lookback_start <- max(1, i - rolling_window)
      lookback_data <- returns_data[lookback_start:(i-1), ]
      new_weights <- weights_function(lookback_data, tickers)
      
      # Calculate turnover (sum of absolute weight changes)
      weight_turnover <- sum(abs(new_weights - current_weights))
      turnover[i] <- weight_turnover
      
      # Apply transaction costs
      trans_cost_impact <- weight_turnover * trans_cost * 100  # Convert to percentage
      daily_return <- daily_return - trans_cost_impact
      
      # Update weights
      current_weights <- new_weights
      last_rebalance <- i
    } else {
      # Drift weights based on returns (weights change naturally as prices move)
      # This is a simplification; in reality weights drift continuously
      drift_factor <- (1 + returns_matrix[i, ] / 100)
      drifted_weights <- current_weights * drift_factor
      current_weights <- drifted_weights / sum(drifted_weights)
    }
    
    portfolio_returns[i] <- daily_return
    weights_history[[i]] <- current_weights
  }
  
  return(list(
    returns = portfolio_returns,
    weights_history = weights_history,
    turnover = turnover,
    total_turnover = sum(turnover)
  ))
}

# -----------------------------------------------------------------------------
# Function: Static weights (Equal and Sector) - no rebalancing needed
# -----------------------------------------------------------------------------
calc_static_portfolio_returns <- function(returns_data, weights, trans_cost = 0) {
  returns_matrix <- as.matrix(returns_data[, names(weights)])
  portfolio_returns <- as.numeric(returns_matrix %*% weights)
  return(portfolio_returns)
}

# -----------------------------------------------------------------------------
# Calculate Portfolio Returns - TRAINING PERIOD
# -----------------------------------------------------------------------------
cat("\n=== Calculating Training Period Returns ===\n")

# Equal Weight (static)
equal_returns_train <- calc_static_portfolio_returns(returns_train, equal_weights)

# Inverse Volatility with rebalancing
vol_portfolio_train <- calc_portfolio_returns(
  returns_train, 
  calc_inverse_vol_weights, 
  rebalance_freq = rebalance_frequency,
  trans_cost = transaction_cost,
  tickers = tickers,
  initial_weights = initial_vol_weights
)

# Sector-Balanced (static)
sector_returns_train <- calc_static_portfolio_returns(returns_train, sector_weights)

# S&P 500
sp500_returns_train <- returns_train$SP500

cat("Training period calculations complete.\n")

# -----------------------------------------------------------------------------
# Calculate Portfolio Returns - TESTING PERIOD (Out-of-Sample)
# -----------------------------------------------------------------------------
cat("\n=== Calculating Testing Period Returns (Out-of-Sample) ===\n")

# Use weights learned from training period for testing
# This is the proper walk-forward approach

# Equal Weight (static - same weights)
equal_returns_test <- calc_static_portfolio_returns(returns_test, equal_weights)

# Inverse Volatility - start with weights from end of training, then rebalance
vol_portfolio_test <- calc_portfolio_returns(
  returns_test, 
  calc_inverse_vol_weights, 
  rebalance_freq = rebalance_frequency,
  trans_cost = transaction_cost,
  tickers = tickers,
  initial_weights = initial_vol_weights  # Use training period weights
)

# Sector-Balanced (static - same weights)
sector_returns_test <- calc_static_portfolio_returns(returns_test, sector_weights)

# S&P 500
sp500_returns_test <- returns_test$SP500

cat("Testing period calculations complete.\n")
cat(paste("Volatility portfolio total turnover (test):", 
          round(vol_portfolio_test$total_turnover, 2), "\n"))

# -----------------------------------------------------------------------------
# Combine Results
# -----------------------------------------------------------------------------
portfolio_returns_train <- data.frame(
  Date = returns_train$Date,
  Equal = equal_returns_train,
  Volatility = vol_portfolio_train$returns,
  Sector = sector_returns_train,
  SP500 = sp500_returns_train,
  Period = "Train"
)

portfolio_returns_test <- data.frame(
  Date = returns_test$Date,
  Equal = equal_returns_test,
  Volatility = vol_portfolio_test$returns,
  Sector = sector_returns_test,
  SP500 = sp500_returns_test,
  Period = "Test"
)

portfolio_returns_all <- rbind(portfolio_returns_train, portfolio_returns_test)

# -----------------------------------------------------------------------------
# Calculate Cumulative Returns
# -----------------------------------------------------------------------------
calc_cumulative <- function(returns_df) {
  data.frame(
    Date = returns_df$Date,
    Equal = 100 * cumprod(1 + returns_df$Equal/100),
    Volatility = 100 * cumprod(1 + returns_df$Volatility/100),
    Sector = 100 * cumprod(1 + returns_df$Sector/100),
    SP500 = 100 * cumprod(1 + returns_df$SP500/100)
  )
}

portfolio_cumulative_train <- calc_cumulative(portfolio_returns_train)
portfolio_cumulative_test <- calc_cumulative(portfolio_returns_test)

# Full period cumulative (for visualization)
portfolio_cumulative_full <- calc_cumulative(portfolio_returns_all)

cat("\n=== Final Portfolio Values ===\n")
cat("Training period ($100 invested):\n")
print(round(tail(portfolio_cumulative_train[, -1], 1), 2))
cat("\nTesting period ($100 invested):\n")
print(round(tail(portfolio_cumulative_test[, -1], 1), 2))

# -----------------------------------------------------------------------------
# Performance Metrics Function
# -----------------------------------------------------------------------------
calc_metrics <- function(returns, risk_free = risk_free_rate) {
  annual_return <- mean(returns) * 252
  annual_vol <- sd(returns) * sqrt(252)
  sharpe_ratio <- (annual_return - risk_free) / annual_vol
  
  # Maximum drawdown
  cumulative <- cumprod(1 + returns/100)
  running_max <- cummax(cumulative)
  drawdown <- (running_max - cumulative) / running_max
  max_drawdown <- max(drawdown) * 100
  
  return(c(
    Annual_Return = round(annual_return, 2),
    Annual_Volatility = round(annual_vol, 2),
    Sharpe_Ratio = round(sharpe_ratio, 2),
    Max_Drawdown = round(max_drawdown, 2)
  ))
}

# -----------------------------------------------------------------------------
# Calculate Metrics for Both Periods
# -----------------------------------------------------------------------------
cat("\n=== Performance Metrics ===\n")

# Training metrics
metrics_train <- data.frame(
  Portfolio = c("Equal", "Volatility", "Sector", "SP500"),
  Period = "Train",
  rbind(
    calc_metrics(portfolio_returns_train$Equal),
    calc_metrics(portfolio_returns_train$Volatility),
    calc_metrics(portfolio_returns_train$Sector),
    calc_metrics(portfolio_returns_train$SP500)
  )
)

# Testing metrics (OUT-OF-SAMPLE)
metrics_test <- data.frame(
  Portfolio = c("Equal", "Volatility", "Sector", "SP500"),
  Period = "Test",
  rbind(
    calc_metrics(portfolio_returns_test$Equal),
    calc_metrics(portfolio_returns_test$Volatility),
    calc_metrics(portfolio_returns_test$Sector),
    calc_metrics(portfolio_returns_test$SP500)
  )
)

metrics_all <- rbind(metrics_train, metrics_test)

cat("\nTraining Period Metrics (2015-2020):\n")
print(metrics_train[, -2])

cat("\nTesting Period Metrics (2021-2025) - OUT-OF-SAMPLE:\n")
print(metrics_test[, -2])

# -----------------------------------------------------------------------------
# Visualization
# -----------------------------------------------------------------------------
cat("\n=== Creating Visualizations ===\n")

# Portfolio performance with train/test split
portfolio_long <- pivot_longer(portfolio_cumulative_full,
                                cols = c(Equal, Volatility, Sector, SP500),
                                names_to = "Portfolio",
                                values_to = "Value")

split_date <- as.Date(train_end_date)

p_performance <- ggplot(portfolio_long, aes(x = Date, y = Value, color = Portfolio, linetype = Portfolio)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = split_date, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = split_date - 100, y = max(portfolio_long$Value) * 0.95, 
           label = "Training", hjust = 1, color = "darkgreen", fontface = "bold") +
  annotate("text", x = split_date + 100, y = max(portfolio_long$Value) * 0.95, 
           label = "Testing (Out-of-Sample)", hjust = 0, color = "darkblue", fontface = "bold") +
  labs(title = "Portfolio Performance: Growth of $100",
       subtitle = paste("Training: 2015-2020 | Testing: 2021-2025 | Rebalancing:", 
                        rebalance_frequency, "days | Transaction cost:", 
                        transaction_cost * 100, "%"),
       x = "Date",
       y = "Portfolio Value ($)") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(paste0(paths$figures, "portfolio_performance.png"), p_performance, width = 12, height = 7)
cat("Saved: portfolio_performance.png\n")

# Metrics comparison bar chart
metrics_long <- pivot_longer(metrics_all, 
                              cols = c(Annual_Return, Sharpe_Ratio),
                              names_to = "Metric",
                              values_to = "Value")

p_metrics <- ggplot(metrics_long, aes(x = Portfolio, y = Value, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Metric, scales = "free_y") +
  labs(title = "Portfolio Performance: Training vs Testing Period",
       subtitle = "Testing period is out-of-sample validation",
       x = "Portfolio",
       y = "Value") +
  theme_minimal() +
  scale_fill_manual(values = c("Train" = "darkgreen", "Test" = "darkblue"))

ggsave(paste0(paths$figures, "metrics_comparison.png"), p_metrics, width = 10, height = 5)
cat("Saved: metrics_comparison.png\n")

# -----------------------------------------------------------------------------
# Save Results
# -----------------------------------------------------------------------------
cat("\n=== Saving Results ===\n")

saveRDS(portfolio_returns_all, paste0(paths$processed_data, "portfolio_returns.rds"))
saveRDS(portfolio_cumulative_full, paste0(paths$processed_data, "portfolio_cumulative.rds"))
saveRDS(metrics_all, paste0(paths$processed_data, "portfolio_metrics.rds"))

# Save separate train/test metrics
saveRDS(metrics_train, paste0(paths$processed_data, "metrics_train.rds"))
saveRDS(metrics_test, paste0(paths$processed_data, "metrics_test.rds"))

# CSV exports
write.csv(metrics_train, paste0(paths$tables, "portfolio_metrics_train.csv"), row.names = FALSE)
write.csv(metrics_test, paste0(paths$tables, "portfolio_metrics_test.csv"), row.names = FALSE)

cat("Saved all portfolio data and metrics.\n")

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------
cat("\n=== Portfolio Analysis Summary ===\n")
cat("Key findings (OUT-OF-SAMPLE testing period 2021-2025):\n")

best_return <- metrics_test$Portfolio[which.max(metrics_test$Annual_Return)]
best_sharpe <- metrics_test$Portfolio[which.max(metrics_test$Sharpe_Ratio)]

cat(paste("- Best annual return:", best_return, 
          "(", max(metrics_test$Annual_Return), "%)\n"))
cat(paste("- Best risk-adjusted return (Sharpe):", best_sharpe, 
          "(", max(metrics_test$Sharpe_Ratio), ")\n"))
cat(paste("- Transaction costs applied:", transaction_cost * 100, "% per trade\n"))
cat(paste("- Rebalancing frequency:", rebalance_frequency, "trading days\n"))

cat("\n=== Portfolio Analysis Complete ===\n")
