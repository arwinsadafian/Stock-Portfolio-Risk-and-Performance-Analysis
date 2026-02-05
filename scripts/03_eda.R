# =============================================================================
# 03_eda.R - Exploratory Data Analysis
# =============================================================================
# This script performs EDA on the stock data.
# Uses TRAINING data only to avoid look-ahead bias in exploration.
# =============================================================================

# Load configuration
source("scripts/00_config.R")

# Load packages
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(reshape2)
})

# -----------------------------------------------------------------------------
# Load Data
# -----------------------------------------------------------------------------
cat("=== Loading Data ===\n")

# Load training data for EDA (avoid look-ahead bias)
prices <- readRDS(paste0(paths$processed_data, "prices_train.rds"))
returns <- readRDS(paste0(paths$processed_data, "returns_train.rds"))

# Also load full data for context
prices_full <- readRDS(paste0(paths$processed_data, "prices_clean.rds"))
returns_full <- readRDS(paste0(paths$processed_data, "returns_clean.rds"))

cat(paste("Training data:", nrow(returns), "days\n"))
cat(paste("Date range:", min(prices$Date), "to", max(prices$Date), "\n"))

# Create output directories
dir.create(paths$figures, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Summary Statistics (Training Period Only)
# -----------------------------------------------------------------------------
cat("\n=== Summary Statistics (Training Period) ===\n")

stats <- data.frame(
  Ticker = tickers,
  Mean_Return = round(sapply(returns[, tickers], mean), 4),
  Std_Dev = round(sapply(returns[, tickers], sd), 4),
  Min_Return = round(sapply(returns[, tickers], min), 2),
  Max_Return = round(sapply(returns[, tickers], max), 2),
  Skewness = round(sapply(returns[, tickers], function(x) {
    n <- length(x)
    m <- mean(x)
    s <- sd(x)
    sum((x - m)^3) / (n * s^3)
  }), 2),
  Kurtosis = round(sapply(returns[, tickers], function(x) {
    n <- length(x)
    m <- mean(x)
    s <- sd(x)
    sum((x - m)^4) / (n * s^4) - 3  # Excess kurtosis
  }), 2)
)

# Sort by mean return
stats <- stats[order(-stats$Mean_Return), ]
print(stats)

# Save statistics
saveRDS(stats, paste0(paths$processed_data, "return_statistics.rds"))
write.csv(stats, paste0(paths$tables, "return_statistics.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# Returns Distribution Boxplot
# -----------------------------------------------------------------------------
cat("\n=== Creating Boxplot ===\n")

returns_long <- pivot_longer(returns, 
                              cols = all_of(tickers),
                              names_to = "Stock",
                              values_to = "Return")

p_boxplot <- ggplot(returns_long, aes(x = reorder(Stock, Return, FUN = sd), y = Return, fill = Stock)) +
  geom_boxplot(outlier.size = 0.5) +
  labs(title = "Distribution of Daily Returns by Stock (Training Period: 2015-2020)",
       subtitle = "Ordered by volatility (standard deviation)",
       x = "Stock",
       y = "Daily Return (%)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0(paths$figures, "returns_boxplot.png"), p_boxplot, width = 12, height = 6)
cat("Saved: returns_boxplot.png\n")

# -----------------------------------------------------------------------------
# Price History Chart
# -----------------------------------------------------------------------------
cat("\n=== Creating Price History Chart ===\n")

prices_long <- pivot_longer(prices_full,
                             cols = all_of(tickers),
                             names_to = "Stock",
                             values_to = "Price")

# Add vertical line for train/test split
split_date <- as.Date(train_end_date)

p_prices <- ggplot(prices_long, aes(x = Date, y = Price, color = Stock)) +
  geom_line(alpha = 0.7) +
  geom_vline(xintercept = split_date, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = split_date, y = max(prices_long$Price) * 0.9, 
           label = "Train/Test Split", hjust = -0.1, color = "red") +
  labs(title = paste("Stock Prices Over Time (", min(prices_full$Date), " to ", max(prices_full$Date), ")", sep = ""),
       subtitle = "Red line indicates train/test split",
       x = "Date",
       y = "Price ($)") +
  theme_minimal()

ggsave(paste0(paths$figures, "price_history.png"), p_prices, width = 14, height = 6)
cat("Saved: price_history.png\n")

# -----------------------------------------------------------------------------
# Normalized Price Charts - Top and Bottom Performers
# -----------------------------------------------------------------------------
cat("\n=== Creating Normalized Price Charts ===\n")

# Normalize prices (all start at 100)
prices_normalized <- prices_full
for (col in tickers) {
  prices_normalized[[col]] <- prices_full[[col]] / prices_full[[col]][1] * 100
}

# Calculate total return for each stock
total_returns <- sapply(tickers, function(t) {
  (prices_normalized[[t]][nrow(prices_normalized)] / 100 - 1) * 100
})
total_returns_sorted <- sort(total_returns, decreasing = TRUE)

# Get top 5 and bottom 5
top_5 <- names(total_returns_sorted)[1:5]
bottom_5 <- names(total_returns_sorted)[(length(tickers)-4):length(tickers)]

# Top performers chart
prices_top <- pivot_longer(prices_normalized,
                            cols = all_of(top_5),
                            names_to = "Stock",
                            values_to = "Price")

p_top <- ggplot(prices_top, aes(x = Date, y = Price, color = Stock)) +
  geom_line() +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = split_date, linetype = "dashed", color = "red") +
  scale_y_log10() +
  labs(title = "Top 5 Performing Stocks (Log Scale)",
       subtitle = "Normalized to 100 at start. Red line = train/test split.",
       x = "Date",
       y = "Indexed Price (Start = 100)") +
  theme_minimal()

ggsave(paste0(paths$figures, "price_normalized_top5.png"), p_top, width = 10, height = 6)
cat("Saved: price_normalized_top5.png\n")

# Bottom performers chart
prices_bottom <- pivot_longer(prices_normalized,
                               cols = all_of(bottom_5),
                               names_to = "Stock",
                               values_to = "Price")

p_bottom <- ggplot(prices_bottom, aes(x = Date, y = Price, color = Stock)) +
  geom_line() +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = split_date, linetype = "dashed", color = "red") +
  scale_y_log10() +
  labs(title = "Bottom 5 Performing Stocks (Log Scale)",
       subtitle = "Normalized to 100 at start. Red line = train/test split.",
       x = "Date",
       y = "Indexed Price (Start = 100)") +
  theme_minimal()

ggsave(paste0(paths$figures, "price_normalized_bottom5.png"), p_bottom, width = 10, height = 6)
cat("Saved: price_normalized_bottom5.png\n")

# -----------------------------------------------------------------------------
# Correlation Analysis (Training Period Only)
# -----------------------------------------------------------------------------
cat("\n=== Correlation Analysis ===\n")

cor_matrix <- cor(returns[, tickers])

# Find highest correlations (excluding diagonal)
cor_melted <- melt(cor_matrix)
cor_melted <- cor_melted[cor_melted$Var1 != cor_melted$Var2, ]
cor_melted <- cor_melted[order(-abs(cor_melted$value)), ]

cat("Top 5 highest correlations:\n")
print(head(cor_melted, 10))

# Create heatmap
p_cor <- ggplot(melt(cor_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 2) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Stock Return Correlations (Training Period: 2015-2020)",
       x = "", y = "", fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0(paths$figures, "correlation_heatmap.png"), p_cor, width = 12, height = 10)
cat("Saved: correlation_heatmap.png\n")

# Save correlation matrix
saveRDS(cor_matrix, paste0(paths$processed_data, "correlation_matrix.rds"))

# -----------------------------------------------------------------------------
# Summary Report
# -----------------------------------------------------------------------------
cat("\n=== EDA Summary ===\n")
cat(paste("Highest volatility:", stats$Ticker[which.max(stats$Std_Dev)], 
          "(SD:", max(stats$Std_Dev), ")\n"))
cat(paste("Lowest volatility:", stats$Ticker[which.min(stats$Std_Dev)], 
          "(SD:", min(stats$Std_Dev), ")\n"))
cat(paste("Highest mean return:", stats$Ticker[1], 
          "(", stats$Mean_Return[1], "%)\n"))
cat(paste("Top 5 performers:", paste(top_5, collapse = ", "), "\n"))
cat(paste("Bottom 5 performers:", paste(bottom_5, collapse = ", "), "\n"))

cat("\n=== EDA Complete ===\n")
