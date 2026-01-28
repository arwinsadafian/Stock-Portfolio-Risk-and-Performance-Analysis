# Load packages
library(dplyr)
library(ggplot2)

# Load cleaned data
prices <- readRDS("data/processed/prices_clean.rds")
returns <- readRDS("data/processed/returns_clean.rds")

# Define tickers
tickers <- c("AAPL", "MSFT", "JPM", "JNJ", "XOM", "WMT", "DIS", "TSLA")

# Summary statistics for returns
print("Summary statistics for daily returns (%):")
summary(returns[, tickers])

# Calculate key statistics for each stock
stats <- data.frame(
  Ticker = tickers,
  Mean_Return = sapply(returns[, tickers], mean),
  Std_Dev = sapply(returns[, tickers], sd),
  Min_Return = sapply(returns[, tickers], min),
  Max_Return = sapply(returns[, tickers], max)
)

print("Return statistics by stock:")
print(stats)

# Create boxplot of returns
returns_long <- tidyr::pivot_longer(returns, 
                                    cols = all_of(tickers),
                                    names_to = "Stock",
                                    values_to = "Return")

ggplot(returns_long, aes(x = Stock, y = Return, fill = Stock)) +
  geom_boxplot() +
  labs(title = "Distribution of Daily Returns by Stock",
       x = "Stock",
       y = "Daily Return (%)") +
  theme_minimal() +
  theme(legend.position = "none")

# Save the plot
ggsave("output/figures/returns_boxplot.png", width = 10, height = 6)
print("Saved boxplot to output/figures/")

# Create price chart over time
prices_long <- tidyr::pivot_longer(prices,
                                   cols = all_of(tickers),
                                   names_to = "Stock",
                                   values_to = "Price")

ggplot(prices_long, aes(x = Date, y = Price, color = Stock)) +
  geom_line() +
  labs(title = "Stock Prices Over Time (2020-2025)",
       x = "Date",
       y = "Price ($)") +
  theme_minimal()

# Save the plot
ggsave("output/figures/price_history.png", width = 12, height = 6)
print("Saved price history to output/figures/")

# Normalize prices (all start at 100)
prices_normalized <- prices
for(col in tickers) {
  prices_normalized[[col]] <- prices[[col]] / prices[[col]][1] * 100
}

# Create normalized price chart
prices_norm_long <- tidyr::pivot_longer(prices_normalized,
                                        cols = all_of(tickers),
                                        names_to = "Stock",
                                        values_to = "Price")

ggplot(prices_norm_long, aes(x = Date, y = Price, color = Stock)) +
  geom_line() +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray") +
  labs(title = "Normalized Stock Prices (Starting at 100)",
       x = "Date",
       y = "Indexed Price (Start = 100)") +
  theme_minimal()

# Save the plot
ggsave("output/figures/price_normalized.png", width = 12, height = 6)
print("Saved normalized price chart to output/figures/")

# Calculate correlation matrix
cor_matrix <- cor(returns[, tickers])

print("Correlation matrix:")
print(round(cor_matrix, 2))

# Create correlation heatmap
library(reshape2)
cor_melted <- melt(cor_matrix)

ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Stock Return Correlations",
       x = "", y = "", fill = "Correlation") +
  theme_minimal()

# Save the plot
ggsave("output/figures/correlation_heatmap.png", width = 8, height = 6)
print("Saved correlation heatmap to output/figures/")