# Load packages
library(dplyr)
library(ggplot2)

# Load cleaned data
prices <- readRDS("data/processed/prices_clean.rds")
returns <- readRDS("data/processed/returns_clean.rds")

# Define tickers
tickers <- c("AAPL", "MSFT", "JPM", "JNJ", "XOM", "WMT", "DIS", "TSLA")

# Number of stocks
n_stocks <- length(tickers)

# Strategy 1: Equal Weight Portfolio
# Put the same amount of money in each stock
equal_weights <- rep(1/n_stocks, n_stocks)
names(equal_weights) <- tickers

print("Equal Weight Portfolio:")
print(round(equal_weights, 3))

# Strategy 2: Inverse Volatility Weight Portfolio
# Put less money in volatile stocks, more in stable stocks
volatilities <- sapply(returns[, tickers], sd)
inverse_vol <- 1 / volatilities
vol_weights <- inverse_vol / sum(inverse_vol)

print("Volatility (Standard Deviation) by stock:")
print(round(volatilities, 3))
print("Inverse Volatility Weights:")
print(round(vol_weights, 3))

# Strategy 3: Sector-Balanced Portfolio
# Tech (AAPL, MSFT, TSLA): 30% total
# Finance (JPM): 20%
# Healthcare (JNJ): 20%
# Energy (XOM): 10%
# Retail (WMT): 10%
# Entertainment (DIS): 10%

sector_weights <- c(
  AAPL = 0.10,
  MSFT = 0.10,
  JPM = 0.20,
  JNJ = 0.20,
  XOM = 0.10,
  WMT = 0.10,
  DIS = 0.10,
  TSLA = 0.10
)

print("Sector-Balanced Weights:")
print(sector_weights)
print(paste("Total:", sum(sector_weights)))

# Calculate portfolio returns for each strategy
# Portfolio return = sum of (weight * stock return) for each day

returns_matrix <- as.matrix(returns[, tickers])

# Equal weight portfolio returns
equal_returns <- returns_matrix %*% equal_weights

# Volatility weight portfolio returns
vol_returns <- returns_matrix %*% vol_weights

# Sector-balanced portfolio returns
sector_returns <- returns_matrix %*% sector_weights

# Combine into one dataframe
portfolio_returns <- data.frame(
  Date = returns$Date,
  Equal = as.numeric(equal_returns),
  Volatility = as.numeric(vol_returns),
  Sector = as.numeric(sector_returns),
  SP500 = returns$SP500
)

print("First 5 rows of portfolio returns:")
print(head(portfolio_returns, 5))

# Calculate cumulative returns (growth of $100)
portfolio_cumulative <- data.frame(
  Date = portfolio_returns$Date,
  Equal = 100 * cumprod(1 + portfolio_returns$Equal/100),
  Volatility = 100 * cumprod(1 + portfolio_returns$Volatility/100),
  Sector = 100 * cumprod(1 + portfolio_returns$Sector/100),
  SP500 = 100 * cumprod(1 + portfolio_returns$SP500/100)
)

print("Final portfolio values (starting with $100):")
print(tail(portfolio_cumulative, 1))

# Plot cumulative portfolio performance
portfolio_long <- tidyr::pivot_longer(portfolio_cumulative,
                                      cols = c(Equal, Volatility, Sector, SP500),
                                      names_to = "Portfolio",
                                      values_to = "Value")

ggplot(portfolio_long, aes(x = Date, y = Value, color = Portfolio)) +
  geom_line() +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray") +
  labs(title = "Portfolio Performance: Growth of $100",
       x = "Date",
       y = "Portfolio Value ($)") +
  theme_minimal()

# Save the plot
ggsave("output/figures/portfolio_performance.png", width = 12, height = 6)
print("Saved portfolio performance chart to output/figures/")

# Calculate performance metrics for each portfolio
calc_metrics <- function(returns) {
  annual_return <- mean(returns) * 252  # 252 trading days per year
  annual_vol <- sd(returns) * sqrt(252)
  sharpe_ratio <- annual_return / annual_vol
  
  return(c(
    Annual_Return = round(annual_return, 2),
    Annual_Volatility = round(annual_vol, 2),
    Sharpe_Ratio = round(sharpe_ratio, 2)
  ))
}

metrics <- data.frame(
  Portfolio = c("Equal", "Volatility", "Sector", "SP500"),
  rbind(
    calc_metrics(portfolio_returns$Equal),
    calc_metrics(portfolio_returns$Volatility),
    calc_metrics(portfolio_returns$Sector),
    calc_metrics(portfolio_returns$SP500)
  )
)

print("Portfolio Performance Metrics:")
print(metrics)

# Save portfolio data
saveRDS(portfolio_returns, "data/processed/portfolio_returns.rds")
saveRDS(portfolio_cumulative, "data/processed/portfolio_cumulative.rds")
saveRDS(metrics, "data/processed/portfolio_metrics.rds")

# Save metrics as CSV for easy viewing
write.csv(metrics, "output/tables/portfolio_metrics.csv", row.names = FALSE)

print("Saved portfolio data and metrics")
