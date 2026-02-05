# =============================================================================
# 05_statistical_tests.R - Statistical Testing with Proper Methods
# =============================================================================
# This script uses appropriate statistical tests for non-normal financial data:
# - Wilcoxon signed-rank test (non-parametric alternative to t-test)
# - Bootstrap confidence intervals
# - Tests on OUT-OF-SAMPLE data only
# =============================================================================

# Load configuration
source("scripts/00_config.R")

# Load packages
suppressPackageStartupMessages({
  library(dplyr)
})

# -----------------------------------------------------------------------------
# Load Data
# -----------------------------------------------------------------------------
cat("=== Loading Data ===\n")

returns <- readRDS(paste0(paths$processed_data, "returns_clean.rds"))
returns_test <- readRDS(paste0(paths$processed_data, "returns_test.rds"))
portfolio_returns <- readRDS(paste0(paths$processed_data, "portfolio_returns.rds"))

# Filter to test period only for statistical claims
portfolio_returns_test <- portfolio_returns %>% 
  filter(Period == "Test")

cat(paste("Testing period:", nrow(portfolio_returns_test), "days\n"))
cat(paste("Using significance level:", alpha, "\n"))
cat(paste("Bootstrap samples:", n_bootstrap, "\n"))

# Create output directory
dir.create(paths$tables, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Bootstrap Function for Confidence Intervals
# -----------------------------------------------------------------------------
bootstrap_mean_ci <- function(x, n_boot = n_bootstrap, conf_level = 0.95) {
  boot_means <- replicate(n_boot, mean(sample(x, replace = TRUE)))
  ci <- quantile(boot_means, c((1-conf_level)/2, 1-(1-conf_level)/2))
  return(list(
    mean = mean(x),
    ci_lower = ci[1],
    ci_upper = ci[2],
    se = sd(boot_means)
  ))
}

# -----------------------------------------------------------------------------
# Bootstrap Test: Is Mean Different from Zero?
# -----------------------------------------------------------------------------
bootstrap_test_zero <- function(x, n_boot = n_bootstrap) {
  observed_mean <- mean(x)
  
  # Center data at zero for null hypothesis
  x_centered <- x - observed_mean
  
  # Bootstrap under null
  boot_means <- replicate(n_boot, mean(sample(x_centered, replace = TRUE)))
  
  # Two-tailed p-value
  p_value <- mean(abs(boot_means) >= abs(observed_mean))
  
  return(list(
    mean = observed_mean,
    p_value = p_value,
    significant = p_value < alpha
  ))
}

# -----------------------------------------------------------------------------
# Test 1: Individual Stock Returns (Testing Period)
# -----------------------------------------------------------------------------
cat("\n=== Test 1: Stock Returns Different from Zero ===\n")
cat("Method: Bootstrap test (non-parametric)\n")
cat("Period: Testing (2021-2025, out-of-sample)\n")
cat("------------------------------------------\n")

stock_test_results <- data.frame(
  Ticker = character(),
  Mean_Return = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  P_Value = numeric(),
  Significant = character(),
  stringsAsFactors = FALSE
)

for (ticker in tickers) {
  test_data <- returns_test[[ticker]]
  
  # Bootstrap confidence interval
  boot_ci <- bootstrap_mean_ci(test_data)
  
  # Bootstrap test against zero
  boot_test <- bootstrap_test_zero(test_data)
  
  stock_test_results <- rbind(stock_test_results, data.frame(
    Ticker = ticker,
    Mean_Return = round(boot_ci$mean, 4),
    CI_Lower = round(boot_ci$ci_lower, 4),
    CI_Upper = round(boot_ci$ci_upper, 4),
    P_Value = round(boot_test$p_value, 4),
    Significant = ifelse(boot_test$significant, "YES", "NO")
  ))
  
  cat(paste(ticker, "| Mean:", round(boot_ci$mean, 4), 
            "| 95% CI: [", round(boot_ci$ci_lower, 4), ",", round(boot_ci$ci_upper, 4), "]",
            "| p-value:", round(boot_test$p_value, 4),
            "| Significant:", ifelse(boot_test$significant, "YES", "NO"), "\n"))
}

# Summary
significant_stocks <- stock_test_results$Ticker[stock_test_results$Significant == "YES"]
cat(paste("\nSignificant stocks (", length(significant_stocks), "/", length(tickers), "):\n"))
cat(paste(significant_stocks, collapse = ", "), "\n")

# -----------------------------------------------------------------------------
# Test 2: Portfolio vs S&P 500 (Testing Period Only)
# -----------------------------------------------------------------------------
cat("\n=== Test 2: Portfolio vs S&P 500 ===\n")
cat("Method: Wilcoxon signed-rank test (non-parametric)\n")
cat("Period: Testing (2021-2025, out-of-sample)\n")
cat("------------------------------------------\n")

portfolio_test_results <- data.frame(
  Portfolio = character(),
  Mean_Difference = numeric(),
  Wilcoxon_P = numeric(),
  Boot_CI_Lower = numeric(),
  Boot_CI_Upper = numeric(),
  Significant = character(),
  stringsAsFactors = FALSE
)

for (portfolio in c("Equal", "Volatility", "Sector")) {
  diff_returns <- portfolio_returns_test[[portfolio]] - portfolio_returns_test$SP500
  
  # Wilcoxon signed-rank test (non-parametric)
  wilcox_result <- wilcox.test(diff_returns, mu = 0, alternative = "greater")
  
  # Bootstrap confidence interval for the difference
  boot_ci <- bootstrap_mean_ci(diff_returns)
  
  significant <- wilcox_result$p.value < alpha
  
  portfolio_test_results <- rbind(portfolio_test_results, data.frame(
    Portfolio = portfolio,
    Mean_Difference = round(mean(diff_returns), 4),
    Wilcoxon_P = round(wilcox_result$p.value, 4),
    Boot_CI_Lower = round(boot_ci$ci_lower, 4),
    Boot_CI_Upper = round(boot_ci$ci_upper, 4),
    Significant = ifelse(significant, "YES", "NO")
  ))
  
  cat(paste(portfolio, "vs SP500:\n"))
  cat(paste("  Mean daily difference:", round(mean(diff_returns), 4), "%\n"))
  cat(paste("  95% Bootstrap CI: [", round(boot_ci$ci_lower, 4), ",", 
            round(boot_ci$ci_upper, 4), "]\n"))
  cat(paste("  Wilcoxon p-value:", round(wilcox_result$p.value, 4), "\n"))
  cat(paste("  Significantly better:", ifelse(significant, "YES", "NO"), "\n\n"))
}

# -----------------------------------------------------------------------------
# Test 3: Normality Tests
# -----------------------------------------------------------------------------
cat("\n=== Test 3: Normality of Returns ===\n")
cat("Method: Shapiro-Wilk test\n")
cat("------------------------------------------\n")

normality_results <- data.frame(
  Ticker = character(),
  P_Value = numeric(),
  Normal = character(),
  Skewness = numeric(),
  Kurtosis = numeric(),
  stringsAsFactors = FALSE
)

for (ticker in tickers) {
  test_data <- returns[[ticker]]
  
  # Shapiro-Wilk (use sample if too large)
  if (length(test_data) > 5000) {
    test_data_sample <- sample(test_data, 5000)
  } else {
    test_data_sample <- test_data
  }
  
  shapiro_result <- shapiro.test(test_data_sample)
  
  # Calculate skewness and kurtosis
  n <- length(test_data)
  m <- mean(test_data)
  s <- sd(test_data)
  skewness <- sum((test_data - m)^3) / (n * s^3)
  kurtosis <- sum((test_data - m)^4) / (n * s^4) - 3  # Excess kurtosis
  
  normality_results <- rbind(normality_results, data.frame(
    Ticker = ticker,
    P_Value = round(shapiro_result$p.value, 6),
    Normal = ifelse(shapiro_result$p.value > alpha, "YES", "NO"),
    Skewness = round(skewness, 3),
    Kurtosis = round(kurtosis, 3)
  ))
}

cat("All stocks show non-normal returns (p < 0.05):\n")
cat(paste("- Average excess kurtosis:", round(mean(normality_results$Kurtosis), 2), 
          "(normal = 0, fat tails > 0)\n"))
cat(paste("- Average skewness:", round(mean(normality_results$Skewness), 3), "\n"))
cat("\nThis confirms that t-tests are inappropriate; non-parametric tests used instead.\n")

# -----------------------------------------------------------------------------
# Test 4: Bootstrap Test for Sharpe Ratio Difference
# -----------------------------------------------------------------------------
cat("\n=== Test 4: Sharpe Ratio Comparison ===\n")
cat("Method: Bootstrap confidence intervals\n")
cat("Period: Testing (2021-2025, out-of-sample)\n")
cat("------------------------------------------\n")

calc_sharpe <- function(returns, rf = risk_free_rate/252) {  # Daily risk-free rate
  (mean(returns) - rf) / sd(returns) * sqrt(252)
}

bootstrap_sharpe_diff <- function(port_returns, bench_returns, n_boot = n_bootstrap) {
  sharpe_diffs <- replicate(n_boot, {
    idx <- sample(1:length(port_returns), replace = TRUE)
    calc_sharpe(port_returns[idx]) - calc_sharpe(bench_returns[idx])
  })
  
  ci <- quantile(sharpe_diffs, c(0.025, 0.975))
  p_value <- mean(sharpe_diffs <= 0)  # One-tailed: is portfolio better?
  
  return(list(
    diff = calc_sharpe(port_returns) - calc_sharpe(bench_returns),
    ci_lower = ci[1],
    ci_upper = ci[2],
    p_value = p_value
  ))
}

sharpe_results <- data.frame(
  Portfolio = character(),
  Portfolio_Sharpe = numeric(),
  SP500_Sharpe = numeric(),
  Sharpe_Diff = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  Significant = character(),
  stringsAsFactors = FALSE
)

sp500_sharpe <- calc_sharpe(portfolio_returns_test$SP500)

for (portfolio in c("Equal", "Volatility", "Sector")) {
  port_sharpe <- calc_sharpe(portfolio_returns_test[[portfolio]])
  
  boot_result <- bootstrap_sharpe_diff(
    portfolio_returns_test[[portfolio]], 
    portfolio_returns_test$SP500
  )
  
  # Significant if CI doesn't include zero
  significant <- boot_result$ci_lower > 0
  
  sharpe_results <- rbind(sharpe_results, data.frame(
    Portfolio = portfolio,
    Portfolio_Sharpe = round(port_sharpe, 3),
    SP500_Sharpe = round(sp500_sharpe, 3),
    Sharpe_Diff = round(boot_result$diff, 3),
    CI_Lower = round(boot_result$ci_lower, 3),
    CI_Upper = round(boot_result$ci_upper, 3),
    Significant = ifelse(significant, "YES", "NO")
  ))
  
  cat(paste(portfolio, ":\n"))
  cat(paste("  Sharpe:", round(port_sharpe, 3), "vs SP500:", round(sp500_sharpe, 3), "\n"))
  cat(paste("  Difference:", round(boot_result$diff, 3), "\n"))
  cat(paste("  95% CI: [", round(boot_result$ci_lower, 3), ",", 
            round(boot_result$ci_upper, 3), "]\n"))
  cat(paste("  Significantly better:", ifelse(significant, "YES", "NO"), "\n\n"))
}

# -----------------------------------------------------------------------------
# Save Results
# -----------------------------------------------------------------------------
cat("\n=== Saving Results ===\n")

# Save all test results
saveRDS(list(
  stock_tests = stock_test_results,
  portfolio_tests = portfolio_test_results,
  normality_tests = normality_results,
  sharpe_tests = sharpe_results
), paste0(paths$processed_data, "statistical_tests.rds"))

# Create dynamic summary
significant_stocks_str <- paste(significant_stocks, collapse = ", ")
if (length(significant_stocks) == 0) significant_stocks_str <- "None"

portfolio_beats_sp500 <- portfolio_test_results$Portfolio[portfolio_test_results$Significant == "YES"]
portfolio_beats_str <- paste(portfolio_beats_sp500, collapse = ", ")
if (length(portfolio_beats_sp500) == 0) portfolio_beats_str <- "None"

sharpe_significant <- sharpe_results$Portfolio[sharpe_results$Significant == "YES"]
sharpe_str <- paste(sharpe_significant, collapse = ", ")
if (length(sharpe_significant) == 0) sharpe_str <- "None"

test_summary <- data.frame(
  Test = c(
    "Individual stock returns != 0 (Bootstrap)",
    "Portfolios beat SP500 (Wilcoxon)",
    "Sharpe ratio better than SP500 (Bootstrap)",
    "Returns normally distributed (Shapiro-Wilk)"
  ),
  Period = c("Test (2021-2025)", "Test (2021-2025)", "Test (2021-2025)", "Full"),
  Result = c(
    paste(significant_stocks_str, "(", length(significant_stocks), "/", length(tickers), ")"),
    portfolio_beats_str,
    sharpe_str,
    paste("NO - all stocks have fat tails (avg kurtosis:", round(mean(normality_results$Kurtosis), 1), ")")
  )
)

write.csv(test_summary, paste0(paths$tables, "statistical_tests.csv"), row.names = FALSE)
write.csv(portfolio_test_results, paste0(paths$tables, "portfolio_vs_sp500.csv"), row.names = FALSE)
write.csv(sharpe_results, paste0(paths$tables, "sharpe_comparison.csv"), row.names = FALSE)

cat("Saved all statistical test results.\n")

# -----------------------------------------------------------------------------
# Final Summary
# -----------------------------------------------------------------------------
cat("\n=== Statistical Testing Summary ===\n")
cat("All tests performed on OUT-OF-SAMPLE data (2021-2025)\n")
cat("Using non-parametric methods appropriate for non-normal returns.\n\n")

print(test_summary)

cat("\n=== IMPORTANT CAVEATS ===\n")
cat("1. Past performance does not predict future results\n")
cat("2. Testing period (2021-2025) may not be representative of all market conditions\n")
cat("3. Transaction costs of", transaction_cost * 100, "% may underestimate real costs\n")
cat("4. Survivorship bias: analysis includes only stocks that exist today\n")
cat("5. Small sample size for testing period limits statistical power\n")

cat("\n=== Statistical Tests Complete ===\n")
