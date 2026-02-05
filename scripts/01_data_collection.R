# =============================================================================
# 01_data_collection.R - Download Stock Data from Yahoo Finance
# =============================================================================
# This script downloads historical stock data with proper error handling.
# =============================================================================

# Load configuration
source("scripts/00_config.R")

# Load package
library(quantmod)

# Print configuration
print_config()

# -----------------------------------------------------------------------------
# Function: Safe Download with Error Handling
# -----------------------------------------------------------------------------
safe_download <- function(ticker, src, from, to, max_retries = 3) {
  for (attempt in 1:max_retries) {
    result <- tryCatch({
      data <- getSymbols(ticker, 
                         src = src,
                         from = from,
                         to = to,
                         auto.assign = FALSE)
      return(list(success = TRUE, data = data, error = NULL))
    }, error = function(e) {
      return(list(success = FALSE, data = NULL, error = e$message))
    })
    
    if (result$success) {
      return(result)
    } else {
      cat(paste("Attempt", attempt, "failed for", ticker, ":", result$error, "\n"))
      if (attempt < max_retries) {
        Sys.sleep(2)  # Wait before retry
      }
    }
  }
  return(result)
}

# -----------------------------------------------------------------------------
# Download Stock Data
# -----------------------------------------------------------------------------
stock_data <- list()
failed_downloads <- c()

cat("\n=== Downloading Stock Data ===\n")
for (ticker in tickers) {
  result <- safe_download(ticker, "yahoo", start_date, end_date)
  
  if (result$success) {
    stock_data[[ticker]] <- result$data
    cat(paste("✓ Downloaded:", ticker, "\n"))
  } else {
    failed_downloads <- c(failed_downloads, ticker)
    cat(paste("✗ FAILED:", ticker, "-", result$error, "\n"))
  }
}

# -----------------------------------------------------------------------------
# Download S&P 500 Benchmark
# -----------------------------------------------------------------------------
cat("\n=== Downloading Benchmark ===\n")
sp500_result <- safe_download("^GSPC", "yahoo", start_date, end_date)

if (sp500_result$success) {
  sp500 <- sp500_result$data
  cat("✓ Downloaded: S&P 500\n")
} else {
  stop("CRITICAL: Failed to download S&P 500 benchmark. Cannot proceed.")
}

# -----------------------------------------------------------------------------
# Check for Failed Downloads
# -----------------------------------------------------------------------------
if (length(failed_downloads) > 0) {
  cat("\n!!! WARNING: The following tickers failed to download:\n")
  cat(paste(failed_downloads, collapse = ", "), "\n")
  cat("Consider removing these from the analysis or trying again later.\n")
  
  # Remove failed tickers from the list
  tickers <- setdiff(tickers, failed_downloads)
  cat(paste("\nProceeding with", length(tickers), "stocks.\n"))
}

# -----------------------------------------------------------------------------
# Validate Data
# -----------------------------------------------------------------------------
cat("\n=== Validating Data ===\n")
for (ticker in names(stock_data)) {
  n_rows <- nrow(stock_data[[ticker]])
  date_range <- paste(min(index(stock_data[[ticker]])), "to", max(index(stock_data[[ticker]])))
  cat(paste(ticker, ":", n_rows, "rows,", date_range, "\n"))
}

# -----------------------------------------------------------------------------
# Save Data
# -----------------------------------------------------------------------------
cat("\n=== Saving Data ===\n")

# Create directories if they don't exist
dir.create(paths$raw_data, recursive = TRUE, showWarnings = FALSE)

# Save each stock
for (ticker in names(stock_data)) {
  saveRDS(stock_data[[ticker]], 
          file = paste0(paths$raw_data, ticker, ".rds"))
  cat(paste("Saved:", ticker, "\n"))
}

# Save S&P 500
saveRDS(sp500, paste0(paths$raw_data, "SP500.rds"))
cat("Saved: S&P 500\n")

# Save the list of successfully downloaded tickers
saveRDS(tickers, paste0(paths$processed_data, "valid_tickers.rds"))
cat("\nSaved list of valid tickers.\n")

# Save download metadata for reproducibility
download_metadata <- list(
  download_date = Sys.time(),
  start_date = start_date,
  end_date = end_date,
  tickers_requested = length(tickers) + length(failed_downloads),
  tickers_successful = length(tickers),
  tickers_failed = failed_downloads
)
saveRDS(download_metadata, paste0(paths$processed_data, "download_metadata.rds"))

cat("\n=== Data Collection Complete ===\n")
cat(paste("Successfully downloaded:", length(stock_data), "stocks + S&P 500\n"))
