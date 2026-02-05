# Stock Portfolio Risk-Return Analysis

A rigorous R-based analysis of stock portfolio strategies with proper walk-forward validation, rolling windows, and non-parametric statistical testing.

## Overview

This project analyzes 20 stocks across 8 sectors over 10 years (2015-2025) to:
- Build and evaluate 3 portfolio strategies
- Test performance against S&P 500 benchmark using out-of-sample validation
- Apply transaction costs and periodic rebalancing
- Use appropriate statistical methods for non-normal financial data

## Methodology

### Walk-Forward Validation
- **Training Period**: 2015-2020 (strategy development)
- **Testing Period**: 2021-2025 (out-of-sample evaluation)
- All reported results are from the testing period

### Key Features
| Feature | Implementation |
|---------|----------------|
| Volatility weights | Rolling 252-day trailing window (no look-ahead bias) |
| Rebalancing | Quarterly (every 63 trading days) |
| Transaction costs | 0.1% per trade |
| Statistical tests | Wilcoxon signed-rank + Bootstrap (non-parametric) |
| Reproducibility | Fixed end date, centralized config, error handling |

## Stocks Analyzed

| Sector | Stocks | Allocation |
|--------|--------|------------|
| Technology | AAPL, MSFT, GOOGL, NVDA | 25% |
| Finance | JPM, V, BAC | 15% |
| Healthcare | JNJ, PFE, UNH | 15% |
| Retail | WMT, AMZN, COST | 15% |
| Energy | XOM, CVX | 10% |
| Entertainment | DIS, NFLX | 10% |
| Automotive | TSLA, F | 5% |
| Industrial | CAT | 5% |

## Portfolio Strategies

1. **Equal Weight** — 5% allocation to each stock, no rebalancing needed
2. **Inverse Volatility** — Higher weight to stable stocks, recalculated quarterly using trailing 252-day volatility
3. **Sector-Balanced** — Fixed weights based on sector allocation table above

## Project Structure

```
project/
├── scripts/
│   ├── 00_config.R              # Shared configuration
│   ├── 01_data_collection.R     # Download data with error handling
│   ├── 02_data_cleaning.R       # Clean data + train/test split
│   ├── 03_eda.R                 # Exploratory analysis
│   ├── 04_portfolio_analysis.R  # Portfolio construction + backtest
│   └── 05_statistical_tests.R   # Non-parametric statistical tests
├── data/
│   ├── raw/                     # Downloaded stock data
│   └── processed/               # Cleaned data + results
├── output/
│   ├── figures/                 # Charts and visualizations
│   └── tables/                  # CSV exports
├── analysis_report.Rmd          # R Markdown report
└── README.md
```

## How to Run

1. Clone this repository
2. Open the project in RStudio
3. Run scripts in order:
   ```r
   source("scripts/00_config.R")
   source("scripts/01_data_collection.R")
   source("scripts/02_data_cleaning.R")
   source("scripts/03_eda.R")
   source("scripts/04_portfolio_analysis.R")
   source("scripts/05_statistical_tests.R")
   ```
4. Knit `analysis_report.Rmd` to generate the full report

## Requirements

- R (>= 4.0)
- Packages: `quantmod`, `dplyr`, `tidyr`, `ggplot2`, `lubridate`, `reshape2`

Install all packages:
```r
install.packages(c("quantmod", "dplyr", "tidyr", "ggplot2", "lubridate", "reshape2"))
```

## Limitations

This analysis improves on naive approaches but still has limitations:

**Addressed in this version:**
- ✓ Look-ahead bias (fixed with rolling windows)
- ✓ Invalid statistics (using non-parametric tests)
- ✓ No out-of-sample testing (train/test split implemented)
- ✓ No transaction costs (0.1% per trade included)

**Still present:**
- ✗ Survivorship bias (only includes stocks that exist today)
- ✗ Limited test period (2021-2025 was mostly bullish)
- ✗ Stock selection bias (well-known large-caps only)
- ✗ Transaction costs may be underestimated
- ✗ No taxes considered

**This is an educational project demonstrating data analysis methodology, not investment advice.**

## Key Findings (Out-of-Sample: 2021-2025)

Results from the testing period (strategies developed on 2015-2020 data):

- All portfolio strategies tested against S&P 500 using Wilcoxon signed-rank test
- Bootstrap confidence intervals used for Sharpe ratio comparisons
- Returns confirmed non-normal (fat tails) via Shapiro-Wilk test

See `output/tables/` for detailed metrics.

## License

MIT License - Feel free to use, modify, and learn from this code.

## Author

Arwin Sadafian
