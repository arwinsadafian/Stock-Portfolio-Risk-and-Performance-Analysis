# Stock Portfolio Risk-Return Analysis

An R-based analysis of stock portfolio strategies comparing risk and return metrics across different investment approaches.

## Overview

This project analyzes 20 stocks across different sectors (2015-2025) to:
- Compare individual stock performance and volatility
- Build and evaluate 3 portfolio strategies
- Test portfolio performance against S&P 500 benchmark
- Provide statistical validation of results

## Stocks Analyzed

| Sector | Stocks |
|--------|--------|
| Technology | AAPL, MSFT, GOOGL, NVDA |
| Finance | JPM, V, BAC |
| Healthcare | JNJ, PFE, UNH |
| Energy | XOM, CVX |
| Retail | WMT, AMZN, COST |
| Entertainment | DIS, NFLX |
| Automotive | TSLA, F |
| Industrial | CAT |

## Portfolio Strategies

1. **Equal Weight** - 5% allocation to each stock
2. **Inverse Volatility** - Higher weight to stable stocks, lower to volatile
3. **Sector-Balanced** - Weights based on sector allocation (Tech 25%, Healthcare 15%, Finance 15%, Retail 15%, Energy 10%, Entertainment 10%, Automotive 5%, Industrial 5%)

## Key Findings

- Sector-Balanced portfolio achieved the highest total return
- NVDA showed highest growth (top performer over 10 years)
- 13 out of 20 stocks showed statistically significant positive returns
- All portfolios significantly outperformed S&P 500 (p < 0.05)
- Stock returns are not normally distributed (fat tails present)

## Project Structure
```
project/
├── data/
│   ├── raw/
│   └── processed/
├── scripts/
│   ├── 01_data_collection.R
│   ├── 02_data_cleaning.R
│   ├── 03_eda.R
│   ├── 04_portfolio_analysis.R
│   └── 05_statistical_tests.R
├── output/
│   ├── figures/
│   └── tables/
├── analysis_report.Rmd
└── README.md
```

## How to Run

1. Clone this repository
2. Open the project in RStudio
3. Run scripts in order (01 through 05)
4. Knit `analysis_report.Rmd` to generate the report

## Requirements

- R (>= 4.0)
- Packages: quantmod, dplyr, tidyr, ggplot2, lubridate, reshape2