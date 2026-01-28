# Stock Portfolio Risk-Return Analysis

An R-based analysis of stock portfolio strategies comparing risk and return metrics across different investment approaches.

## Overview

This project analyzes 8 stocks across different sectors (2020-2025) to:
- Compare individual stock performance and volatility
- Build and evaluate 3 portfolio strategies
- Test portfolio performance against S&P 500 benchmark
- Provide statistical validation of results

## Stocks Analyzed

| Ticker | Company | Sector |
|--------|---------|--------|
| AAPL | Apple | Technology |
| MSFT | Microsoft | Technology |
| JPM | JPMorgan Chase | Finance |
| JNJ | Johnson & Johnson | Healthcare |
| XOM | ExxonMobil | Energy |
| WMT | Walmart | Retail |
| DIS | Disney | Entertainment |
| TSLA | Tesla | Automotive |

## Portfolio Strategies

1. **Equal Weight** - 12.5% allocation to each stock
2. **Inverse Volatility** - Higher weight to stable stocks
3. **Sector-Balanced** - Weights based on sector diversification

## Key Findings

- Equal Weight portfolio achieved the highest return and Sharpe ratio
- TSLA showed highest volatility, JNJ showed lowest
- Equal and Sector portfolios significantly outperformed S&P 500 (p < 0.05)
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