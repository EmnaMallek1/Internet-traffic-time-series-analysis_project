# Internet Traffic Forecasting

Exploratory analysis, decomposition, autocorrelation analysis, and forecasting
(ARIMA/SARIMA) on an Internet traffic dataset (in bits), sampled every 5
minutes on an ISP's network.

## Data source

The data comes from the **Time Series Data Library (TSDL)** compiled by Rob
Hyndman (Monash University), a dataset widely reused in time series courses.
It originates from Cortez et al.'s work on Internet traffic forecasting with
neural networks, based on real measurements collected via SNMP on an ISP's
network interface.

## Contents

- `internet_traffic_analysis.R` — main script:
  1. Data loading and cleaning (dates, duplicates, missing values)
  2. Data quality analysis
  3. Time series construction (`ts`)
  4. Static and interactive visualization (base R, dygraphs)
  5. Decomposition (`stl`, `decompose`)
  6. Autocorrelation (`acf`, `pacf`)
  7. Forecasting: seasonal naive baseline vs. ARIMA/SARIMA, evaluated with
     MAE / RMSE / MAPE on a time-based train/test split

## Requirements

```r
install.packages(c("readr", "lubridate", "dygraphs", "xts", "dplyr", "forecast"))
```

## Usage

Place `internet-traffic-data-in-bits-fr.csv` in the project root, then:

```r
source("internet_traffic_analysis.R")
```

## Notes

- The script automatically excludes rows whose date could not be parsed and
  prints a sample of the problematic values for diagnostics.
- The default forecast horizon is one day (`h <- 24 * 12`).
