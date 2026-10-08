# Twelve Data Shiny explorer

This repository contains the Shiny app for the `twelvedataR` package.
It uses Twelve Data market data and requires a personal API key.

## Requirements

- R and the `shiny` package
- The `twelvedataR` package
- A Twelve Data API key

## Setup

```r
install.packages(c("shiny", "remotes"))
remotes::install_github("Tvths/twelvedataR")
```

Add this line to your user `.Renviron` file:

```text
TWELVEDATA_API_KEY=your_key_here
```

Restart R after saving the file.

## Run from GitHub

```r
shiny::runGitHub("Twelvedata_shiny", "Tvths")
```

In the app, choose a symbol, interval, and period, then select **Get data**.
The controls let you select an observation count or date range and toggle the
20-period moving average.
