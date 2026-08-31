# Geopolitical Risk, Bitcoin, and Spillover Effects in South African Markets

R code for the empirical analysis conducted as part of an honours thesis investigating the relationship between geopolitical risk, Bitcoin, and South African financial markets.

## Research Question

Does geopolitical risk affect Bitcoin volatility, and does this volatility spill over into South African financial markets?

## Overview

This project examines the relationship between geopolitical risk, Bitcoin, and the South African equity market. The analysis focuses on Bitcoin volatility, geopolitical risk, and volatility transmission between Bitcoin and the JSE.

The empirical analysis includes volatility modelling, regression analysis, geopolitical risk regimes, major geopolitical episodes, and volatility transmission analysis.

## Methodology

The analysis includes:

- Descriptive and exploratory data analysis
- Augmented Dickey-Fuller stationarity tests
- Ljung-Box and ARCH tests
- GARCH, EGARCH, and GJR-GARCH models
- ARMA specification comparisons
- Regression analysis of geopolitical risk and Bitcoin returns and volatility
- Newey-West heteroskedasticity and autocorrelation consistent standard errors
- Geopolitical risk regime analysis
- Structural break and geopolitical episode analysis
- Correlation and regression analysis of Bitcoin and JSE volatility
- Fisher's r-to-z tests for comparing correlations

## Repository Structure

The repository currently contains the R scripts used for the empirical analysis:

| Script | Description |
|---|---|
| `1_Preliminary_Data_Analysis_Updated.R` | Preliminary data analysis and statistical diagnostics |
| `2_1_Bitcoin_Volatility_Updated.R` | Bitcoin volatility modelling and model selection |
| `2_2_Index_Volatility_Modelling.R` | J303 volatility modelling and model selection |
| `3_GPR_Bitcoin_analysis_final.R` | Geopolitical risk and Bitcoin returns and volatility |
| `4_Bitcoin_JSE_Analysis_final.R` | Bitcoin and JSE return relationship analysis |
| `5_GPR_Risk_Transmission_final.R` | Geopolitical risk regime analysis |
| `6_Event_Study_final.R` | Analysis of major geopolitical episodes |
| `7_Volatility_Transmission_Analysis_final.R` | Bitcoin and JSE volatility transmission analysis |

## Data

The empirical analysis uses financial-market and geopolitical-risk data.

The underlying dataset is not currently included in this repository. This repository therefore contains the analysis code rather than the raw data.

## Software

The analysis was conducted in **R** using packages including:

- `readxl`
- `dplyr`
- `ggplot2`
- `rugarch`
- `FinTS`
- `tseries`
- `moments`
- `lmtest`
- `sandwich`
- `car`
- `strucchange`
- `zoo`

## Author

**Reiner Wiese**

BCom Honours in Financial Risk Management  
Stellenbosch University
