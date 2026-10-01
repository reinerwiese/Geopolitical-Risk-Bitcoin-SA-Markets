# Geopolitical Risk, Bitcoin, and Spillover Effects in South African Markets

Empirical analysis code and data for a BCom Honours thesis in Financial Risk Management at Stellenbosch University.

## Research Question

> **Does geopolitical risk affect Bitcoin volatility, and does this volatility spill over into South African financial markets?**

The project investigates two linked relationships:

1. **Geopolitical risk → Bitcoin volatility**
2. **Bitcoin volatility → South African equity-market volatility**

The analysis uses conditional volatility measures estimated from GARCH-family models and examines the relationships using full-sample, geopolitical-risk regime, and event-based analyses.

---

## Project Overview

The empirical workflow combines:

- Preliminary financial time-series diagnostics
- Bitcoin and J303 volatility modelling
- GARCH-family model comparison
- Alternative EGARCH mean specifications
- Geopolitical risk and Bitcoin volatility analysis
- Bitcoin–J303 volatility spillover analysis
- Geopolitical-risk regime analysis
- Alternative geopolitical-risk measures
- Geopolitical event studies
- Dynamic volatility regressions
- Newey–West HAC inference
- Granger-style volatility transmission analysis
- Automated production of thesis tables and figures

The scripts share a common data environment created by `0_Run_First.R`. The automated results script, `9_Final_Results_Output.R`, runs the analysis scripts in controlled environments and writes the results to the `Results/` directory.

---

## Repository Structure

```text
.
├── 0_Run_First.R
├── 1_Preliminary_Data_Analysis.R
├── 2_1_Bitcoin_Volatility.R
├── 2_2_Index_Volatility.R
├── 3_GPR_BTC_Analysis.R
├── 4_BTC_J303_Analysis.R
├── 5_GPR_BTC_Threshold_Study.R
├── 6_GPR_BTC_Event_Study.R
├── 7_BTC_J303_Threshold_Study.R
├── 8_BTC_J303_Event_Study.R
├── 9_Final_Results_Output.R
├── Thesis_Data.xlsx
├── Results/
│   ├── Tables/
│   │   ├── Core/
│   │   └── Appendix/
│   └── Figures/
└── README.md
```

> **Note:** The filenames above are the intended repository filenames. Uploaded/downloaded copies may contain version suffixes added by the file-sharing interface.

---

## Analysis Pipeline

The scripts are intended to be used in the following order:

```text
Thesis_Data.xlsx
       │
       ▼
0. Run First
   ├── Load and prepare data
   ├── Estimate reference EGARCH models
   ├── Create BTC and J303 conditional volatility
   ├── Calculate data-driven GPR thresholds
   ├── Assign GPR regimes
   └── Identify GPR events
       │
       ▼
1. Preliminary Data Analysis
   ├── Descriptive statistics
   ├── Distributional diagnostics
   ├── Stationarity tests
   └── Return autocorrelation / ARCH diagnostics
       │
       ├───────────────┬─────────────────┐
       ▼               ▼                 ▼
2.1 Bitcoin       2.2 J303          3. GPR → BTC
Volatility        Volatility        Returns/Volatility
       │               │                 │
       └───────────────┴─────────────────┘
                       │
                       ▼
               4. BTC → J303
                 Volatility
                  Spillover
                       │
              ┌────────┴────────┐
              ▼                 ▼
        5. GPR Threshold    6. GPR Event
             Study              Study
              │                 │
              ▼                 ▼
        7. BTC → J303       8. BTC → J303
        Threshold Study     Event Study
              │                 │
              └────────┬────────┘
                       ▼
              9. Final Results Output
                       │
              ┌────────┴────────┐
              ▼                 ▼
          Tables              Figures
```

---

## Script Guide

### `0_Run_First.R` — Common setup and reference volatility models

This script should be run before the other analysis scripts.

It:

- Loads `Thesis_Data.xlsx`, sheet `Data`
- Removes incomplete observations using `na.omit()`
- Converts `Date` to `Date` format
- Estimates reference EGARCH(1,1) models with:
  - ARMA(0,0) mean specification
  - Student-t innovations
- Creates:
  - `BTC_Volatility`
  - `J303_Volatility`
- Calculates GPR thresholds using two-cluster k-means with a fixed seed
- Creates lower/elevated GPR regime variables
- Identifies GPR episodes using a centred 21-day moving average

The GPR regime thresholds are **calculated from the data** rather than being hard-coded in this script.

### `1_Preliminary_Data_Analysis.R` — Preliminary diagnostics

Provides initial time-series and distributional analysis, including:

- Descriptive statistics
- Time-series plots
- Return distributions
- QQ plots
- Boxplots
- Augmented Dickey–Fuller stationarity tests
- Jarque–Bera normality tests
- Ljung–Box tests for returns
- Ljung–Box tests for squared returns
- ARCH-LM tests
- Weekday return standard-deviation checks

The weekday analysis is particularly relevant for documenting the different effective return intervals created when Bitcoin data are aligned to JSE trading dates.

### `2_1_Bitcoin_Volatility.R` — Bitcoin volatility model selection

Estimates and compares:

- sGARCH(1,1)
- EGARCH(1,1)
- GJR-GARCH(1,1)

using Student-t innovations.

It also compares EGARCH mean specifications:

- ARMA(0,0)
- ARMA(1,0)
- ARMA(0,1)
- ARMA(1,1)

Model comparison includes log-likelihood and information criteria such as AIC and BIC, together with residual and parameter-stability diagnostics. The script extracts an EGARCH-based conditional-volatility series for its downstream calculations.

### `2_2_Index_Volatility.R` — J303 volatility model selection

Performs the analogous volatility-modelling exercise for the J303 return series.

It estimates and compares:

- sGARCH(1,1)
- EGARCH(1,1)
- GJR-GARCH(1,1)

and examines alternative EGARCH mean specifications, model-selection criteria, and diagnostics.

### `3_GPR_BTC_Analysis.R` — Geopolitical risk and Bitcoin volatility

Examines the relationship between geopolitical risk and Bitcoin conditional volatility.

The script includes:

- Exploratory plots
- Pearson correlation analysis
- Baseline GPR regressions
- GPR lag specifications
- Dynamic models including lagged Bitcoin volatility
- Model-comparison statistics
- Regression diagnostics
- Newey–West robust inference
- Additional GPR measures where implemented

The static regressions are retained as baseline/descriptive models, while dynamic specifications account for persistence in Bitcoin volatility.

### `4_BTC_J303_Analysis.R` — Bitcoin–J303 volatility spillover

Examines the relationship between Bitcoin conditional volatility and J303 conditional volatility.

The analysis includes:

- Contemporaneous correlation
- Baseline Bitcoin-to-J303 volatility regressions
- Dynamic models including lagged J303 volatility
- Lagged Bitcoin volatility
- GPR controls
- Regression diagnostics
- Newey–West robust inference
- Granger-style volatility transmission analysis
- Robustness analysis where implemented

The dynamic specifications are intended to address the strong persistence present in conditional-volatility series.

### `5_GPR_BTC_Threshold_Study.R` — GPR regime analysis for Bitcoin

Examines whether the GPR–Bitcoin volatility relationship differs between lower- and elevated-GPR regimes.

The script includes:

- Regime-specific descriptive analysis
- Regime-specific correlations
- Static and dynamic regressions
- Interaction models
- Alternative GPR measures:
  - `GPRD`
  - `GPRD_ACT`
  - `GPRD_THREAT`
- Regression diagnostics
- Newey–West inference
- Regime comparisons

The regime definitions are based on the data-driven thresholds generated in `0_Run_First.R`.

### `6_GPR_BTC_Event_Study.R` — GPR event analysis for Bitcoin

Examines Bitcoin volatility during identified geopolitical-risk episodes.

The event analysis is repeated using:

- 11-day smoothing
- 21-day smoothing
- 31-day smoothing

For the identified events, the script calculates event statistics, correlations, regression results, diagnostics, and Newey–West inference. It also evaluates the stability of event identification across smoothing windows.

### `7_BTC_J303_Threshold_Study.R` — BTC–J303 spillover by GPR regime

Examines whether the Bitcoin–J303 volatility relationship differs between lower- and elevated-GPR regimes.

The analysis includes:

- Regime-specific baseline models
- Dynamic regime models
- Lagged volatility terms
- BTC–J303 interaction with GPR regime
- Model diagnostics
- Newey–West inference
- Regime comparisons

### `8_BTC_J303_Event_Study.R` — BTC–J303 spillover during identified events

Examines the Bitcoin–J303 volatility relationship during identified geopolitical-risk events.

As with the GPR–Bitcoin event study, the analysis is run using 11-, 21-, and 31-day smoothing windows and includes:

- Event statistics
- BTC–J303 correlations
- Event-specific regressions
- Regression diagnostics
- Newey–West inference
- Event-window comparison
- Event-identification stability analysis

The current script should be treated as the implementation of the **current event specification**; event definitions and windows remain an area that must be kept consistent with the final thesis methodology.

### `9_Final_Results_Output.R` — Automated results generation

This script provides the reproducible results pipeline.

It:

1. Creates the `Results/` directory structure.
2. Checks that the expected analysis scripts and `Thesis_Data.xlsx` are present.
3. Runs `0_Run_First.R`.
4. Runs Scripts 1–8 in isolated environments.
5. Extracts selected results from the analysis environments.
6. Writes tables to:
   - `Results/Tables/Core/`
   - `Results/Tables/Appendix/`
7. Writes figures to:
   - `Results/Figures/`

It also reports the number of generated core tables, appendix tables, and figures.

---

## Data

### Main input

**File:** `Thesis_Data.xlsx`  
**Worksheet:** `Data`

The analysis uses the following variables:

| Variable | Description |
|---|---|
| `Date` | Observation date |
| `Index_log_returns` | J303 index log returns |
| `BTC_log_returns` | Bitcoin log returns |
| `GPRD` | Main geopolitical risk measure |
| `GPRD_ACT` | GPR measure relating to geopolitical acts |
| `GPRD_THREAT` | GPR measure relating to geopolitical threats |

The scripts currently load the workbook using the filename `Thesis_Data.xlsx`.

### Data handling

The common setup script:

- Reads the `Data` worksheet
- Treats `"NA"` as missing when importing
- Removes incomplete observations with `na.omit()`
- Converts `Date` to R's `Date` class

Individual downstream scripts may remove additional observations when constructing lagged variables.

### Important return-frequency consideration

Bitcoin trades continuously, whereas the JSE does not. When Bitcoin observations are aligned to JSE trading dates, a Monday Bitcoin return can cover the weekend as well as Monday, while other observations may represent a single day.

This means the effective interval represented by a Bitcoin return is not identical for every observation and should be documented when interpreting the volatility analysis.

### Data provenance

The dataset is intended to represent the corrected Bloomberg-based dataset used for the thesis analysis. Any redistribution of data should comply with the relevant data-provider and university requirements.

---

## Methodological Framework

The repository currently implements the following main methods:

### Preliminary analysis

- Descriptive statistics
- Skewness and kurtosis
- Time-series plots
- ADF stationarity tests
- Jarque–Bera tests
- Ljung–Box tests
- Squared-return autocorrelation tests
- ARCH-LM tests
- Weekday diagnostics

### Conditional volatility modelling

- sGARCH(1,1)
- EGARCH(1,1)
- GJR-GARCH(1,1)
- Student-t innovations
- Alternative ARMA mean specifications
- AIC
- BIC
- Log-likelihood
- Variance-persistence diagnostics
- Nyblom parameter-stability diagnostics
- Standardised residual diagnostics

### Volatility regressions

- Contemporaneous relationships
- Lagged GPR specifications
- Dynamic volatility regressions
- Lagged dependent variables
- Lagged Bitcoin volatility
- GPR controls
- Interaction models
- Model diagnostics
- Newey–West HAC inference

### Spillover / transmission analysis

- Bitcoin–J303 volatility correlations
- Baseline spillover regressions
- Dynamic spillover regressions
- Granger-style volatility transmission testing
- GPR-regime interactions
- Event-specific spillover analysis

### Geopolitical-risk analysis

- Main GPR index
- GPR acts component
- GPR threats component
- Data-driven lower/elevated GPR regimes
- Moving-average event identification
- Event-window sensitivity
- Fisher r-to-z correlation comparisons where implemented

---

## Software and Packages

The analysis is written in **R**.

Packages used across the current scripts include:

- `readxl`
- `dplyr`
- `ggplot2`
- `rugarch`
- `zoo`
- `moments`
- `tseries`
- `FinTS`
- `lmtest`
- `sandwich`
- `car`

Additional package requirements may depend on the specific analysis being run.

---

## Reproducibility

### Recommended workflow

1. Place the R scripts and `Thesis_Data.xlsx` in the same project directory.
2. Open R/RStudio with that directory as the working directory.
3. Install the required packages.
4. Run:

```r
source("0_Run_First.R")
```

5. Run the analysis scripts in numerical order if individual outputs need to be inspected.
6. For the automated results build, run:

```r
source("9_Final_Results_Output.R")
```

The automated script expects the repository to use the exact filenames listed in the repository structure above.

### Important

The current codebase contains both reference model-estimation code in `0_Run_First.R` and model-selection work in Scripts `2_1` and `2_2`. If the final volatility specification is changed during the thesis process, the downstream volatility-dependent results must be regenerated.

---

## Interpretation and Scope

The analysis distinguishes between:

- **Association:** correlations and baseline regressions
- **Volatility persistence:** dynamic volatility models
- **Transmission/spillover:** lagged Bitcoin–J303 specifications and Granger-style tests
- **Regime dependence:** differences across lower/elevated GPR regimes
- **Event behaviour:** relationships during identified geopolitical-risk episodes

The results should therefore be interpreted in the context of the particular model specification, sample period, event definition, diagnostics, and robustness checks used.

Statistical significance in a baseline OLS model should not automatically be treated as robust evidence when serial correlation or heteroskedasticity is present; Newey–West inference and dynamic specifications are included to address these issues.

---

## Authors

**Reiner Wiese**  
**Christopher Cresswell**

BCom Honours in Financial Risk Management  
Stellenbosch University

---

## Project Status

This repository contains the current empirical analysis code and thesis dataset used to generate the project's empirical results, tables, and figures.

The repository is an active research project. Final model specifications, event definitions, and thesis interpretations should be aligned with the final validated analysis before submission.
