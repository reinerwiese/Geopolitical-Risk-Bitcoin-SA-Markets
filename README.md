# Geopolitical Risk, Bitcoin and the South African Equity Market: Testing for Volatility Spillover

This repository contains the R scripts and supporting event-audit workbook for a BCom Honours thesis in Financial Risk Management at Stellenbosch University. The project looks at two questions: whether geopolitical risk is related to Bitcoin volatility, and whether Bitcoin volatility helps explain volatility in the South African equity market, represented by the FTSE/JSE Capped All Share Index (J303).

The analysis uses estimated conditional-volatility series, dynamic regressions, GPR regimes and event windows. Results are interpreted cautiously: a same-day relationship between Bitcoin and J303 volatility is not, by itself, evidence that one market transmits shocks to the other.

## Files

The R scripts and `GPR_Events.xlsx` should be kept in the same project folder as this README. The scripts also expect a workbook called `Thesis_Data.xlsx`, with the analysis data on a worksheet named `Data`. That workbook is not included here: the data were obtained through Bloomberg Terminal and are not publicly available. You will need authorised access to the data to prepare the workbook and run the analysis.

When you run Script 9, it creates a `Results` folder if needed. The output is saved under `Results/Tables/Core`, `Results/Tables/Appendix` and `Results/Figures`.

## Running the code

Use R or RStudio and set the working directory to the project folder. Install the required packages if they are not already installed.

To generate the tables and figures, run:

```r
source("9_Final_Results_Output.R")
```

This script runs the common setup and analysis scripts, then exports the selected results. You do not need to run Scripts 0–8 separately first when using this full build.

If you want to inspect the analysis one script at a time, run `0_Run_First.R` first and then run the remaining scripts in numerical order. Script 9 expects the R scripts, `GPR_Events.xlsx`, and a local copy of `Thesis_Data.xlsx`. It reads the `Data` sheet from `Thesis_Data.xlsx` and uses the `135 Raw Peak Audit` and `Sources` sheets in `GPR_Events.xlsx`.

## What each script does

**`0_Run_First.R` — Common setup**

Loads and prepares the analysis data, estimates the reference EGARCH models used to create the Bitcoin and J303 conditional-volatility series, calculates the GPR regime thresholds, and identifies candidate GPR events.

**`1_Preliminary_Data_Analysis.R` — Preliminary checks**

Produces descriptive statistics and checks the return series using stationarity, distribution, autocorrelation and ARCH tests. It also includes weekday return checks, which are useful because Bitcoin trades continuously while the JSE does not.

**`2_1_Bitcoin_Volatility.R` and `2_2_Index_Volatility.R` — Volatility models**

Compare sGARCH, EGARCH and GJR-GARCH specifications for Bitcoin and J303, along with alternative EGARCH mean specifications. The scripts also calculate model diagnostics and retain the fitted conditional-volatility series for the later analysis.

**`3_GPR_BTC_Analysis.R` — GPR and Bitcoin volatility**

Tests the relationship between the GPR index and Bitcoin conditional volatility. Alongside the baseline models, the current dynamic specifications include GPR lags 0–2 and lagged Bitcoin volatility. Full-sample and post-2017 results are considered, with the ACT and THREAT components examined separately. Newey–West inference and joint tests are used for the dynamic models.

**`4_BTC_J303_Analysis.R` — Bitcoin and J303 volatility**

Tests whether Bitcoin conditional volatility helps explain J303 conditional volatility after accounting for lagged J303 volatility. The script includes dynamic models and Granger-causality tests using Newey–West Wald tests. It also estimates Bitcoin volatility using USD returns as a robustness check, since converting Bitcoin to rand can introduce movements related to the exchange rate.

**`5_GPR_BTC_Threshold_Study.R` — GPR regimes for Bitcoin**

Examines whether the GPR–Bitcoin volatility relationship differs between lower- and elevated-GPR regimes. It includes dynamic models and interaction tests, using the overall GPR measure and the ACT and THREAT components. The post-2017 ACT interaction is also included.

**`6_GPR_BTC_Event_Study.R` — GPR events and Bitcoin volatility**

Studies Bitcoin volatility around identified geopolitical-risk events. Event identification is checked across 11-, 21- and 31-day smoothing windows. The analysis produces event statistics, correlations, regressions and significance grids.

**`7_BTC_J303_Threshold_Study.R` — Bitcoin–J303 relationship by GPR regime**

Tests whether the Bitcoin–J303 volatility relationship differs between lower- and elevated-GPR regimes. It includes dynamic models, interactions with the GPR regime, ACT and THREAT specifications, and formal tests of differences between regimes.

**`8_BTC_J303_Event_Study.R` — Bitcoin–J303 event analysis**

Examines the Bitcoin–J303 volatility relationship around the identified events using 11-, 21- and 31-day windows. The 21-observation results distinguish between repeated specifications that select the same event window and genuinely distinct windows.

**`9_Final_Results_Output.R` — Tables and figures**

Runs the analysis scripts and exports the chosen results to `Results/Tables/Core/`, `Results/Tables/Appendix/` and `Results/Figures/`. Tables are saved as CSV and TSV files, and figures as PNG files. The script also checks important exports and removes older generated files that have been superseded by the current tables or figures; it does not remove the source scripts or workbooks.

## Data

### `Thesis_Data.xlsx` (not included)

The scripts expect a workbook called `Thesis_Data.xlsx` with a worksheet named `Data`. The workbook is not included in this repository because the financial data were obtained through Bloomberg Terminal and are not publicly available. The worksheet combines those financial series with the publicly available Geopolitical Risk (GPR) Index data. The expected variables are:

| Variable | Description | Source |
|---|---|---|
| `Date` | Observation date | — |
| `BTC_log_returns` | Bitcoin log returns | Bloomberg Terminal |
| `Index_log_returns` | J303 log returns | Bloomberg Terminal |
| `GPRD` | Overall geopolitical risk index | Caldara and Iacoviello GPR Index |
| `GPRD_ACT` | Geopolitical risk associated with acts | Caldara and Iacoviello GPR Index |
| `GPRD_THREAT` | Geopolitical risk associated with threats | Caldara and Iacoviello GPR Index |

The `GPRD`, `GPRD_ACT` and `GPRD_THREAT` series are published by Dario Caldara and Matteo Iacoviello and can be downloaded from their [official GPR website](https://www.matteoiacoviello.com/gpr.htm). When using these data, please credit the authors and cite their paper:

Caldara, D. and Iacoviello, M. (2022). “Measuring Geopolitical Risk.” *American Economic Review*, 112(4), 1194–1225.

Although the GPR series are public, the combined `Thesis_Data.xlsx` workbook is not shared because it also contains the Bloomberg-based financial data. Anyone wishing to reproduce the analysis will need appropriate access to those financial data and must follow Bloomberg's data-use terms.

The common setup treats `NA` as a missing value and removes incomplete observations before estimating the reference volatility models. Lagged regressions may lose further observations. In the current dataset, the cleaned sample used for the main analysis runs from 3 January 2014 to 22 June 2026; check the data-audit table after rerunning the scripts if the workbook changes.

Bitcoin trades seven days a week, unlike the JSE. After aligning Bitcoin observations to JSE dates, some Bitcoin returns cover a different interval from others, especially around weekends. This should be kept in mind when interpreting the results.

### `GPR_Events.xlsx`

This workbook stores the event audit and source information used by Script 9. The `135 Raw Peak Audit` sheet contains the raw-peak audit, while the `Sources` sheet contains the source records. Keep it in the project folder with the R scripts and your local copy of `Thesis_Data.xlsx`.

Because the Bloomberg data are not included, the analysis scripts cannot be run from the repository alone. Anyone reproducing the analysis will need appropriate access to the source data and must follow Bloomberg's data-use terms and the university's requirements.

## Methods and interpretation

The main volatility models are from the GARCH family, including sGARCH(1,1), EGARCH(1,1) and GJR-GARCH(1,1). The fitted reference EGARCH models use a Student-t innovation distribution. Model comparison uses log-likelihood and information criteria, with residual and parameter-stability checks.

Regression results include Newey–West heteroskedasticity- and autocorrelation-consistent inference where specified. GPR effects in the main dynamic models are scaled so that results can be read as percentage-point changes in daily conditional volatility per one-standard-deviation increase in the relevant GPR measure.

In this project, **Granger causality refers to predictive content**: whether lagged values of one series improve the prediction of another series beyond the latter's own lags. It does not establish structural causation. The Bitcoin–J303 analysis also includes a USD-based Bitcoin-volatility check because the rand conversion may affect the apparent relationship.

Event results need particular care. Different GPR measures can select the same peak and therefore the same regression window, so repeated specifications should not be counted as separate events. The Crimea window in March 2014 also overlaps with the period following the Mt. Gox collapse; the unusually high Bitcoin volatility in that window should not be attributed to Crimea alone. Event-window length, overlapping episodes and the number of measures and specifications tested should be considered when interpreting significance.

## Packages

The scripts use R packages including:

- `readxl`
- `dplyr`
- `ggplot2`
- `rugarch`
- `FinTS`
- `zoo`
- `moments`
- `tseries`
- `lmtest`
- `sandwich`
- `car`

Install any missing packages before running the scripts. Some functions are called with their package namespace, such as `readxl::read_excel()`.

## Authors

**Reiner Wiese** and **Christopher Cresswell**  
BCom Honours in Financial Risk Management  
Stellenbosch University
