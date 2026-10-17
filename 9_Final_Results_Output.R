# 1. Load Packages

library(dplyr)
library(ggplot2)
library(rugarch)
library(FinTS)


# 2. Setup

script_dir <- getwd()

output_dir <- file.path(script_dir, "Results")
core_table_dir <- file.path(output_dir, "Tables", "Core")
appendix_table_dir <- file.path(output_dir, "Tables", "Appendix")
figure_dir <- file.path(output_dir, "Figures")

dir.create(output_dir, showWarnings = FALSE)
dir.create(core_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(appendix_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)


# Remove stale outputs that are superseded by consolidated tables/figures.
# Only generated files under Results are removed; source scripts and workbooks
# are never changed by Script 9.

obsolete_table_basenames <- c(
  "T03_Bitcoin_GARCH_Comparison",
  "T04_Bitcoin_EGARCH_Mean_Comparison",
  "T05_J303_GARCH_Comparison",
  "T06_J303_EGARCH_Mean_Comparison",
  "T07_Final_GARCH_Diagnostics",
  "T05a_EGARCH_Residual_Diagnostics",
  "T08_Weekday_Return_SD",
  "T10_GPR_Bitcoin_Regression",
  "T11a_GPR_Component_Dynamic_Regression",
  "T12_GPR_Bitcoin_Post2017",
  "T23_GPR_ACT_Regime_Dynamic_Models",
  "T24_GPR_THREAT_Regime_Dynamic_Models",
  "T25_GPR_Regime_Interaction",
  "T25a_GPR_Alternative_Measure_Interactions",
  "T26_Bitcoin_J303_Regime_Descriptives",
  "T27_Bitcoin_J303_Regime_Correlation",
  "T30_Bitcoin_J303_GPR_ACT_Regime_Spillover",
  "T31_Bitcoin_J303_GPR_THREAT_Regime_Spillover",
  "T31a_Bitcoin_J303_Alternative_GPR_Interactions",
  "T16_Bitcoin_J303_Post2017",
  "T17_Granger_Post2017",
  "T32_GPR_Bitcoin_Events_21Day",
  "T35_GPR_Bitcoin_Event_Window_Comparison",
  "T38_Bitcoin_J303_Events_21Day",
  "T31a_Bitcoin_J303_GPRD_Regime_Incremental_R2",
  "T31b_Bitcoin_J303_ACT_Regime_Incremental_R2",
  "T31c_Bitcoin_J303_THREAT_Regime_Incremental_R2",
  "T36d_Event_Episode_to_Source_Codes",
  "T37_GPR_Bitcoin_Event_Regression_Stability",
  "T42_Bitcoin_J303_Event_Regression_Stability"
)

obsolete_table_files <- unlist(
  lapply(
    c(core_table_dir, appendix_table_dir),
    function(folder) {
      unlist(
        lapply(
          obsolete_table_basenames,
          function(base) {
            file.path(folder, paste0(base, c(".csv", ".tsv")))
          }
        ),
        use.names = FALSE
      )
    }
  ),
  use.names = FALSE
)

obsolete_table_files <- obsolete_table_files[file.exists(obsolete_table_files)]

if (length(obsolete_table_files) > 0) {
  invisible(file.remove(obsolete_table_files))
}

# Some tables are promoted from Appendix to Core in this version.
promoted_table_basenames <- c(
  "T35a_GPR_Bitcoin_Event_Significance_Correlation_Grid",
  "T35b_GPR_Bitcoin_Event_Significance_NW_Grid",
  "T29_Bitcoin_J303_Regime_Interaction",
  "T40_Bitcoin_J303_Event_Regression_21Day",
  "T40a_Bitcoin_J303_Event_Unique_Window_Summary"
)

promoted_appendix_files <- unlist(
  lapply(
    promoted_table_basenames,
    function(base) {
      file.path(appendix_table_dir, paste0(base, c(".csv", ".tsv")))
    }
  ),
  use.names = FALSE
)

promoted_appendix_files <- promoted_appendix_files[
  file.exists(promoted_appendix_files)
]

if (length(promoted_appendix_files) > 0) {
  invisible(file.remove(promoted_appendix_files))
}

# Remove obsolete standalone panel figures that are replaced by faceted figures.
obsolete_figure_files <- file.path(
  figure_dir,
  c(
    "F01_Bitcoin_Returns.png",
    "F02_J303_Returns.png",
    "F03_GPRD.png",
    "F03a_GPRD_ACT.png",
    "F03b_GPRD_THREAT.png",
    "F04_Bitcoin_Conditional_Volatility.png",
    "F05_J303_Conditional_Volatility.png",
    "F06_GPR_Bitcoin_Volatility.png",
    "F07_Bitcoin_J303_Volatility.png",
    "F08_GPR_Regimes.png",
    "F09_GPR_Bitcoin_Event_Coefficients_21Day.png",
    "F10_Bitcoin_J303_Event_Coefficients_21Day.png",
    "F11_Bitcoin_Standardized_Residuals.png",
    "F12_J303_Standardized_Residuals.png"
  )
)

obsolete_figure_files <- obsolete_figure_files[file.exists(obsolete_figure_files)]

if (length(obsolete_figure_files) > 0) {
  invisible(file.remove(obsolete_figure_files))
}


# 3. Check Required Files

script_files <- c(
  "0_Run_First.R",
  "1_Preliminary_Data_Analysis.R",
  "2_1_Bitcoin_Volatility.R",
  "2_2_Index_Volatility.R",
  "3_GPR_BTC_Analysis.R",
  "4_BTC_J303_Analysis.R",
  "5_GPR_BTC_Threshold_Study.R",
  "6_GPR_BTC_Event_Study.R",
  "7_BTC_J303_Threshold_Study.R",
  "8_BTC_J303_Event_Study.R"
)

if (!all(file.exists(file.path(script_dir, script_files)))) {
  stop("One or more analysis scripts were not found.")
}

if (!file.exists(file.path(script_dir, "Thesis_Data.xlsx"))) {
  stop("Thesis_Data.xlsx not found.")
}

if (!file.exists(file.path(script_dir, "GPR_Events.xlsx"))) {
  stop("GPR_Events.xlsx not found.")
}


# 4. Run Analysis Scripts

master <- new.env(parent = globalenv())

sys.source(file.path(script_dir, script_files[1]), envir = master)

run_in_isolated_env <- function(path) {
  
  e <- new.env(parent = master)
  tmp_pdf <- tempfile(fileext = ".pdf")
  
  grDevices::pdf(tmp_pdf)
  
  on.exit({
    try(grDevices::dev.off(), silent = TRUE)
    unlink(tmp_pdf)
  })
  
  sys.source(path, envir = e)
  e
}

e1 <- run_in_isolated_env(file.path(script_dir, script_files[2]))
e21 <- run_in_isolated_env(file.path(script_dir, script_files[3]))
e22 <- run_in_isolated_env(file.path(script_dir, script_files[4]))
e3 <- run_in_isolated_env(file.path(script_dir, script_files[5]))
e4 <- run_in_isolated_env(file.path(script_dir, script_files[6]))
e5 <- run_in_isolated_env(file.path(script_dir, script_files[7]))
e6 <- run_in_isolated_env(file.path(script_dir, script_files[8]))
e7 <- run_in_isolated_env(file.path(script_dir, script_files[9]))
e8 <- run_in_isolated_env(file.path(script_dir, script_files[10]))


# 5. Functions

round_numeric <- function(x, digits = 4) {
  
  if (!is.numeric(x)) {
    return(x)
  }
  
  out <- round(x, digits)
  
  small_nonzero <- !is.na(x) & x != 0 & abs(x) < 10^(-digits)
  
  out[small_nonzero] <- signif(x[small_nonzero], digits)
  
  out
}


# Standardise p-value presentation across all exported tables.
# All p-values below 0.01 are displayed as <0.01, rather than as zero
# or as varying scientific-notation thresholds.
format_p_values <- function(x, digits = 4) {
  
  out <- rep(NA_character_, length(x))
  
  # Use the same reporting threshold for all exported p-values.
  # This includes values numerically returned as exactly zero.
  is_small <- !is.na(x) & is.finite(x) & x >= 0 & x < 0.01
  is_reportable <- !is.na(x) & is.finite(x) & x >= 0.01 & x <= 1
  is_other <- !is.na(x) & !(is_small | is_reportable)
  
  out[is_small] <- "<0.01"
  out[is_reportable] <- formatC(
    x[is_reportable],
    format = "g",
    digits = digits
  )
  out[is_other] <- as.character(x[is_other])
  
  out
}


write_table <- function(
    x, name, digits = 4, appendix = FALSE, display_headers = NULL
) {
  
  x <- as.data.frame(x)
  
  # Keep p-values before general numeric rounding so that small values
  # are not rounded to zero in the exported tables. Some upstream scripts
  # already format p-values as character strings; include those too so the
  # same reporting threshold is applied consistently across every export.
  p_value_cols <- names(x)[
    grepl(
      "(^p$|p.?value|p.?val|(^|_)p($|_)|^pr\\s*\\()",
      names(x),
      ignore.case = TRUE
    )
  ]
  raw_p_values <- x[p_value_cols]
  
  x[] <- lapply(
    x,
    round_numeric,
    digits = digits
  )
  
  # Apply the same <0.01 threshold and four-significant-digit convention
  # to numeric and pre-formatted character p-value columns in both exports.
  for (nm in p_value_cols) {
    raw_p <- raw_p_values[[nm]]
    
    if (is.numeric(raw_p)) {
      x[[nm]] <- format_p_values(raw_p, digits = 4)
    } else {
      raw_text <- trimws(as.character(raw_p))
      numeric_p <- suppressWarnings(as.numeric(raw_text))
      formatted_p <- raw_text
      
      can_format <- !is.na(numeric_p) &
        is.finite(numeric_p) &
        numeric_p >= 0 &
        numeric_p <= 1
      
      formatted_p[can_format] <- format_p_values(
        numeric_p[can_format],
        digits = 4
      )
      
      # Preserve explicit reporting bounds such as <0.01. Standardise only
      # bounds whose numeric value is itself below the common <0.01 threshold.
      less_than_label <- grepl("^<", raw_text)
      less_than_bound <- suppressWarnings(
        as.numeric(sub("^<\\s*", "", raw_text))
      )
      standardise_to_01 <- less_than_label &
        !is.na(less_than_bound) &
        less_than_bound < 0.01
      formatted_p[standardise_to_01] <- "<0.01"
      formatted_p[is.na(raw_p)] <- NA_character_
      
      x[[nm]] <- formatted_p
    }
  }
  
  destination <- if (appendix) {
    appendix_table_dir
  } else {
    core_table_dir
  }
  
  write.csv(
    x,
    file = file.path(
      destination,
      paste0(name, ".csv")
    ),
    row.names = FALSE,
    na = ""
  )
  # Create a display-only version for LaTeX.
  # P-value formatting is standardised in both exports; the following
  # changes only make event IDs, text, and headings easier to read in TSV.
  x_tsv <- x
  original_names <- names(x_tsv)
  
  # Make event IDs easier to read in printed tables.
  if ("Event_ID" %in% original_names) {
    x_tsv[["Event_ID"]] <- gsub(
      "_", "-", x_tsv[["Event_ID"]], fixed = TRUE
    )
  }
  
  # Replace underscores in displayed text, but preserve URLs.
  for (nm in original_names) {
    if (
      is.character(x_tsv[[nm]]) &&
      nm != "Event_ID" &&
      !nm %in% c("Primary_Source_Details", "Supporting_Source_Details") &&
      !grepl("url|link|reference", nm, ignore.case = TRUE)
    ) {
      x_tsv[[nm]] <- gsub(
        "_", " ", x_tsv[[nm]], fixed = TRUE
      )
    }
  }
  
  # Make column headings more readable.
  display_names <- gsub(
    "_", " ", original_names, fixed = TRUE
  )
  
  display_names <- gsub(
    "([a-z0-9])([A-Z])",
    "\\1 \\2",
    display_names,
    perl = TRUE
  )
  
  display_names <- gsub(
    " +", " ", display_names
  )
  
  # Shorten diagnostic headings where appropriate.
  display_names[
    display_names == "Ljung Box Residual p"
  ] <- "LB p (residuals)"
  
  display_names[
    display_names == "Ljung Box Squared p"
  ] <- "LB p (squared residuals)"
  
  display_names[
    display_names == "Nyblom Joint"
  ] <- "Nyblom joint"
  
  # Shorten selected display headings in the affected thesis tables only.
  # CSV source-column names and all underlying table values remain unchanged.
  display_heading_map <- c(
    "Convergence Code" = "Conv. code",
    "Convergence Status" = "Conv. status",
    "LB p (squared residuals)" = "LB p (resid. squared)",
    "Nyblom Joint Critical 10pct" = "Nyblom crit. 10 pct",
    "Nyblom Joint Critical 5pct" = "Nyblom crit. 5 pct",
    "Nyblom Joint Critical 1pct" = "Nyblom crit. 1 pct",
    "GPRD ACT Coefficient pp" = "ACT coefficient (pp)",
    "GPRD THREAT Coefficient pp" = "THREAT coefficient (pp)",
    "GPRD ACT NW pvalue" = "ACT NW p",
    "GPRD THREAT NW pvalue" = "THREAT NW p",
    "Number of Specifications" = "Specs",
    "Number of Distinct Windows" = "Distinct windows",
    "Significant Correlation Specifications" = "Sig. corr. specs",
    "Significant NW Regression Specifications" = "Sig. NW specs",
    "Significant Distinct Correlation Windows" = "Sig. corr. windows",
    "Significant Distinct NW Windows" = "Sig. NW windows",
    "Event Start Date" = "Episode start",
    "Event End Date" = "Episode end",
    "Raw Rank" = "Peak rank",
    "Raw Peak Date" = "Raw peak date",
    "Raw GPR Value" = "GPR value",
    "Underlying Event" = "Event description",
    "Story Break Date" = "Story date",
    "Story Date Flag" = "Story-date check",
    "News Start Date" = "Window start",
    "News End Date" = "Window end",
    "Regression Window Flag" = "Window check",
    "Source Role" = "Role",
    "Publication Date" = "Pub. date",
    "Primary Publisher" = "Primary publisher",
    "Primary Publication Date" = "Primary pub. date",
    "Primary Source Headline" = "Primary headline",
    "Primary Source URL" = "Primary URL",
    "Supporting Publisher" = "Supporting publisher",
    "Supporting Publication Date" = "Supporting pub. date",
    "Supporting Source Headline" = "Supporting headline",
    "Supporting Source URL" = "Supporting URL",
    "Primary Source Details" = "Primary source",
    "Supporting Source Details" = "Supporting source",
    "Evidence Note" = "Evidence note",
    "Peak Date Evidence Note" = "Peak/evidence note",
    "Peak-Date / Evidence Note" = "Peak/evidence note"
  )
  for (old_heading in names(display_heading_map)) {
    display_names[display_names == old_heading] <- display_heading_map[[old_heading]]
  }
  
  # Apply table-specific display headings only to TSV exports. The CSV
  # column names remain unchanged. Keep TSV headings as plain text because
  # csvsimple's automatic tabular commands cannot safely parse embedded
  # LaTeX heading macros; add line breaks in the LaTeX table definition.
  if (!is.null(display_headers)) {
    if (is.null(names(display_headers)) || any(!nzchar(names(display_headers)))) {
      stop("display_headers must be a named character vector.")
    }
    missing_display_headers <- setdiff(names(display_headers), original_names)
    if (length(missing_display_headers) > 0L) {
      stop(
        "Display-header mapping refers to unknown source columns: ",
        paste(missing_display_headers, collapse = ", ")
      )
    }
    for (source_heading in names(display_headers)) {
      display_names[match(source_heading, original_names)] <-
        unname(display_headers[[source_heading]])
    }
  }
  
  # Render comparison signs such as <0.01 correctly in LaTeX-imported TSVs.
  # CSV exports remain plain text for convenient inspection in Excel.
  for (nm in intersect(p_value_cols, original_names)) {
    if (is.character(x_tsv[[nm]])) {
      p_text <- trimws(x_tsv[[nm]])
      needs_math <- !is.na(p_text) &
        grepl("^[<>]", p_text) &
        !grepl("^\\$.*\\$$", p_text)
      x_tsv[[nm]][needs_math] <- paste0("$", p_text[needs_math], "$")
    }
  }
  
  # Table 3.1 Panel B combines each test statistic and p-value in one cell.
  # Keep <0.01 in LaTeX math mode inside parentheses in the TSV only.
  if (identical(name, "T02a_Preliminary_Tests")) {
    combined_stat_p_columns <- intersect(
      c("BTC_Statistic_P", "J303_Statistic_P"),
      original_names
    )
    for (nm in combined_stat_p_columns) {
      x_tsv[[nm]] <- gsub("\\(<0\\.01\\)", "($<0.01$)", x_tsv[[nm]])
    }
  }
  
  names(x_tsv) <- display_names
  
  write.table(
    x_tsv,
    file = file.path(
      destination,
      paste0(name, ".tsv")
    ),
    sep = "\t",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE,
    na = ""
  )
  
  print(x)
  
  invisible(x)
}


save_plot <- function(p, name, width = 7, height = 4.5) {
  
  ggsave(
    filename = file.path(
      figure_dir,
      paste0(name, ".png")
    ),
    plot = p,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}


theme_thesis <- theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 12
    ),
    plot.subtitle = element_text(
      size = 10
    ),
    axis.title = element_text(
      size = 10
    ),
    axis.text = element_text(
      colour = "black"
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      colour = "grey85",
      linewidth = 0.25
    )
  )


# 6. Data Audit

data0 <- master$data0

# Audit the original workbook rows separately from the cleaned analysis sample.
# Script 0 applies na.omit(), so auditing only master$data0 would incorrectly
# suggest that the source workbook had no incomplete rows.

raw_data_for_audit <- readxl::read_excel(
  file.path(script_dir, "Thesis_Data.xlsx"),
  sheet = "Data",
  na = "NA"
)

raw_dates <- as.Date(raw_data_for_audit$Date)
valid_raw_dates <- raw_dates[!is.na(raw_dates)]

cleaned_dates <- as.Date(data0$Date)
missing_counts <- colSums(is.na(raw_data_for_audit))
index_return_missing <- if ("Index_log_returns" %in% names(missing_counts)) {
  unname(missing_counts[["Index_log_returns"]])
} else {
  NA_integer_
}

data_audit <- data.frame(
  Item = c(
    "Rows in source Data sheet",
    "Source rows with a non-missing date",
    "Rows retained after complete-case cleaning",
    "Rows excluded during complete-case cleaning",
    "Rows with missing Date",
    "Rows with missing Index_log_returns",
    "Source start date (among dated rows)",
    "Source end date (among dated rows)",
    "Cleaned estimation sample start date",
    "Cleaned estimation sample end date",
    "Unique non-missing source dates",
    "Duplicated non-missing source dates"
  ),
  Value = c(
    as.character(nrow(raw_data_for_audit)),
    as.character(sum(!is.na(raw_dates))),
    as.character(nrow(data0)),
    as.character(nrow(raw_data_for_audit) - nrow(data0)),
    as.character(sum(is.na(raw_dates))),
    as.character(index_return_missing),
    as.character(min(valid_raw_dates)),
    as.character(max(valid_raw_dates)),
    as.character(min(cleaned_dates, na.rm = TRUE)),
    as.character(max(cleaned_dates, na.rm = TRUE)),
    as.character(length(unique(valid_raw_dates))),
    as.character(sum(duplicated(valid_raw_dates)))
  ),
  stringsAsFactors = FALSE
)

write_table(
  data_audit,
  "T01_Data_Audit"
)


# 7. Descriptive Statistics

statistics <- e1$statistics

statistics <- data.frame(
  Variable = rownames(statistics),
  statistics,
  row.names = NULL
)

write_table(
  statistics,
  "T02_Descriptive_Statistics"
)

# Preliminary stationarity, autocorrelation and ARCH tests.
# Table 3.1 Panel B uses one row per test, with Bitcoin and J303 results
# side by side. Each statistic is paired with its p-value in the same cell
# to keep the table compact and improve comparability.

preliminary_tests <- e1$preliminary_test_results %>%
  mutate(
    P_Value = as.numeric(P_Value),
    P_Value = dplyr::case_when(
      is.na(P_Value) ~ NA_character_,
      P_Value < 0.01 ~ "<0.01",
      Test == "Augmented Dickey-Fuller" & P_Value <= 0.01 ~ "<0.01",
      TRUE ~ formatC(P_Value, format = "g", digits = 4)
    )
  )

preliminary_test_base <- preliminary_tests %>%
  mutate(
    Asset = ifelse(grepl("^Bitcoin", Series), "Bitcoin", "J303"),
    Statistic = as.numeric(Statistic),
    Statistic_P = paste0(
      formatC(as.numeric(Statistic), format = "f", digits = 4),
      " (",
      P_Value,
      ")"
    )
  ) %>%
  select(
    Test,
    Lag_Order,
    Degrees_of_Freedom,
    Asset,
    Statistic_P
  )

preliminary_tests_bitcoin <- preliminary_test_base %>%
  filter(Asset == "Bitcoin") %>%
  transmute(
    Test,
    Lag_Order,
    Degrees_of_Freedom,
    BTC_Statistic_P = Statistic_P
  )

preliminary_tests_j303 <- preliminary_test_base %>%
  filter(Asset == "J303") %>%
  transmute(
    Test,
    Lag_Order,
    J303_Statistic_P = Statistic_P
  )

preliminary_tests_wide <- dplyr::full_join(
  preliminary_tests_bitcoin,
  preliminary_tests_j303,
  by = c("Test", "Lag_Order")
) %>%
  arrange(match(
    Test,
    c(
      "Augmented Dickey-Fuller",
      "Ljung-Box: Returns",
      "Ljung-Box: Squared Returns",
      "ARCH LM"
    )
  ))

if (
  nrow(preliminary_tests_wide) != 4L ||
  !all(c(
    "BTC_Statistic_P", "J303_Statistic_P"
  ) %in% names(preliminary_tests_wide))
) {
  stop("T02a validation failed: expected four wide-format preliminary-test rows with paired Bitcoin and J303 statistic/p-value cells.")
}

write_table(
  preliminary_tests_wide,
  "T02a_Preliminary_Tests",
  display_headers = c(
    Test = "Test",
    Lag_Order = "Lag",
    Degrees_of_Freedom = "df",
    BTC_Statistic_P = "Bitcoin stat. (p)",
    J303_Statistic_P = "J303 stat. (p)"
  )
)


# 8. GARCH Model Selection

# Consolidate the candidate volatility-model comparisons for Bitcoin and J303.
# Log-likelihood, AIC and BIC are retained for the main comparison table.

garch_model_comparison <- dplyr::bind_rows(
  e21$comparison %>%
    transmute(
      Asset = "Bitcoin",
      Model,
      LogLikelihood,
      AIC,
      BIC
    ),
  e22$comparison %>%
    transmute(
      Asset = "J303",
      Model,
      LogLikelihood,
      AIC,
      BIC
    )
)

write_table(
  garch_model_comparison,
  "T03_GARCH_Model_Comparison"
)


# Alternative EGARCH mean specifications are retained in one appendix table.

egarch_mean_comparison <- dplyr::bind_rows(
  e21$mean_comparison %>%
    transmute(
      Asset = "Bitcoin",
      Model,
      LogLikelihood,
      AIC,
      BIC,
      Shibata,
      HannanQuinn
    ),
  e22$arma.comparison %>%
    transmute(
      Asset = "J303",
      Model,
      LogLikelihood,
      AIC,
      BIC,
      Shibata,
      HannanQuinn
    )
) %>%
  mutate(
    # Standardise the display labels across assets without changing models or estimates.
    Model = dplyr::recode(
      Model,
      "ARMA(0,0)-EGARCH" = "EGARCH ARMA(0,0)",
      "ARMA(1,0)-EGARCH" = "EGARCH ARMA(1,0)",
      "ARMA(0,1)-EGARCH" = "EGARCH ARMA(0,1)",
      "ARMA(1,1)-EGARCH" = "EGARCH ARMA(1,1)",
      .default = Model
    )
  )

write_table(
  egarch_mean_comparison,
  "T03a_EGARCH_Mean_Specification_Comparison",
  appendix = TRUE
)


# 9. EGARCH Parameter Estimates and Final Diagnostics

# Report the fitted EGARCH parameters for both series, including the
# asymmetry term (gamma1) that motivates the asymmetric specification.

# The rugarch coefficient matrix can carry leading spaces in its column
# names. Standardise the four coefficient-statistic columns by position,
# preserving the underlying estimates and p-values.
prepare_egarch_parameter_table <- function(x, asset_label) {
  x <- as.data.frame(x, check.names = FALSE)
  
  if (ncol(x) != 6) {
    stop(
      paste0(
        "Unexpected EGARCH parameter-table structure for ", asset_label,
        ". Expected 6 columns (Model, Parameter, and 4 coefficient statistics); found: ",
        paste(names(x), collapse = ", ")
      )
    )
  }
  
  names(x) <- c(
    "Model",
    "Parameter",
    "Estimate",
    "Std. Error",
    "t value",
    "Pr(>|t|)"
  )
  
  # The Asset column already identifies J303; remove its duplicate prefix from the model label.
  if (identical(asset_label, "J303")) {
    x$Model <- sub("^J303[[:space:]]+", "", as.character(x$Model))
  }
  
  x$Asset <- asset_label
  
  x %>%
    dplyr::select(
      Asset,
      Model,
      Parameter,
      Estimate,
      Std_Error = `Std. Error`,
      t_Statistic = `t value`,
      P_Value = `Pr(>|t|)`
    )
}

parameter_bitcoin <- prepare_egarch_parameter_table(
  e21$egarch_parameter_results,
  "Bitcoin"
)

parameter_j303 <- prepare_egarch_parameter_table(
  e22$J303_egarch_parameter_results,
  "J303"
)

egarch_parameter_table <- dplyr::bind_rows(
  parameter_bitcoin,
  parameter_j303
)

write_table(
  egarch_parameter_table,
  "T04_EGARCH_Parameter_Estimates"
)

# Display-only version for Chapter 3, Table 3.3 Panel A.
# Keep the original T04 export unchanged; show Asset and Model only on the
# first row of each Asset/Model group for a cleaner LaTeX table.
t04_parameter_display <- egarch_parameter_table %>%
  dplyr::group_by(Asset, Model) %>%
  dplyr::mutate(.group_row = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Asset = dplyr::if_else(.group_row == 1L, as.character(Asset), ""),
    Model = dplyr::if_else(.group_row == 1L, as.character(Model), "")
  ) %>%
  dplyr::select(-.group_row)

write_table(
  t04_parameter_display,
  "T04_EGARCH_Parameter_Estimates_Display"
)

if (
  nrow(t04_parameter_display) != 12 ||
  ncol(t04_parameter_display) != 7 ||
  sum(nzchar(t04_parameter_display$Asset)) != 2 ||
  sum(nzchar(t04_parameter_display$Model)) != 2
) {
  stop("T04 display validation failed: expected 12 rows and exactly two visible Asset/Model group labels.")
}


# Compact final-model diagnostics: persistence, remaining residual dependence,
# ARCH effects and Nyblom joint parameter stability.

garch_diagnostic_row <- function(
    fit,
    asset,
    model_name
) {
  z <- residuals(
    fit,
    standardize = TRUE
  )
  
  lb1 <- Box.test(
    z,
    lag = 20,
    type = "Ljung-Box"
  )
  
  lb2 <- Box.test(
    z^2,
    lag = 20,
    type = "Ljung-Box"
  )
  
  arch <- ArchTest(
    z,
    lags = 12
  )
  
  persistence_value <- tryCatch(
    as.numeric(persistence(fit)[1]),
    error = function(e) NA_real_
  )
  
  nyblom_result <- tryCatch(
    nyblom(fit),
    error = function(e) NULL
  )
  
  ny_joint <- if (!is.null(nyblom_result)) {
    as.numeric(nyblom_result$JointStat)
  } else {
    NA_real_
  }
  
  get_joint_critical <- function(level) {
    if (
      !is.null(nyblom_result) &&
      !is.null(nyblom_result$JointCritical) &&
      level %in% names(nyblom_result$JointCritical)
    ) {
      return(as.numeric(nyblom_result$JointCritical[[level]]))
    }
    NA_real_
  }
  
  data.frame(
    Asset = asset,
    Model = model_name,
    Persistence = persistence_value,
    Ljung_Box_Residual_p = lb1$p.value,
    Ljung_Box_Squared_p = lb2$p.value,
    ARCH_LM_p = arch$p.value,
    Nyblom_Joint = ny_joint,
    Nyblom_Joint_Critical_10pct = get_joint_critical("10%"),
    Nyblom_Joint_Critical_5pct = get_joint_critical("5%"),
    Nyblom_Joint_Critical_1pct = get_joint_critical("1%"),
    stringsAsFactors = FALSE
  )
}


garch_diagnostics <- rbind(
  
  garch_diagnostic_row(
    e21$fit.egarch,
    "Bitcoin",
    "ARMA(0,0)-EGARCH(1,1)-t"
  ),
  
  garch_diagnostic_row(
    e22$fit.egarch,
    "J303",
    "ARMA(0,0)-EGARCH(1,1)-t"
  )
)

rownames(garch_diagnostics) <- NULL

write_table(
  garch_diagnostics,
  "T05_Final_GARCH_Diagnostics",
  display_headers = c(
    Asset = "Asset",
    Model = "Model",
    Persistence = "Persistence",
    Ljung_Box_Residual_p = "LB p (residuals)",
    Ljung_Box_Squared_p = "LB p (squared resid.)",
    ARCH_LM_p = "ARCH LM p",
    Nyblom_Joint = "Nyblom joint stat.",
    Nyblom_Joint_Critical_10pct = "Nyblom crit. 10 pct",
    Nyblom_Joint_Critical_5pct = "Nyblom crit. 5 pct",
    Nyblom_Joint_Critical_1pct = "Nyblom crit. 1 pct"
  )
)


# Detailed residual-test p-values and parameter stability are covered by T05_Final_GARCH_Diagnostics.


# 10. Weekday Return Checks

# The weekday checks remain available in Script 1. Henco requested that the
# standalone Appendix A.7 table be removed, so Script 9 does not export it.


# 11. GPR -> Bitcoin Volatility

# Preserve the correlation result as a supplementary output. Henco's direction
# is that this result can be reported in prose rather than as a Chapter 4 table.
write_table(
  e3$correlation.summary,
  "T09_GPR_Bitcoin_Correlation",
  appendix = TRUE
)

# Main regression effects are standardised and reported in percentage points of
# daily conditional volatility per one-SD increase in GPR.

get_term_coefficient <- function(model, term, multiplier = 1) {
  if (term %in% names(coef(model))) {
    return(unname(coef(model)[term]) * multiplier)
  }
  NA_real_
}


get_term_p_value <- function(nw, term) {
  if (term %in% rownames(nw)) {
    return(unname(nw[term, "Pr(>|t|)"]))
  }
  NA_real_
}


build_gpr_model_rows <- function(
    models,
    nw_models,
    sample_label,
    delta_dynamic1,
    test_dynamic1,
    delta_dynamic2,
    test_dynamic2
) {
  
  model_names <- names(models)
  
  dplyr::bind_rows(
    lapply(
      model_names,
      function(model_name) {
        
        model <- models[[model_name]]
        nw <- nw_models[[model_name]]
        
        delta_r2 <- NA_real_
        joint_f <- NA_real_
        joint_p <- NA_real_
        
        if (identical(
          model_name,
          "Dynamic: BTC Volatility Lag1 + GPR Lags0-2"
        )) {
          delta_r2 <- delta_dynamic1
          joint_f <- test_dynamic1$F[2]
          joint_p <- test_dynamic1$`Pr(>F)`[2]
        }
        
        if (identical(
          model_name,
          "Dynamic: BTC Volatility Lags1-2 + GPR Lags0-2"
        )) {
          delta_r2 <- delta_dynamic2
          joint_f <- test_dynamic2$F[2]
          joint_p <- test_dynamic2$`Pr(>F)`[2]
        }
        
        data.frame(
          Sample = sample_label,
          Model = model_name,
          N = nobs(model),
          
          GPR_Lag0_Effect_pp_per_SD =
            get_term_coefficient(model, "GPRD_z", 100),
          GPR_Lag1_Effect_pp_per_SD =
            get_term_coefficient(model, "GPR_Lag1", 100),
          GPR_Lag2_Effect_pp_per_SD =
            get_term_coefficient(model, "GPR_Lag2", 100),
          
          GPR_Lag0_NW_P_Value =
            get_term_p_value(nw, "GPRD_z"),
          GPR_Lag1_NW_P_Value =
            get_term_p_value(nw, "GPR_Lag1"),
          GPR_Lag2_NW_P_Value =
            get_term_p_value(nw, "GPR_Lag2"),
          
          BTC_Volatility_Lag1_Coefficient =
            get_term_coefficient(model, "BTC_Volatility_Lag1"),
          BTC_Volatility_Lag1_NW_P_Value =
            get_term_p_value(nw, "BTC_Volatility_Lag1"),
          
          BTC_Volatility_Lag2_Coefficient =
            get_term_coefficient(model, "BTC_Volatility_Lag2"),
          BTC_Volatility_Lag2_NW_P_Value =
            get_term_p_value(nw, "BTC_Volatility_Lag2"),
          
          Adj_R2 = summary(model)$adj.r.squared,
          Delta_R2 = delta_r2,
          Joint_F_Statistic = joint_f,
          Joint_Wald_P_Value = joint_p
        )
        
      }
    )
  )
}


gpr_main_full <- build_gpr_model_rows(
  e3$models,
  e3$nw.models,
  "Full sample",
  e3$delta_R2_dynamic1,
  e3$partial_F_dynamic1,
  e3$delta_R2_dynamic2,
  e3$partial_F_dynamic2
)

gpr_main_post2017 <- build_gpr_model_rows(
  e3$post2017.models,
  e3$post2017.nw,
  "Post-2017",
  e3$post2017.delta_R2_dynamic1,
  e3$post2017.F_dynamic1,
  e3$post2017.delta_R2_dynamic2,
  e3$post2017.F_dynamic2
)

t10_gpr_main <- dplyr::bind_rows(
  gpr_main_full,
  gpr_main_post2017
)

write_table(
  t10_gpr_main,
  "T10_GPR_Bitcoin_Main_Models"
)


# Table 4.1: display-ready version of the existing GPRD-Bitcoin regressions.
# This preserves the original T10 export and only reformats the same estimates
# and Newey-West p-values into the compact layout used in the thesis.

table41_model_order <- c(
  "Volatility: No Lag",
  "Volatility: 1 Lag",
  "Volatility: 2 Lags",
  "Dynamic: BTC Volatility Lag1 + GPR Lags0-2",
  "Dynamic: BTC Volatility Lags1-2 + GPR Lags0-2"
)

table41_specification_labels <- c(
  "Volatility: No Lag" = "GPRD only",
  "Volatility: 1 Lag" = "GPRD lags 0-1",
  "Volatility: 2 Lags" = "GPRD lags 0-2",
  "Dynamic: BTC Volatility Lag1 + GPR Lags0-2" = "BTC lag 1; GPRD lags 0-2",
  "Dynamic: BTC Volatility Lags1-2 + GPR Lags0-2" = "BTC lags 1-2; GPRD lags 0-2"
)

format_table41_p <- function(p_value, latex = FALSE) {
  if (is.na(p_value) || !is.finite(p_value) || p_value < 0 || p_value > 1) {
    return("-")
  }
  
  p_text <- if (p_value < 0.01) {
    "<0.01"
  } else {
    formatC(p_value, format = "g", digits = 4)
  }
  
  if (latex && startsWith(p_text, "<")) {
    p_text <- paste0("$", p_text, "$")
  }
  
  trimws(p_text)
}

format_table41_coefficient <- function(estimate, p_value, latex = FALSE) {
  if (is.na(estimate) || !is.finite(estimate)) {
    return("-")
  }
  
  p_text <- format_table41_p(p_value, latex = latex)
  paste0(formatC(estimate, format = "f", digits = 4), " (", p_text, ")")
}

format_table41_number <- function(x, digits = 4) {
  if (is.na(x) || !is.finite(x)) {
    return("-")
  }
  formatC(x, format = "f", digits = digits)
}

build_table41_display <- function(latex = FALSE) {
  ordered_rows <- t10_gpr_main %>%
    dplyr::filter(Model %in% table41_model_order) %>%
    dplyr::mutate(
      Sample_Order = match(Sample, c("Full sample", "Post-2017")),
      Model_Order = match(Model, table41_model_order),
      Sample = ifelse(Sample == "Full sample", "Full", Sample),
      Specification = unname(table41_specification_labels[Model])
    ) %>%
    dplyr::arrange(Sample_Order, Model_Order)
  
  display <- data.frame(
    Sample = ordered_rows$Sample,
    Specification = ordered_rows$Specification,
    N = as.integer(ordered_rows$N),
    `GPRD_t (NW p)` = mapply(
      format_table41_coefficient,
      ordered_rows$GPR_Lag0_Effect_pp_per_SD,
      ordered_rows$GPR_Lag0_NW_P_Value,
      MoreArgs = list(latex = latex),
      USE.NAMES = FALSE
    ),
    `GPRD_t-1 (NW p)` = mapply(
      format_table41_coefficient,
      ordered_rows$GPR_Lag1_Effect_pp_per_SD,
      ordered_rows$GPR_Lag1_NW_P_Value,
      MoreArgs = list(latex = latex),
      USE.NAMES = FALSE
    ),
    `GPRD_t-2 (NW p)` = mapply(
      format_table41_coefficient,
      ordered_rows$GPR_Lag2_Effect_pp_per_SD,
      ordered_rows$GPR_Lag2_NW_P_Value,
      MoreArgs = list(latex = latex),
      USE.NAMES = FALSE
    ),
    `Adj. R2` = vapply(ordered_rows$Adj_R2, format_table41_number, character(1)),
    `Delta R2` = vapply(ordered_rows$Delta_R2, format_table41_number, character(1)),
    `Joint F (NW p)` = mapply(
      function(f_statistic, p_value) {
        if (is.na(f_statistic) || !is.finite(f_statistic)) {
          return("-")
        }
        p_text <- format_table41_p(p_value, latex = latex)
        paste0(formatC(f_statistic, format = "f", digits = 4), " (", p_text, ")")
      },
      ordered_rows$Joint_F_Statistic,
      ordered_rows$Joint_Wald_P_Value,
      USE.NAMES = FALSE
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  if (nrow(display) != 10L) {
    stop(
      "Table 4.1 display export failed: expected 10 rows (five specifications for each sample)."
    )
  }
  
  display
}

t10a_table41_display <- build_table41_display(latex = FALSE)
t10a_table41_latex <- build_table41_display(latex = TRUE)

write.csv(
  t10a_table41_display,
  file = file.path(core_table_dir, "T10a_GPR_Bitcoin_Table_41_Display.csv"),
  row.names = FALSE,
  na = ""
)

# Keep TSV column headings as plain text because \csvautobooktabular
# uses header text internally and cannot safely parse embedded LaTeX commands.
# LaTeX math is retained only in the coefficient/p-value cell contents.
names(t10a_table41_latex) <- c(
  "Sample",
  "Specification",
  "N",
  "GPRD t (NW p)",
  "GPRD t-1 (NW p)",
  "GPRD t-2 (NW p)",
  "Adj. R2",
  "Delta R2",
  "Joint F (NW p)"
)

write.table(
  t10a_table41_latex,
  file = file.path(core_table_dir, "T10a_GPR_Bitcoin_Table_41_Display.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE,
  na = ""
)


# Table 4.5: place GPRD, ACT and THREAT dynamic estimates together.
# Coefficients are percentage-point effects per one-SD increase in the
# relevant GPR measure. Joint tests assess the current and two lagged terms.

gprd_dynamic_name <- "Dynamic: BTC Volatility Lags1-2 + GPR Lags0-2"
gprd_dynamic_model <- e3$models[[gprd_dynamic_name]]
gprd_dynamic_nw <- e3$nw.models[[gprd_dynamic_name]]

t11_gprd <- data.frame(
  Measure = "GPRD",
  Sample = "Full sample",
  N = nobs(gprd_dynamic_model),
  GPR_Lag0_Effect_pp_per_SD =
    get_term_coefficient(gprd_dynamic_model, "GPRD_z", 100),
  GPR_Lag1_Effect_pp_per_SD =
    get_term_coefficient(gprd_dynamic_model, "GPR_Lag1", 100),
  GPR_Lag2_Effect_pp_per_SD =
    get_term_coefficient(gprd_dynamic_model, "GPR_Lag2", 100),
  GPR_Lag0_NW_P_Value =
    get_term_p_value(gprd_dynamic_nw, "GPRD_z"),
  GPR_Lag1_NW_P_Value =
    get_term_p_value(gprd_dynamic_nw, "GPR_Lag1"),
  GPR_Lag2_NW_P_Value =
    get_term_p_value(gprd_dynamic_nw, "GPR_Lag2"),
  Adj_R2 = summary(gprd_dynamic_model)$adj.r.squared,
  Delta_R2 = e3$delta_R2_dynamic2,
  Joint_F_Statistic = e3$partial_F_dynamic2$F[2],
  Joint_Wald_P_Value = e3$partial_F_dynamic2$`Pr(>F)`[2]
)

t11_components <- e3$gpr.component.dynamic.summary

if (all(c("delta_R2_ACT", "delta_R2_THREAT") %in% ls(envir = e3))) {
  t11_components$Delta_R2 <- c(
    e3$delta_R2_ACT,
    e3$delta_R2_THREAT
  )
} else {
  stop("Full-precision ACT/THREAT Delta_R2 values were not found in Script 3 output.")
}

t11_components <- t11_components %>%
  mutate(
    Measure = c("GPRD_ACT", "GPRD_THREAT"),
    Sample = "Full sample",
    N = c(
      nobs(e3$component.dynamic.models[[1]]),
      nobs(e3$component.dynamic.models[[2]])
    ),
    Joint_F_Statistic = c(
      e3$partial_F_ACT$F[2],
      e3$partial_F_THREAT$F[2]
    )
  ) %>%
  transmute(
    Measure,
    Sample,
    N,
    GPR_Lag0_Effect_pp_per_SD = GPR_Coefficient_pp,
    GPR_Lag1_Effect_pp_per_SD = GPR_Lag1_Coefficient_pp,
    GPR_Lag2_Effect_pp_per_SD = GPR_Lag2_Coefficient_pp,
    GPR_Lag0_NW_P_Value = NW_pvalue,
    GPR_Lag1_NW_P_Value = NW_Lag1_pvalue,
    GPR_Lag2_NW_P_Value = NW_Lag2_pvalue,
    Adj_R2,
    Delta_R2,
    Joint_F_Statistic,
    Joint_Wald_P_Value = Partial_F_pvalue
  )

# Add the post-2017 ACT and THREAT dynamic models estimated in Script 3.
# Use the full-precision R-squared changes and joint Wald statistics.
t11_components_post2017 <- e3$post2017.component.dynamic.summary

if (all(c(
  "post2017.delta_R2_ACT",
  "post2017.delta_R2_THREAT"
) %in% ls(envir = e3))) {
  t11_components_post2017$Delta_R2 <- c(
    e3$post2017.delta_R2_ACT,
    e3$post2017.delta_R2_THREAT
  )
} else {
  stop("Post-2017 ACT/THREAT Delta_R2 values were not found in Script 3 output.")
}

t11_components_post2017 <- t11_components_post2017 %>%
  mutate(
    Measure = c("GPRD_ACT", "GPRD_THREAT"),
    Sample = "Post-2017",
    N = c(
      nobs(e3$post2017.component.dynamic.models[[1]]),
      nobs(e3$post2017.component.dynamic.models[[2]])
    ),
    Joint_F_Statistic = c(
      e3$post2017.F_ACT$F[2],
      e3$post2017.F_THREAT$F[2]
    )
  ) %>%
  transmute(
    Measure,
    Sample,
    N,
    GPR_Lag0_Effect_pp_per_SD = GPR_Coefficient_pp,
    GPR_Lag1_Effect_pp_per_SD = GPR_Lag1_Coefficient_pp,
    GPR_Lag2_Effect_pp_per_SD = GPR_Lag2_Coefficient_pp,
    GPR_Lag0_NW_P_Value = GPR_NW_pvalue,
    GPR_Lag1_NW_P_Value = GPR_Lag1_NW_pvalue,
    GPR_Lag2_NW_P_Value = GPR_Lag2_NW_pvalue,
    Adj_R2,
    Delta_R2,
    Joint_F_Statistic,
    Joint_Wald_P_Value = Joint_Wald_pvalue
  )

t11_gprd <- t11_gprd %>%
  select(names(t11_components))

t11_component_dynamic <- dplyr::bind_rows(
  t11_gprd,
  t11_components,
  t11_components_post2017
)

# Table 4.2 / Appendix B.2: preserve the same wide-format data and shorten
# only the displayed headings. The output filename and all estimates remain the same.
t11_component_dynamic_display <- t11_component_dynamic %>%
  rename(
    GPR_Lag0_pp_per_SD = GPR_Lag0_Effect_pp_per_SD,
    GPR_Lag1_pp_per_SD = GPR_Lag1_Effect_pp_per_SD,
    GPR_Lag2_pp_per_SD = GPR_Lag2_Effect_pp_per_SD,
    GPR_Lag0_p = GPR_Lag0_NW_P_Value,
    GPR_Lag1_p = GPR_Lag1_NW_P_Value,
    GPR_Lag2_p = GPR_Lag2_NW_P_Value,
    Joint_F = Joint_F_Statistic,
    Joint_p = Joint_Wald_P_Value
  )

# For the existing Table 4.2 number, LaTeX can display the same export twice:
# Panel A with coefficient/p-value pairs and Panel B with fit/joint-test results.
write_table(
  t11_component_dynamic_display,
  "T11_GPR_Component_Dynamic_Regression",
  display_headers = c(
    Measure = "Measure",
    Sample = "Sample",
    N = "N",
    GPR_Lag0_pp_per_SD = "GPR lag 0 (pp/SD)",
    GPR_Lag1_pp_per_SD = "GPR lag 1 (pp/SD)",
    GPR_Lag2_pp_per_SD = "GPR lag 2 (pp/SD)",
    GPR_Lag0_p = "GPR lag 0 p",
    GPR_Lag1_p = "GPR lag 1 p",
    GPR_Lag2_p = "GPR lag 2 p",
    Adj_R2 = "Adj. R-squared",
    Delta_R2 = "Delta R-squared",
    Joint_F = "Joint F",
    Joint_p = "Joint p-value"
  )
)

# Retain the original static ACT/THREAT component estimates as a supplementary
# result. They are distinct from the updated dynamic model above.
write_table(
  e3$gpr.component.summary,
  "T11_GPR_Component_Regression",
  appendix = TRUE
)


# 12. Bitcoin -> J303 Volatility

# Preserve the unconditional Bitcoin-J303 correlation as a supplementary
# output; the main chapter can describe it in prose alongside the model results.
write_table(
  e4$correlation.summary,
  "T13_Bitcoin_J303_Correlation",
  appendix = TRUE
)

# Consolidated model table: full sample and post-2017; Bitcoin volatility
# estimated in ZAR and USD. The dynamic model reports the incremental R-squared
# and the joint Newey-West Wald test for current and lagged Bitcoin volatility.

build_spillover_rows <- function(
    models,
    nw_models,
    sample_label,
    bitcoin_measure,
    restricted_model,
    joint_test
) {
  
  btc_variable <- if (bitcoin_measure == "USD") {
    "BTC_Volatility_USD"
  } else {
    "BTC_Volatility"
  }
  
  btc_lag1 <- paste0(btc_variable, "_Lag1")
  btc_lag2 <- paste0(btc_variable, "_Lag2")
  
  dplyr::bind_rows(
    lapply(
      names(models),
      function(model_name) {
        
        model <- models[[model_name]]
        nw <- nw_models[[model_name]]
        
        restricted_r2 <- NA_real_
        dynamic_full_r2 <- NA_real_
        delta_r2 <- NA_real_
        joint_f <- NA_real_
        joint_p <- NA_real_
        
        if (identical(model_name, "Dynamic 2 Lags")) {
          restricted_r2 <- summary(restricted_model)$r.squared
          dynamic_full_r2 <- summary(model)$r.squared
          delta_r2 <- dynamic_full_r2 - restricted_r2
          joint_f <- joint_test$F[2]
          joint_p <- joint_test$`Pr(>F)`[2]
        }
        
        data.frame(
          Sample = sample_label,
          Bitcoin_Measure = bitcoin_measure,
          Model = model_name,
          N = nobs(model),
          
          BTC_Current_Coefficient =
            get_term_coefficient(model, btc_variable),
          BTC_Current_NW_P_Value =
            get_term_p_value(nw, btc_variable),
          
          BTC_Lag1_Coefficient =
            get_term_coefficient(model, btc_lag1),
          BTC_Lag1_NW_P_Value =
            get_term_p_value(nw, btc_lag1),
          
          BTC_Lag2_Coefficient =
            get_term_coefficient(model, btc_lag2),
          BTC_Lag2_NW_P_Value =
            get_term_p_value(nw, btc_lag2),
          
          J303_Lag1_Coefficient =
            get_term_coefficient(model, "J303_Volatility_Lag1"),
          J303_Lag1_NW_P_Value =
            get_term_p_value(nw, "J303_Volatility_Lag1"),
          
          J303_Lag2_Coefficient =
            get_term_coefficient(model, "J303_Volatility_Lag2"),
          J303_Lag2_NW_P_Value =
            get_term_p_value(nw, "J303_Volatility_Lag2"),
          
          Adj_R2 = summary(model)$adj.r.squared,
          Restricted_R2 = restricted_r2,
          Dynamic_Full_R2 = dynamic_full_r2,
          Delta_R2 = delta_r2,
          Joint_F_Statistic = joint_f,
          Joint_Wald_P_Value = joint_p
        )
        
      }
    )
  )
}


t14_spillover <- dplyr::bind_rows(
  
  build_spillover_rows(
    e4$models,
    e4$nw.models,
    "Full sample",
    "ZAR",
    e4$restricted.dynamic,
    e4$partial.F.dynamic
  ),
  
  build_spillover_rows(
    e4$usd.models,
    e4$usd.nw,
    "Full sample",
    "USD",
    e4$restricted.dynamic.usd,
    e4$partial.F.dynamic.usd
  ),
  
  build_spillover_rows(
    e4$post2017.models,
    e4$post2017.nw,
    "Post-2017",
    "ZAR",
    e4$post2017.restricted.dynamic,
    e4$post2017.partial.F.dynamic
  ),
  
  build_spillover_rows(
    e4$post2017.usd.models,
    e4$post2017.usd.nw,
    "Post-2017",
    "USD",
    e4$post2017.restricted.dynamic.usd,
    e4$post2017.partial.F.dynamic.usd
  )
)

# Preserve the complete wide-format Bitcoin-J303 model comparison.
# For Table 5.1, the LaTeX table can use two vertically stacked panels under the
# existing caption: Panel A for coefficients/p-values; Panel B for fit/joint tests
# and the directional Granger results from T15. The existing exports are retained.
write_table(
  t14_spillover,
  "T14_Bitcoin_J303_Spillover_Models",
  display_headers = c(
    Sample = "Sample",
    Bitcoin_Measure = "Bitcoin measure",
    Model = "Model",
    N = "N",
    BTC_Current_Coefficient = "BTC current coef.",
    BTC_Current_NW_P_Value = "BTC current (NW p)",
    BTC_Lag1_Coefficient = "BTC lag 1 coef.",
    BTC_Lag1_NW_P_Value = "BTC lag 1 (NW p)",
    BTC_Lag2_Coefficient = "BTC lag 2 coef.",
    BTC_Lag2_NW_P_Value = "BTC lag 2 (NW p)",
    J303_Lag1_Coefficient = "J303 lag 1 coef.",
    J303_Lag2_Coefficient = "J303 lag 2 coef.",
    Adj_R2 = "Adj. R-squared",
    Restricted_R2 = "Restricted R-squared",
    Dynamic_Full_R2 = "Full-model R-squared",
    Delta_R2 = "Delta R-squared",
    Joint_F_Statistic = "Joint F",
    Joint_Wald_P_Value = "Joint p-value"
  )
)

# Report the Newey-West Granger tests for both directions, both Bitcoin
# currency measures and both samples. Bitcoin -> J303 excludes same-day BTC.

t15_granger <- dplyr::bind_rows(
  e4$granger.summary %>%
    mutate(Sample = "Full sample"),
  e4$granger.post2017.summary %>%
    mutate(Sample = "Post-2017")
) %>%
  dplyr::select(
    Sample,
    Bitcoin_Measure,
    Direction,
    F_Statistic,
    P_Value
  )

write_table(
  t15_granger,
  "T15_Granger_Causality",
  display_headers = c(
    Sample = "Sample",
    Bitcoin_Measure = "Bitcoin measure",
    Direction = "Granger direction",
    F_Statistic = "F statistic",
    P_Value = "p-value"
  )
)


# Table 5.1: display-ready full-sample and post-2017 ZAR dynamic regressions,
# combined with the matching ZAR Granger-causality p-values. The detailed T14
# and T15 exports remain unchanged; this additional export replaces manual
# hard-coded values in the Chapter 5 LaTeX table.

table51_dynamic_rows <- t14_spillover %>%
  dplyr::filter(
    Bitcoin_Measure == "ZAR",
    Model == "Dynamic 2 Lags"
  ) %>%
  dplyr::mutate(
    Sample_Order = match(Sample, c("Full sample", "Post-2017"))
  ) %>%
  dplyr::arrange(Sample_Order)

if (nrow(table51_dynamic_rows) != 2L ||
    !identical(table51_dynamic_rows$Sample, c("Full sample", "Post-2017"))) {
  stop("Table 5.1 display export failed: expected ZAR Dynamic 2 Lags rows for Full sample and Post-2017.")
}

table51_nw_by_sample <- list(
  "Full sample" = e4$nw.models[["Dynamic 2 Lags"]],
  "Post-2017" = e4$post2017.nw[["Dynamic 2 Lags"]]
)

if (any(vapply(table51_nw_by_sample, is.null, logical(1)))) {
  stop("Table 5.1 display export failed: Newey-West results for the dynamic model are missing.")
}

table51_nw_p <- function(sample_label, term) {
  get_term_p_value(table51_nw_by_sample[[sample_label]], term)
}

table51_granger_p <- function(sample_label, direction_label) {
  match_row <- t15_granger %>%
    dplyr::filter(
      Sample == sample_label,
      Bitcoin_Measure == "ZAR",
      Direction == direction_label
    )
  if (nrow(match_row) != 1L) {
    stop(
      "Table 5.1 display export failed: expected exactly one ZAR Granger result for ",
      sample_label, " / ", direction_label, "."
    )
  }
  as.numeric(match_row$P_Value[[1]])
}

# Table 5.1 only: keep the CSV readable as plain text, but stack each
# Newey-West p-value below its coefficient in the LaTeX-facing TSV. This is a
# display-only transformation; estimates and p-values still come from the
# existing regression output objects.
format_table51_stacked_cell <- function(estimate, p_value, latex = FALSE) {
  if (!latex) {
    return(format_table41_coefficient(estimate, p_value, latex = FALSE))
  }
  if (is.na(estimate) || !is.finite(estimate)) {
    return("-")
  }
  
  p_text <- format_table41_p(p_value, latex = FALSE)
  p_math <- if (startsWith(p_text, "<")) {
    paste0("$p", p_text, "$")
  } else {
    paste0("$p=", p_text, "$")
  }
  
  paste0(
    "\\shortstack{",
    format_table41_number(estimate),
    "\\\\(", p_math, ")}" 
  )
}

format_table51_joint <- function(f_statistic, p_value, latex = FALSE) {
  if (is.na(f_statistic) || !is.finite(f_statistic)) {
    return("-")
  }
  if (latex) {
    return(format_table51_stacked_cell(f_statistic, p_value, latex = TRUE))
  }
  paste0(
    format_table41_number(f_statistic),
    " (", format_table41_p(p_value, latex = FALSE), ")"
  )
}

format_table51_granger_pair <- function(sample_label, latex = FALSE) {
  p_btc_to_j303 <- table51_granger_p(
    sample_label,
    "Bitcoin Volatility -> J303 Volatility"
  )
  p_j303_to_btc <- table51_granger_p(
    sample_label,
    "J303 Volatility -> Bitcoin Volatility"
  )
  
  if (latex) {
    return(paste0(
      "\\shortstack{BTC$\\to$J303: ",
      format_table41_p(p_btc_to_j303, latex = TRUE),
      "\\\\J303$\\to$BTC: ",
      format_table41_p(p_j303_to_btc, latex = TRUE),
      "}"
    ))
  }
  
  paste0(
    format_table41_p(p_btc_to_j303, latex = FALSE),
    " / ",
    format_table41_p(p_j303_to_btc, latex = FALSE)
  )
}

build_table51_display <- function(latex = FALSE) {
  data.frame(
    Sample = table51_dynamic_rows$Sample,
    N = as.integer(table51_dynamic_rows$N),
    `J303 lag 1 coef. (NW p)` = mapply(
      format_table51_stacked_cell,
      table51_dynamic_rows$J303_Lag1_Coefficient,
      vapply(
        table51_dynamic_rows$Sample,
        table51_nw_p,
        numeric(1),
        term = "J303_Volatility_Lag1"
      ),
      MoreArgs = list(latex = latex),
      USE.NAMES = FALSE
    ),
    `J303 lag 2 coef. (NW p)` = mapply(
      format_table51_stacked_cell,
      table51_dynamic_rows$J303_Lag2_Coefficient,
      vapply(
        table51_dynamic_rows$Sample,
        table51_nw_p,
        numeric(1),
        term = "J303_Volatility_Lag2"
      ),
      MoreArgs = list(latex = latex),
      USE.NAMES = FALSE
    ),
    `Bitcoin t (NW p)` = mapply(
      format_table51_stacked_cell,
      table51_dynamic_rows$BTC_Current_Coefficient,
      table51_dynamic_rows$BTC_Current_NW_P_Value,
      MoreArgs = list(latex = latex),
      USE.NAMES = FALSE
    ),
    `Bitcoin t-1 (NW p)` = mapply(
      format_table51_stacked_cell,
      table51_dynamic_rows$BTC_Lag1_Coefficient,
      table51_dynamic_rows$BTC_Lag1_NW_P_Value,
      MoreArgs = list(latex = latex),
      USE.NAMES = FALSE
    ),
    `Bitcoin t-2 (NW p)` = mapply(
      format_table51_stacked_cell,
      table51_dynamic_rows$BTC_Lag2_Coefficient,
      table51_dynamic_rows$BTC_Lag2_NW_P_Value,
      MoreArgs = list(latex = latex),
      USE.NAMES = FALSE
    ),
    `Adj. R2` = vapply(
      table51_dynamic_rows$Adj_R2,
      format_table41_number,
      character(1)
    ),
    `Restricted R2` = vapply(
      table51_dynamic_rows$Restricted_R2,
      format_table41_number,
      character(1)
    ),
    `Full R2` = vapply(
      table51_dynamic_rows$Dynamic_Full_R2,
      format_table41_number,
      character(1)
    ),
    `Delta R2` = vapply(
      table51_dynamic_rows$Delta_R2,
      format_table41_number,
      character(1)
    ),
    `Joint F (NW p)` = mapply(
      format_table51_joint,
      table51_dynamic_rows$Joint_F_Statistic,
      table51_dynamic_rows$Joint_Wald_P_Value,
      MoreArgs = list(latex = latex),
      USE.NAMES = FALSE
    ),
    `Granger p: BTC to J303 / J303 to BTC` = vapply(
      table51_dynamic_rows$Sample,
      format_table51_granger_pair,
      character(1),
      latex = latex
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

t14a_table51_display <- build_table51_display(latex = FALSE)
t14a_table51_latex <- build_table51_display(latex = TRUE)

if (nrow(t14a_table51_display) != 2L || ncol(t14a_table51_display) != 13L) {
  stop("Table 5.1 display export failed: expected 2 sample rows and 13 columns.")
}

# CSV retains plain coefficient/p-value text. The LaTeX-facing TSV stacks
# p-values beneath coefficients (and the joint F statistic) and labels the two
# Granger directions on separate lines, solely for Table 5.1 presentation.
write.csv(
  t14a_table51_display,
  file = file.path(core_table_dir, "T14a_Bitcoin_J303_Table_51_Display.csv"),
  row.names = FALSE,
  na = ""
)

write.table(
  t14a_table51_latex,
  file = file.path(core_table_dir, "T14a_Bitcoin_J303_Table_51_Display.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE,
  na = ""
)


# Table 5.1 alternative layout: turn the table so that model terms run down
# the rows and Full sample / Post-2017 estimates are compared across columns.
# These are additional display-only exports. The existing T14, T15 and T14a
# exports are preserved unchanged, and values are read from the same fitted
# model summaries and Newey-West / Granger result objects.
table51_sample_row <- function(sample_label) {
  x <- table51_dynamic_rows %>% dplyr::filter(Sample == sample_label)
  if (nrow(x) != 1L) {
    stop("Table 5.1 panel export failed: expected one dynamic row for ", sample_label, ".")
  }
  x
}

table51_coefficient_specs <- data.frame(
  Term = c(
    "J303 volatility, lag 1",
    "J303 volatility, lag 2",
    "Bitcoin volatility, current",
    "Bitcoin volatility, lag 1",
    "Bitcoin volatility, lag 2"
  ),
  Estimate_Column = c(
    "J303_Lag1_Coefficient",
    "J303_Lag2_Coefficient",
    "BTC_Current_Coefficient",
    "BTC_Lag1_Coefficient",
    "BTC_Lag2_Coefficient"
  ),
  stringsAsFactors = FALSE
)

build_table51_coefficients_panel <- function(latex = FALSE) {
  full <- table51_sample_row("Full sample")
  post <- table51_sample_row("Post-2017")
  
  full_est <- vapply(
    table51_coefficient_specs$Estimate_Column,
    function(nm) format_table41_number(as.numeric(full[[nm]][1])),
    character(1)
  )
  post_est <- vapply(
    table51_coefficient_specs$Estimate_Column,
    function(nm) format_table41_number(as.numeric(post[[nm]][1])),
    character(1)
  )
  
  nw_terms <- c(
    "J303_Volatility_Lag1",
    "J303_Volatility_Lag2",
    "BTC_Volatility",
    "BTC_Volatility_Lag1",
    "BTC_Volatility_Lag2"
  )
  full_p <- vapply(
    nw_terms,
    function(term) format_table41_p(table51_nw_p("Full sample", term), latex = latex),
    character(1)
  )
  post_p <- vapply(
    nw_terms,
    function(term) format_table41_p(table51_nw_p("Post-2017", term), latex = latex),
    character(1)
  )
  
  data.frame(
    Term = table51_coefficient_specs$Term,
    `Full sample estimate` = full_est,
    `Full sample NW p-value` = full_p,
    `Post-2017 estimate` = post_est,
    `Post-2017 NW p-value` = post_p,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

build_table51_model_fit_panel <- function(latex = FALSE) {
  full <- table51_sample_row("Full sample")
  post <- table51_sample_row("Post-2017")
  
  full_values <- c(
    as.character(as.integer(full$N[[1]])),
    format_table41_number(full$Adj_R2[[1]]),
    format_table41_number(full$Restricted_R2[[1]]),
    format_table41_number(full$Dynamic_Full_R2[[1]]),
    format_table41_number(full$Delta_R2[[1]]),
    format_table41_number(full$Joint_F_Statistic[[1]]),
    format_table41_p(full$Joint_Wald_P_Value[[1]], latex = latex),
    format_table41_p(table51_granger_p("Full sample", "Bitcoin Volatility -> J303 Volatility"), latex = latex),
    format_table41_p(table51_granger_p("Full sample", "J303 Volatility -> Bitcoin Volatility"), latex = latex)
  )
  
  post_values <- c(
    as.character(as.integer(post$N[[1]])),
    format_table41_number(post$Adj_R2[[1]]),
    format_table41_number(post$Restricted_R2[[1]]),
    format_table41_number(post$Dynamic_Full_R2[[1]]),
    format_table41_number(post$Delta_R2[[1]]),
    format_table41_number(post$Joint_F_Statistic[[1]]),
    format_table41_p(post$Joint_Wald_P_Value[[1]], latex = latex),
    format_table41_p(table51_granger_p("Post-2017", "Bitcoin Volatility -> J303 Volatility"), latex = latex),
    format_table41_p(table51_granger_p("Post-2017", "J303 Volatility -> Bitcoin Volatility"), latex = latex)
  )
  
  data.frame(
    Statistic = c(
      "Observations",
      "Adjusted R-squared",
      "Restricted R-squared",
      "Full-model R-squared",
      "Delta R-squared",
      "Joint F-statistic",
      "Joint-test p-value",
      "Granger p-value: Bitcoin to J303",
      "Granger p-value: J303 to Bitcoin"
    ),
    `Full sample` = full_values,
    `Post-2017` = post_values,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

t51a_coefficients_csv <- build_table51_coefficients_panel(latex = FALSE)
t51a_coefficients_tsv <- build_table51_coefficients_panel(latex = TRUE)
t51b_model_fit_csv <- build_table51_model_fit_panel(latex = FALSE)
t51b_model_fit_tsv <- build_table51_model_fit_panel(latex = TRUE)

if (nrow(t51a_coefficients_csv) != 5L || ncol(t51a_coefficients_csv) != 5L ||
    nrow(t51b_model_fit_csv) != 9L || ncol(t51b_model_fit_csv) != 3L) {
  stop("Table 5.1 panel exports failed: expected a 5x5 coefficient panel and a 9x3 model-fit panel.")
}

write.csv(
  t51a_coefficients_csv,
  file = file.path(core_table_dir, "T14c_Bitcoin_J303_Table_51_Coefficients_Display.csv"),
  row.names = FALSE,
  na = ""
)
write.table(
  t51a_coefficients_tsv,
  file = file.path(core_table_dir, "T14c_Bitcoin_J303_Table_51_Coefficients_Display.tsv"),
  sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE, na = ""
)
write.csv(
  t51b_model_fit_csv,
  file = file.path(core_table_dir, "T14d_Bitcoin_J303_Table_51_Model_Fit_Display.csv"),
  row.names = FALSE,
  na = ""
)
write.table(
  t51b_model_fit_tsv,
  file = file.path(core_table_dir, "T14d_Bitcoin_J303_Table_51_Model_Fit_Display.tsv"),
  sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE, na = ""
)


# Table 5.2: display-ready robustness results for ZAR- and USD-based Bitcoin
# volatility in the full and post-2017 samples. This combines the existing
# dynamic-model rows in T14 with the corresponding directional Granger p-values
# in T15. Existing T14 and T15 exports are unchanged.
table52_dynamic_rows <- t14_spillover %>%
  dplyr::filter(Model == "Dynamic 2 Lags") %>%
  dplyr::mutate(
    Sample_Order = match(Sample, c("Full sample", "Post-2017")),
    Bitcoin_Measure_Order = match(Bitcoin_Measure, c("ZAR", "USD"))
  ) %>%
  dplyr::arrange(Sample_Order, Bitcoin_Measure_Order)

if (
  nrow(table52_dynamic_rows) != 4L ||
  !identical(table52_dynamic_rows$Sample, c(
    "Full sample", "Full sample", "Post-2017", "Post-2017"
  )) ||
  !identical(table52_dynamic_rows$Bitcoin_Measure, c("ZAR", "USD", "ZAR", "USD"))
) {
  stop("Table 5.2 display export failed: expected ZAR and USD Dynamic 2 Lags rows for both samples.")
}

table52_granger_p <- function(sample_label, bitcoin_measure, direction_label) {
  match_row <- t15_granger %>%
    dplyr::filter(
      Sample == sample_label,
      Bitcoin_Measure == bitcoin_measure,
      Direction == direction_label
    )
  if (nrow(match_row) != 1L) {
    stop(
      "Table 5.2 display export failed: expected exactly one Granger result for ",
      sample_label, " / ", bitcoin_measure, " / ", direction_label, "."
    )
  }
  as.numeric(match_row$P_Value[[1]])
}

build_table52_display <- function(latex = FALSE) {
  data.frame(
    Sample = table52_dynamic_rows$Sample,
    `Bitcoin measure` = table52_dynamic_rows$Bitcoin_Measure,
    `Bitcoin t (NW p)` = mapply(
      format_table41_coefficient,
      table52_dynamic_rows$BTC_Current_Coefficient,
      table52_dynamic_rows$BTC_Current_NW_P_Value,
      MoreArgs = list(latex = latex),
      USE.NAMES = FALSE
    ),
    `Bitcoin t-1 (NW p)` = mapply(
      format_table41_coefficient,
      table52_dynamic_rows$BTC_Lag1_Coefficient,
      table52_dynamic_rows$BTC_Lag1_NW_P_Value,
      MoreArgs = list(latex = latex),
      USE.NAMES = FALSE
    ),
    `Bitcoin t-2 (NW p)` = mapply(
      format_table41_coefficient,
      table52_dynamic_rows$BTC_Lag2_Coefficient,
      table52_dynamic_rows$BTC_Lag2_NW_P_Value,
      MoreArgs = list(latex = latex),
      USE.NAMES = FALSE
    ),
    `Adj. R2` = vapply(
      table52_dynamic_rows$Adj_R2,
      format_table41_number,
      character(1)
    ),
    `Delta R2` = vapply(
      table52_dynamic_rows$Delta_R2,
      format_table41_number,
      character(1)
    ),
    `Joint F` = vapply(
      table52_dynamic_rows$Joint_F_Statistic,
      format_table41_number,
      character(1)
    ),
    `Joint-test p` = vapply(
      table52_dynamic_rows$Joint_Wald_P_Value,
      format_table41_p,
      character(1),
      latex = latex
    ),
    `Granger BTC-to-J303 p` = vapply(
      seq_len(nrow(table52_dynamic_rows)),
      function(i) {
        format_table41_p(
          table52_granger_p(
            table52_dynamic_rows$Sample[[i]],
            table52_dynamic_rows$Bitcoin_Measure[[i]],
            "Bitcoin Volatility -> J303 Volatility"
          ),
          latex = latex
        )
      },
      character(1)
    ),
    `Granger J303-to-BTC p` = vapply(
      seq_len(nrow(table52_dynamic_rows)),
      function(i) {
        format_table41_p(
          table52_granger_p(
            table52_dynamic_rows$Sample[[i]],
            table52_dynamic_rows$Bitcoin_Measure[[i]],
            "J303 Volatility -> Bitcoin Volatility"
          ),
          latex = latex
        )
      },
      character(1)
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

t14b_table52_display <- build_table52_display(latex = FALSE)
t14b_table52_latex <- build_table52_display(latex = TRUE)

if (nrow(t14b_table52_display) != 4L || ncol(t14b_table52_display) != 11L) {
  stop("Table 5.2 display export failed: expected 4 sample/measure rows and 11 columns.")
}

write.csv(
  t14b_table52_display,
  file = file.path(core_table_dir, "T14b_Bitcoin_J303_Table_52_Display.csv"),
  row.names = FALSE,
  na = ""
)

write.table(
  t14b_table52_latex,
  file = file.path(core_table_dir, "T14b_Bitcoin_J303_Table_52_Display.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE,
  na = ""
)


write_table(
  e4$diagnostic.summary,
  "T18_Bitcoin_J303_Diagnostics",
  appendix = TRUE
)

# Keep the dynamic Breusch-Godfrey summary as well: T18 contains its p-value,
# but not the statistic reported by this separate test object.
write_table(
  e4$dynamic.serial.summary,
  "T19_Bitcoin_J303_Dynamic_Serial_Correlation",
  appendix = TRUE
)


# 13. GPR Regimes: Bitcoin Volatility

gpr_thresholds <- data.frame(
  Measure = c(
    "GPRD",
    "GPRD_ACT",
    "GPRD_THREAT"
  ),
  Threshold = c(
    master$GPRD_threshold,
    master$GPRD_ACT_threshold,
    master$GPRD_THREAT_threshold
  )
)

write_table(
  gpr_thresholds,
  "T20_GPR_Thresholds",
  appendix = TRUE
)

write_table(
  e5$regime.summary,
  "T21_GPR_Regime_Descriptives",
  appendix = TRUE
)


# Consolidate the GPRD, ACT and THREAT dynamic regime comparisons into one
# appendix table, corresponding to the previous separate regime tables.
# Regime-split HAC p-values are supplementary: formal regime differences are
# tested using the full-sample interaction results exported from Script 5.

get_nw_p_value <- function(nw_result, term) {
  if (
    is.null(nw_result) ||
    is.null(rownames(nw_result)) ||
    !term %in% rownames(nw_result) ||
    !"Pr(>|t|)" %in% colnames(nw_result)
  ) {
    return(NA_real_)
  }
  as.numeric(nw_result[term, "Pr(>|t|)"])
}

get_regime_btc_lag_p_values <- function(nw_low, nw_high) {
  data.frame(
    BTC_Vol_Lag1_NW_p = c(
      get_nw_p_value(nw_low, "BTC_Volatility_Lag1"),
      get_nw_p_value(nw_high, "BTC_Volatility_Lag1")
    ),
    BTC_Vol_Lag2_NW_p = c(
      get_nw_p_value(nw_low, "BTC_Volatility_Lag2"),
      get_nw_p_value(nw_high, "BTC_Volatility_Lag2")
    )
  )
}

t22_gprd <- e5$dynamic.comparison %>%
  mutate(
    Measure = "GPRD",
    N = c(
      nobs(e5$model.low.dynamic),
      nobs(e5$model.high.dynamic)
    ),
    BTC_Vol_Lag1_NW_p = get_regime_btc_lag_p_values(
      e5$nw.low.dynamic,
      e5$nw.high.dynamic
    )$BTC_Vol_Lag1_NW_p,
    BTC_Vol_Lag2_NW_p = get_regime_btc_lag_p_values(
      e5$nw.low.dynamic,
      e5$nw.high.dynamic
    )$BTC_Vol_Lag2_NW_p,
    BTC_Vol_Lag2 = c(
      coef(e5$model.low.dynamic)["BTC_Volatility_Lag2"],
      coef(e5$model.high.dynamic)["BTC_Volatility_Lag2"]
    ),
    Adj_R2 = c(
      summary(e5$model.low.dynamic)$adj.r.squared,
      summary(e5$model.high.dynamic)$adj.r.squared
    ),
    Residual_SE = c(
      summary(e5$model.low.dynamic)$sigma,
      summary(e5$model.high.dynamic)$sigma
    )
  ) %>%
  rename(
    GPR_Lag0 = GPRD,
    GPR_Lag0_NW_p = GPRD_NW_p,
    GPR_Lag1 = GPRD_Lag1,
    GPR_Lag1_NW_p = GPRD_Lag1_NW_p,
    GPR_Lag2 = GPRD_Lag2,
    GPR_Lag2_NW_p = GPRD_Lag2_NW_p
  ) %>%
  left_join(
    e5$dynamic.incremental.r2,
    by = "Regime"
  )


t22_act <- e5$act.dynamic.comparison %>%
  mutate(
    Measure = "GPRD_ACT",
    N = c(
      nobs(e5$model.low.act.dynamic),
      nobs(e5$model.high.act.dynamic)
    ),
    BTC_Vol_Lag1_NW_p = get_regime_btc_lag_p_values(
      e5$nw.low.act.dynamic,
      e5$nw.high.act.dynamic
    )$BTC_Vol_Lag1_NW_p,
    BTC_Vol_Lag2_NW_p = get_regime_btc_lag_p_values(
      e5$nw.low.act.dynamic,
      e5$nw.high.act.dynamic
    )$BTC_Vol_Lag2_NW_p
  ) %>%
  left_join(
    e5$act.incremental.r2,
    by = "Regime"
  )


t22_threat <- e5$threat.dynamic.comparison %>%
  mutate(
    Measure = "GPRD_THREAT",
    N = c(
      nobs(e5$model.low.threat.dynamic),
      nobs(e5$model.high.threat.dynamic)
    ),
    BTC_Vol_Lag1_NW_p = get_regime_btc_lag_p_values(
      e5$nw.low.threat.dynamic,
      e5$nw.high.threat.dynamic
    )$BTC_Vol_Lag1_NW_p,
    BTC_Vol_Lag2_NW_p = get_regime_btc_lag_p_values(
      e5$nw.low.threat.dynamic,
      e5$nw.high.threat.dynamic
    )$BTC_Vol_Lag2_NW_p
  ) %>%
  left_join(
    e5$threat.incremental.r2,
    by = "Regime"
  )


t22_gpr_regimes <- dplyr::bind_rows(
  t22_gprd,
  t22_act,
  t22_threat
) %>%
  relocate(
    Measure,
    Regime,
    N
  ) %>%
  rename(
    GPR_Lag0_Effect_pp_per_SD = GPR_Lag0,
    GPR_Lag0_NW_P_Value = GPR_Lag0_NW_p,
    GPR_Lag1_Effect_pp_per_SD = GPR_Lag1,
    GPR_Lag1_NW_P_Value = GPR_Lag1_NW_p,
    GPR_Lag2_Effect_pp_per_SD = GPR_Lag2,
    GPR_Lag2_NW_P_Value = GPR_Lag2_NW_p
  ) %>%
  relocate(
    BTC_Vol_Lag2,
    .before = BTC_Vol_Lag2_NW_p
  )

# Appendix B.5: shorten displayed headings without changing the underlying results.
# The full-width export is retained so LaTeX can use the same file for two panels.
t22_gpr_regimes_display <- t22_gpr_regimes %>%
  rename(
    BTC_Lag1 = BTC_Vol_Lag1,
    BTC_Lag1_p = BTC_Vol_Lag1_NW_p,
    GPR_Lag0_pp_per_SD = GPR_Lag0_Effect_pp_per_SD,
    GPR_Lag0_p = GPR_Lag0_NW_P_Value,
    GPR_Lag1_pp_per_SD = GPR_Lag1_Effect_pp_per_SD,
    GPR_Lag1_p = GPR_Lag1_NW_P_Value,
    GPR_Lag2_pp_per_SD = GPR_Lag2_Effect_pp_per_SD,
    GPR_Lag2_p = GPR_Lag2_NW_P_Value,
    BTC_Lag2 = BTC_Vol_Lag2,
    BTC_Lag2_p = BTC_Vol_Lag2_NW_p,
    Resid_SE = Residual_SE,
    Partial_F = Partial_F_Statistic,
    Partial_F_p = Partial_F_P_Value
  )

write_table(
  t22_gpr_regimes_display,
  "T22_GPR_Regime_Dynamic_Models",
  appendix = TRUE,
  display_headers = c(
    Measure = "Measure",
    Regime = "Regime",
    N = "N",
    BTC_Lag1 = "BTC lag 1 coef.",
    BTC_Lag1_p = "BTC lag 1 p",
    GPR_Lag0_pp_per_SD = "GPR lag 0 (pp/SD)",
    GPR_Lag0_p = "GPR lag 0 p",
    GPR_Lag1_pp_per_SD = "GPR lag 1 (pp/SD)",
    GPR_Lag1_p = "GPR lag 1 p",
    GPR_Lag2_pp_per_SD = "GPR lag 2 (pp/SD)",
    GPR_Lag2_p = "GPR lag 2 p",
    BTC_Lag2 = "BTC lag 2 coef.",
    BTC_Lag2_p = "BTC lag 2 p",
    Adj_R2 = "Adj. R-squared",
    Resid_SE = "Residual SE",
    Restricted_R2 = "Restricted R-squared",
    Full_R2 = "Full-model R-squared",
    Delta_R2 = "Delta R-squared",
    Partial_F = "Partial F",
    Partial_F_p = "Partial F p-value"
  )
)


# Appendix B.5: two display-only panels from the existing regime models.
# Panel A pairs each coefficient with its Newey-West p-value in one cell.
# Panel B reports model fit and partial F tests. The original T22 export,
# fitted results, and existing table filenames are left unchanged.
format_t22_stacked_cell <- function(estimate, p_value, latex = FALSE) {
  estimate_numeric <- suppressWarnings(as.numeric(estimate))
  p_numeric <- suppressWarnings(as.numeric(p_value))
  estimate_text <- format_table41_number(estimate_numeric)
  p_text <- format_table41_p(p_numeric, latex = FALSE)
  if (identical(estimate_text, "-")) {
    return("-")
  }
  if (latex) {
    # For small p-values, display p < 0.01 (never p = < 0.01).
    p_label <- if (startsWith(p_text, "<")) {
      paste0("$p", p_text, "$")
    } else {
      paste0("$p=", p_text, "$")
    }
    return(paste0("\\shortstack{$", estimate_text, "$\\\\(", p_label, ")}"))
  }
  paste0(estimate_text, " (p", if (startsWith(p_text, "<")) "" else "=", p_text, ")")
}

t22_b5_measure_labels <- dplyr::recode(
  as.character(t22_gpr_regimes_display$Measure),
  GPRD = "GPRD",
  GPRD_ACT = "GPRD ACT",
  GPRD_THREAT = "GPRD THREAT"
)

t22b5_coefficients_csv <- data.frame(
  Measure = t22_b5_measure_labels,
  Regime = as.character(t22_gpr_regimes_display$Regime),
  N = as.character(as.integer(t22_gpr_regimes_display$N)),
  `GPR lag 0` = mapply(
    format_t22_stacked_cell,
    t22_gpr_regimes_display$GPR_Lag0_pp_per_SD,
    t22_gpr_regimes_display$GPR_Lag0_p,
    MoreArgs = list(latex = FALSE), USE.NAMES = FALSE
  ),
  `GPR lag 1` = mapply(
    format_t22_stacked_cell,
    t22_gpr_regimes_display$GPR_Lag1_pp_per_SD,
    t22_gpr_regimes_display$GPR_Lag1_p,
    MoreArgs = list(latex = FALSE), USE.NAMES = FALSE
  ),
  `GPR lag 2` = mapply(
    format_t22_stacked_cell,
    t22_gpr_regimes_display$GPR_Lag2_pp_per_SD,
    t22_gpr_regimes_display$GPR_Lag2_p,
    MoreArgs = list(latex = FALSE), USE.NAMES = FALSE
  ),
  `Bitcoin lag 1` = mapply(
    format_t22_stacked_cell,
    t22_gpr_regimes_display$BTC_Lag1,
    t22_gpr_regimes_display$BTC_Lag1_p,
    MoreArgs = list(latex = FALSE), USE.NAMES = FALSE
  ),
  `Bitcoin lag 2` = mapply(
    format_t22_stacked_cell,
    t22_gpr_regimes_display$BTC_Lag2,
    t22_gpr_regimes_display$BTC_Lag2_p,
    MoreArgs = list(latex = FALSE), USE.NAMES = FALSE
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

t22b5_coefficients_tsv <- data.frame(
  Measure = t22_b5_measure_labels,
  Regime = as.character(t22_gpr_regimes_display$Regime),
  N = as.character(as.integer(t22_gpr_regimes_display$N)),
  `GPR lag 0` = mapply(
    format_t22_stacked_cell,
    t22_gpr_regimes_display$GPR_Lag0_pp_per_SD,
    t22_gpr_regimes_display$GPR_Lag0_p,
    MoreArgs = list(latex = TRUE), USE.NAMES = FALSE
  ),
  `GPR lag 1` = mapply(
    format_t22_stacked_cell,
    t22_gpr_regimes_display$GPR_Lag1_pp_per_SD,
    t22_gpr_regimes_display$GPR_Lag1_p,
    MoreArgs = list(latex = TRUE), USE.NAMES = FALSE
  ),
  `GPR lag 2` = mapply(
    format_t22_stacked_cell,
    t22_gpr_regimes_display$GPR_Lag2_pp_per_SD,
    t22_gpr_regimes_display$GPR_Lag2_p,
    MoreArgs = list(latex = TRUE), USE.NAMES = FALSE
  ),
  `Bitcoin lag 1` = mapply(
    format_t22_stacked_cell,
    t22_gpr_regimes_display$BTC_Lag1,
    t22_gpr_regimes_display$BTC_Lag1_p,
    MoreArgs = list(latex = TRUE), USE.NAMES = FALSE
  ),
  `Bitcoin lag 2` = mapply(
    format_t22_stacked_cell,
    t22_gpr_regimes_display$BTC_Lag2,
    t22_gpr_regimes_display$BTC_Lag2_p,
    MoreArgs = list(latex = TRUE), USE.NAMES = FALSE
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

build_t22b5_model_fit_panel <- function(latex = FALSE) {
  data.frame(
    Measure = t22_b5_measure_labels,
    Regime = as.character(t22_gpr_regimes_display$Regime),
    `Adjusted R-squared` = vapply(
      t22_gpr_regimes_display$Adj_R2,
      function(x) format_table41_number(as.numeric(x)), character(1)
    ),
    `Residual SE` = vapply(
      t22_gpr_regimes_display$Resid_SE,
      function(x) format_table41_number(as.numeric(x)), character(1)
    ),
    `Restricted R-squared` = vapply(
      t22_gpr_regimes_display$Restricted_R2,
      function(x) format_table41_number(as.numeric(x)), character(1)
    ),
    `Full-model R-squared` = vapply(
      t22_gpr_regimes_display$Full_R2,
      function(x) format_table41_number(as.numeric(x)), character(1)
    ),
    `Delta R-squared` = vapply(
      t22_gpr_regimes_display$Delta_R2,
      function(x) format_table41_number(as.numeric(x)), character(1)
    ),
    `Partial F` = vapply(
      t22_gpr_regimes_display$Partial_F,
      function(x) format_table41_number(as.numeric(x)), character(1)
    ),
    `Partial F p-value` = vapply(
      t22_gpr_regimes_display$Partial_F_p,
      function(x) format_table41_p(as.numeric(x), latex = latex), character(1)
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

t22b5_model_fit_csv <- build_t22b5_model_fit_panel(latex = FALSE)
t22b5_model_fit_tsv <- build_t22b5_model_fit_panel(latex = TRUE)

if (
  nrow(t22b5_coefficients_csv) != 6L || ncol(t22b5_coefficients_csv) != 8L ||
  nrow(t22b5_model_fit_csv) != 6L || ncol(t22b5_model_fit_csv) != 9L
) {
  stop("Appendix B.5 display export failed: expected a 6x8 coefficient panel and a 6x9 model-fit panel.")
}

write.csv(
  t22b5_coefficients_csv,
  file = file.path(appendix_table_dir, "T22a_GPR_Regime_Dynamic_Coefficients_Display.csv"),
  row.names = FALSE, na = ""
)
write.table(
  t22b5_coefficients_tsv,
  file = file.path(appendix_table_dir, "T22a_GPR_Regime_Dynamic_Coefficients_Display.tsv"),
  sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE, na = ""
)
write.csv(
  t22b5_model_fit_csv,
  file = file.path(appendix_table_dir, "T22b_GPR_Regime_Dynamic_Model_Fit_Display.csv"),
  row.names = FALSE, na = ""
)
write.table(
  t22b5_model_fit_tsv,
  file = file.path(appendix_table_dir, "T22b_GPR_Regime_Dynamic_Model_Fit_Display.tsv"),
  sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE, na = ""
)

# 14. GPR Regime Interaction Models

# Consolidate the GPRD, ACT, THREAT and post-2017 ACT interaction coefficients
# in one appendix table. The estimates and robust standard errors use the
# percentage-point scaling applied by Script 5.

extract_interaction_rows <- function(
    nw,
    terms,
    labels,
    measure,
    sample_label
) {
  
  data.frame(
    Measure = measure,
    Sample = sample_label,
    Variable = labels,
    Estimate = vapply(
      terms,
      function(term) unname(nw[term, "Estimate"]),
      numeric(1)
    ),
    Robust_SE = vapply(
      terms,
      function(term) unname(nw[term, "Std. Error"]),
      numeric(1)
    ),
    P_Value = vapply(
      terms,
      function(term) unname(nw[term, "Pr(>|t|)"]),
      numeric(1)
    )
  )
}


t25_gpr_interactions <- dplyr::bind_rows(
  
  extract_interaction_rows(
    e5$interaction.nw,
    c(
      "GPRD",
      "GPR_RegimeElevated GPR",
      "GPRD:GPR_RegimeElevated GPR"
    ),
    c(
      "GPR slope (pp per 1-SD GPR)",
      "Elevated-GPR level shift (pp daily volatility)",
      "GPR × elevated-GPR slope difference (pp per 1-SD GPR)"
    ),
    "GPRD",
    "Full sample"
  ),
  
  extract_interaction_rows(
    e5$interaction.act.nw,
    c(
      "GPRD_ACT",
      "GPRD_ACT_RegimeElevated GPR",
      "GPRD_ACT:GPRD_ACT_RegimeElevated GPR"
    ),
    c(
      "GPR slope (pp per 1-SD GPR)",
      "Elevated-GPR level shift (pp daily volatility)",
      "GPR × elevated-GPR slope difference (pp per 1-SD GPR)"
    ),
    "GPRD_ACT",
    "Full sample"
  ),
  
  extract_interaction_rows(
    e5$interaction.threat.nw,
    c(
      "GPRD_THREAT",
      "GPRD_THREAT_RegimeElevated GPR",
      "GPRD_THREAT:GPRD_THREAT_RegimeElevated GPR"
    ),
    c(
      "GPR slope (pp per 1-SD GPR)",
      "Elevated-GPR level shift (pp daily volatility)",
      "GPR × elevated-GPR slope difference (pp per 1-SD GPR)"
    ),
    "GPRD_THREAT",
    "Full sample"
  ),
  
  extract_interaction_rows(
    e5$post2017.act.interaction.nw,
    c(
      "GPRD_ACT",
      "GPRD_ACT_RegimeElevated GPR",
      "GPRD_ACT:GPRD_ACT_RegimeElevated GPR"
    ),
    c(
      "GPR slope (pp per 1-SD GPR)",
      "Elevated-GPR level shift (pp daily volatility)",
      "GPR × elevated-GPR slope difference (pp per 1-SD GPR)"
    ),
    "GPRD_ACT",
    "Post-2017"
  )
)

write_table(
  t25_gpr_interactions,
  "T25_GPR_Regime_Interaction_Models",
  appendix = TRUE
)


# 15. GPR Regimes: Bitcoin -> J303

# Regime-split tables are supplementary comparisons. The formal comparison
# of relationships across regimes is supplied by T29b from the full sample.

t28_gprd <- e7$dynamic.comparison %>%
  mutate(
    Measure = "GPRD",
    N = c(
      nobs(e7$model.low.dynamic),
      nobs(e7$model.high.dynamic)
    ),
    Adj_R2 = c(
      summary(e7$model.low.dynamic)$adj.r.squared,
      summary(e7$model.high.dynamic)$adj.r.squared
    ),
    Residual_SE = c(
      summary(e7$model.low.dynamic)$sigma,
      summary(e7$model.high.dynamic)$sigma
    )
  ) %>%
  left_join(
    e7$gprd.incremental.r2.partial %>%
      select(
        Regime,
        Restricted_R2,
        Full_R2,
        Delta_R2,
        Partial_F_Statistic,
        Partial_F_P_Value
      ),
    by = "Regime"
  )


t28_act <- e7$act.dynamic.comparison %>%
  mutate(
    Measure = "GPRD_ACT",
    N = c(
      nobs(e7$model.low.act.dynamic),
      nobs(e7$model.high.act.dynamic)
    ),
    Adj_R2 = c(
      summary(e7$model.low.act.dynamic)$adj.r.squared,
      summary(e7$model.high.act.dynamic)$adj.r.squared
    ),
    Residual_SE = c(
      summary(e7$model.low.act.dynamic)$sigma,
      summary(e7$model.high.act.dynamic)$sigma
    )
  ) %>%
  left_join(
    e7$act.incremental.r2.partial %>%
      select(
        Regime,
        Restricted_R2,
        Full_R2,
        Delta_R2,
        Partial_F_Statistic,
        Partial_F_P_Value
      ),
    by = "Regime"
  )


t28_threat <- e7$threat.dynamic.comparison %>%
  mutate(
    Measure = "GPRD_THREAT",
    N = c(
      nobs(e7$model.low.threat.dynamic),
      nobs(e7$model.high.threat.dynamic)
    ),
    Adj_R2 = c(
      summary(e7$model.low.threat.dynamic)$adj.r.squared,
      summary(e7$model.high.threat.dynamic)$adj.r.squared
    ),
    Residual_SE = c(
      summary(e7$model.low.threat.dynamic)$sigma,
      summary(e7$model.high.threat.dynamic)$sigma
    )
  ) %>%
  left_join(
    e7$threat.incremental.r2.partial %>%
      select(
        Regime,
        Restricted_R2,
        Full_R2,
        Delta_R2,
        Partial_F_Statistic,
        Partial_F_P_Value
      ),
    by = "Regime"
  )


t28_regime_comparison <- dplyr::bind_rows(
  t28_gprd,
  t28_act,
  t28_threat
) %>%
  relocate(
    Measure,
    Regime,
    N
  )

# Export the regime-split dynamic spillover models only. The former B.8
# descriptive-statistics table is explicitly dropped by Henco and is not
# reintroduced into this consolidated table.
# Appendix B.6: shorten the headings while retaining all rows and estimates.
t28_regime_comparison_display <- t28_regime_comparison %>%
  rename(
    J303_Lag1 = J303_Vol_Lag1,
    J303_Lag1_p = J303_Vol_Lag1_NW_p,
    J303_Lag2 = J303_Vol_Lag2,
    J303_Lag2_p = J303_Vol_Lag2_NW_p,
    BTC_Lag0 = BTC_Volatility,
    BTC_Lag0_p = BTC_Volatility_NW_p,
    BTC_Lag1 = BTC_Volatility_Lag1,
    BTC_Lag1_p = BTC_Volatility_Lag1_NW_p,
    BTC_Lag2 = BTC_Volatility_Lag2,
    BTC_Lag2_p = BTC_Volatility_Lag2_NW_p,
    Resid_SE = Residual_SE,
    Partial_F = Partial_F_Statistic,
    Partial_F_p = Partial_F_P_Value
  )

write_table(
  t28_regime_comparison_display,
  "T28_Bitcoin_J303_Regime_Spillover",
  appendix = TRUE,
  display_headers = c(
    Measure = "Measure",
    Regime = "Regime",
    N = "N",
    J303_Lag1 = "J303 lag 1 coef.",
    J303_Lag1_p = "J303 lag 1 p",
    J303_Lag2 = "J303 lag 2 coef.",
    J303_Lag2_p = "J303 lag 2 p",
    BTC_Lag0 = "BTC current coef.",
    BTC_Lag0_p = "BTC current p",
    BTC_Lag1 = "BTC lag 1 coef.",
    BTC_Lag1_p = "BTC lag 1 p",
    BTC_Lag2 = "BTC lag 2 coef.",
    BTC_Lag2_p = "BTC lag 2 p",
    Adj_R2 = "Adj. R-squared",
    Resid_SE = "Residual SE",
    Restricted_R2 = "Restricted R-squared",
    Full_R2 = "Full-model R-squared",
    Delta_R2 = "Delta R-squared",
    Partial_F = "Partial F",
    Partial_F_p = "Partial F p-value"
  )
)


# Appendix B.6: two display-only panels from the existing Bitcoin-J303
# regime models. Coefficients are paired with their Newey-West p-values in
# Panel A; model fit and partial F tests are in Panel B. The original T28
# export and all fitted model results remain unchanged.
t28_b6_measure_labels <- dplyr::recode(
  as.character(t28_regime_comparison_display$Measure),
  GPRD = "GPRD",
  GPRD_ACT = "GPRD ACT",
  GPRD_THREAT = "GPRD THREAT"
)

t28b6_coefficients_csv <- data.frame(
  Measure = t28_b6_measure_labels,
  Regime = as.character(t28_regime_comparison_display$Regime),
  N = as.character(as.integer(t28_regime_comparison_display$N)),
  `J303 lag 1` = mapply(
    format_t22_stacked_cell,
    t28_regime_comparison_display$J303_Lag1,
    t28_regime_comparison_display$J303_Lag1_p,
    MoreArgs = list(latex = FALSE), USE.NAMES = FALSE
  ),
  `J303 lag 2` = mapply(
    format_t22_stacked_cell,
    t28_regime_comparison_display$J303_Lag2,
    t28_regime_comparison_display$J303_Lag2_p,
    MoreArgs = list(latex = FALSE), USE.NAMES = FALSE
  ),
  `Bitcoin current` = mapply(
    format_t22_stacked_cell,
    t28_regime_comparison_display$BTC_Lag0,
    t28_regime_comparison_display$BTC_Lag0_p,
    MoreArgs = list(latex = FALSE), USE.NAMES = FALSE
  ),
  `Bitcoin lag 1` = mapply(
    format_t22_stacked_cell,
    t28_regime_comparison_display$BTC_Lag1,
    t28_regime_comparison_display$BTC_Lag1_p,
    MoreArgs = list(latex = FALSE), USE.NAMES = FALSE
  ),
  `Bitcoin lag 2` = mapply(
    format_t22_stacked_cell,
    t28_regime_comparison_display$BTC_Lag2,
    t28_regime_comparison_display$BTC_Lag2_p,
    MoreArgs = list(latex = FALSE), USE.NAMES = FALSE
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

t28b6_coefficients_tsv <- data.frame(
  Measure = t28_b6_measure_labels,
  Regime = as.character(t28_regime_comparison_display$Regime),
  N = as.character(as.integer(t28_regime_comparison_display$N)),
  `J303 lag 1` = mapply(
    format_t22_stacked_cell,
    t28_regime_comparison_display$J303_Lag1,
    t28_regime_comparison_display$J303_Lag1_p,
    MoreArgs = list(latex = TRUE), USE.NAMES = FALSE
  ),
  `J303 lag 2` = mapply(
    format_t22_stacked_cell,
    t28_regime_comparison_display$J303_Lag2,
    t28_regime_comparison_display$J303_Lag2_p,
    MoreArgs = list(latex = TRUE), USE.NAMES = FALSE
  ),
  `Bitcoin current` = mapply(
    format_t22_stacked_cell,
    t28_regime_comparison_display$BTC_Lag0,
    t28_regime_comparison_display$BTC_Lag0_p,
    MoreArgs = list(latex = TRUE), USE.NAMES = FALSE
  ),
  `Bitcoin lag 1` = mapply(
    format_t22_stacked_cell,
    t28_regime_comparison_display$BTC_Lag1,
    t28_regime_comparison_display$BTC_Lag1_p,
    MoreArgs = list(latex = TRUE), USE.NAMES = FALSE
  ),
  `Bitcoin lag 2` = mapply(
    format_t22_stacked_cell,
    t28_regime_comparison_display$BTC_Lag2,
    t28_regime_comparison_display$BTC_Lag2_p,
    MoreArgs = list(latex = TRUE), USE.NAMES = FALSE
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

build_t28b6_model_fit_panel <- function(latex = FALSE) {
  data.frame(
    Measure = t28_b6_measure_labels,
    Regime = as.character(t28_regime_comparison_display$Regime),
    `Adjusted R-squared` = vapply(
      t28_regime_comparison_display$Adj_R2,
      function(x) format_table41_number(as.numeric(x)), character(1)
    ),
    `Residual SE` = vapply(
      t28_regime_comparison_display$Resid_SE,
      function(x) format_table41_number(as.numeric(x)), character(1)
    ),
    `Restricted R-squared` = vapply(
      t28_regime_comparison_display$Restricted_R2,
      function(x) format_table41_number(as.numeric(x)), character(1)
    ),
    `Full-model R-squared` = vapply(
      t28_regime_comparison_display$Full_R2,
      function(x) format_table41_number(as.numeric(x)), character(1)
    ),
    `Delta R-squared` = vapply(
      t28_regime_comparison_display$Delta_R2,
      function(x) format_table41_number(as.numeric(x)), character(1)
    ),
    `Partial F` = vapply(
      t28_regime_comparison_display$Partial_F,
      function(x) format_table41_number(as.numeric(x)), character(1)
    ),
    `Partial F p-value` = vapply(
      t28_regime_comparison_display$Partial_F_p,
      function(x) format_table41_p(as.numeric(x), latex = latex), character(1)
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

t28b6_model_fit_csv <- build_t28b6_model_fit_panel(latex = FALSE)
t28b6_model_fit_tsv <- build_t28b6_model_fit_panel(latex = TRUE)

if (
  nrow(t28b6_coefficients_csv) != 6L || ncol(t28b6_coefficients_csv) != 8L ||
  nrow(t28b6_model_fit_csv) != 6L || ncol(t28b6_model_fit_csv) != 9L
) {
  stop("Appendix B.6 display export failed: expected a 6x8 coefficient panel and a 6x9 model-fit panel.")
}

write.csv(
  t28b6_coefficients_csv,
  file = file.path(appendix_table_dir, "T28a_Bitcoin_J303_Regime_Coefficients_Display.csv"),
  row.names = FALSE, na = ""
)
write.table(
  t28b6_coefficients_tsv,
  file = file.path(appendix_table_dir, "T28a_Bitcoin_J303_Regime_Coefficients_Display.tsv"),
  sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE, na = ""
)
write.csv(
  t28b6_model_fit_csv,
  file = file.path(appendix_table_dir, "T28b_Bitcoin_J303_Regime_Model_Fit_Display.csv"),
  row.names = FALSE, na = ""
)
write.table(
  t28b6_model_fit_tsv,
  file = file.path(appendix_table_dir, "T28b_Bitcoin_J303_Regime_Model_Fit_Display.tsv"),
  sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE, na = ""
)

# Table 5.3: consolidate the GPRD, ACT, THREAT and post-2017 ACT dynamic
# interaction estimates. This avoids reporting the alternative measure models
# as a separate table that duplicates the main interaction table.

prepare_j303_interaction_summary <- function(x, measure, sample_label) {
  x %>%
    mutate(
      Measure = measure,
      Sample = sample_label,
      # Script 7 formats the post-2017 p-values as character strings,
      # while the full-sample summaries retain numeric p-values. Convert
      # all p-values to character for row-binding; write_table() then applies
      # the common reporting format to both numeric-origin and text p-values.
      P_Value = as.character(P_Value),
      Variable = dplyr::recode(
        Variable,
        "BTC Volatility" =
          "BTC-volatility slope in lower-GPR regime",
        "Elevated GPR" =
          "Elevated-GPR intercept shift",
        "BTC Volatility × Elevated GPR" =
          "BTC-volatility slope difference: elevated vs lower GPR"
      )
    ) %>%
    select(
      Measure,
      Sample,
      Variable,
      Estimate,
      Robust_SE,
      P_Value
    )
}

t29_regime_interactions <- dplyr::bind_rows(
  prepare_j303_interaction_summary(
    e7$interaction.summary,
    "GPRD",
    "Full sample"
  ),
  prepare_j303_interaction_summary(
    e7$act.interaction.summary,
    "GPRD_ACT",
    "Full sample"
  ),
  prepare_j303_interaction_summary(
    e7$threat.interaction.summary,
    "GPRD_THREAT",
    "Full sample"
  ),
  prepare_j303_interaction_summary(
    e7$post2017.act.interaction.summary,
    "GPRD_ACT",
    "Post-2017"
  )
)

write_table(
  t29_regime_interactions,
  "T29_Bitcoin_J303_Regime_Interaction"
)


# Table 5.3: display-ready version of the existing T29 results. This preserves
# the original five-column grouped layout while keeping all estimates linked
# to the upstream interaction-model results. The generic T29 export is unchanged.
format_table53_estimate <- function(x, latex = FALSE) {
  if (is.na(x) || !is.finite(x)) {
    return("-")
  }
  
  # Retain the original table's readable scientific notation for tiny values.
  if (x != 0 && abs(x) < 1e-4) {
    if (latex) {
      exponent <- floor(log10(abs(x)))
      mantissa <- x / (10^exponent)
      return(paste0(
        "$",
        formatC(mantissa, format = "f", digits = 3),
        "\\times10^{", exponent, "}$"
      ))
    }
    return(formatC(x, format = "e", digits = 3))
  }
  
  formatC(x, format = "f", digits = 4)
}

build_table53_display <- function(latex = FALSE) {
  display_base <- t29_regime_interactions %>%
    mutate(
      Display_Group = dplyr::case_when(
        Measure == "GPRD" & Sample == "Full sample" ~ "GPRD, full sample",
        Measure == "GPRD_ACT" & Sample == "Full sample" ~ "ACT, full sample",
        Measure == "GPRD_THREAT" & Sample == "Full sample" ~ "THREAT, full sample",
        Measure == "GPRD_ACT" & Sample == "Post-2017" ~ "ACT, post-2017",
        TRUE ~ NA_character_
      ),
      Display_Term = dplyr::recode(
        Variable,
        "BTC-volatility slope in lower-GPR regime" =
          "Bitcoin slope in lower-GPR regime",
        "Elevated-GPR intercept shift" =
          "Elevated-GPR intercept shift",
        "BTC-volatility slope difference: elevated vs lower GPR" =
          "Slope difference: elevated vs. lower GPR"
      )
    ) %>%
    group_by(Measure, Sample) %>%
    mutate(
      Group_Row = dplyr::row_number(),
      `Measure and sample` = ifelse(
        Group_Row == 1L,
        dplyr::first(Display_Group),
        ""
      )
    ) %>%
    ungroup()
  
  data.frame(
    `Measure and sample` = display_base$`Measure and sample`,
    Term = display_base$Display_Term,
    Estimate = vapply(
      display_base$Estimate,
      format_table53_estimate,
      character(1),
      latex = latex
    ),
    `Newey-West SE` = vapply(
      display_base$Robust_SE,
      function(x) {
        if (is.na(x) || !is.finite(x)) "-" else formatC(x, format = "f", digits = 4)
      },
      character(1)
    ),
    `p-value` = vapply(
      suppressWarnings(as.numeric(display_base$P_Value)),
      format_table41_p,
      character(1),
      latex = latex
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

t29c_table53_display <- build_table53_display(latex = FALSE)
t29c_table53_latex <- build_table53_display(latex = TRUE)

if (
  nrow(t29c_table53_display) != 12L ||
  ncol(t29c_table53_display) != 5L ||
  sum(nzchar(t29c_table53_display[["Measure and sample"]])) != 4L ||
  anyNA(t29c_table53_display$Term)
) {
  stop("Table 5.3 display export failed: expected 12 rows, 5 columns and 4 grouped labels.")
}

write.csv(
  t29c_table53_display,
  file = file.path(core_table_dir, "T29c_Bitcoin_J303_Regime_Interaction_Display.csv"),
  row.names = FALSE,
  na = ""
)

# Add one blank separator row after each three-row group in the LaTeX-only TSV.
# This reproduces the visual grouping of the previous hand-formatted table.
blank_t29_row <- as.data.frame(
  as.list(rep("", ncol(t29c_table53_latex))),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
names(blank_t29_row) <- names(t29c_table53_latex)
t29c_tsv_rows <- list()
for (i in seq_len(nrow(t29c_table53_latex))) {
  t29c_tsv_rows[[length(t29c_tsv_rows) + 1L]] <-
    t29c_table53_latex[i, , drop = FALSE]
  if (i %in% c(3L, 6L, 9L)) {
    t29c_tsv_rows[[length(t29c_tsv_rows) + 1L]] <- blank_t29_row
  }
}
t29c_table53_tsv <- dplyr::bind_rows(t29c_tsv_rows)

write.table(
  t29c_table53_tsv,
  file = file.path(core_table_dir, "T29c_Bitcoin_J303_Regime_Interaction_Display.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE,
  na = ""
)


# Fully interacted dynamic model: BTC volatility at lags 0, 1 and 2 is
# interacted with the GPR regime. This supports the fully interacted equation.

write_table(
  e7$interaction.lag.summary,
  "T29a_Bitcoin_J303_Fully_Interacted_Model",
  appendix = TRUE
)


# Joint full-sample Newey-West tests of whether the three BTC-volatility
# coefficients differ collectively across regimes.

write_table(
  e7$formal.regime.tests.summary,
  "T29b_Bitcoin_J303_Formal_Regime_Difference_Tests"
)


# 16. GPR -> Bitcoin Event Analysis

# Appendix D: event-window descriptive statistics and GPR-Bitcoin regressions
# for the 21-observation event window.

t33_statistics <- e6$event.results_21$statistics %>%
  dplyr::select(
    Event_ID,
    Event_Codes,
    Measure,
    Smoothing,
    Peak_Date,
    Sample_Size,
    Mean_GPR,
    SD_GPR,
    Mean_BTC_Volatility,
    SD_BTC_Volatility
  )

write_table(
  t33_statistics,
  "T33_GPR_Bitcoin_Event_Statistics_21Day",
  appendix = TRUE
)

# T34: restore the full GPR-Bitcoin event-window regression table.
# These are the current Script 6 estimates for the 15 measure-specific
# 21-observation specifications, not the estimates from the 10 October draft.

t34_gpr_btc_event_regression <- e6$event.results_21$regression

if (nrow(t34_gpr_btc_event_regression) != 15L) {
  stop(
    paste0(
      "T34 validation failed: expected 15 GPR-Bitcoin event-window ",
      "regression specifications, but found ",
      nrow(t34_gpr_btc_event_regression),
      "."
    )
  )
}

write_table(
  t34_gpr_btc_event_regression,
  "T34_GPR_Bitcoin_Event_Regression_21Day"
)


# D.9 and D.10: replace the former main-text summary table with the
# correlation and Newey-West significance grids.

t35a <- tibble::rownames_to_column(
  e6$significant.correlation.grid,
  var = "Measure"
)

write_table(
  t35a,
  "T35a_GPR_Bitcoin_Event_Significance_Correlation_Grid"
)


t35b <- tibble::rownames_to_column(
  e6$significant.regression.grid,
  var = "Measure"
)

write_table(
  t35b,
  "T35b_GPR_Bitcoin_Event_Significance_NW_Grid"
)


# 17. Bitcoin -> J303 Event Analysis

# D.10: 21-observation window descriptive statistics remain in the appendix.

t39_statistics <- e8$event.results_21$statistics %>%
  dplyr::select(
    Event_ID,
    Event_Codes,
    Measure,
    Smoothing,
    Peak_Date,
    Sample_Size,
    Mean_BTC_Volatility,
    SD_BTC_Volatility,
    Mean_J303_Volatility,
    SD_J303_Volatility
  )

write_table(
  t39_statistics,
  "T39_Bitcoin_J303_Event_Statistics_21Day",
  appendix = TRUE
)


# Table 5.4: report each distinct 21-observation window once, even when more
# than one GPR measure selected that same window.

deduplicate_event_codes <- function(x) {
  vapply(
    x,
    function(value) {
      if (is.na(value) || !nzchar(trimws(value))) {
        return(value)
      }
      codes <- trimws(unlist(strsplit(value, ";", fixed = TRUE)))
      codes <- codes[nzchar(codes)]
      paste(unique(codes), collapse = "; ")
    },
    character(1),
    USE.NAMES = FALSE
  )
}

t40_distinct_windows <- e8$event.results_21$regression.distinct %>%
  mutate(
    Event_Codes = deduplicate_event_codes(Event_Codes),
    Coefficient_Sign = ifelse(
      BTC_Coefficient < 0,
      "Negative",
      ifelse(BTC_Coefficient > 0, "Positive", "Zero")
    )
  ) %>%
  dplyr::transmute(
    Event_Window,
    Event_ID,
    Event_Codes,
    Measure,
    Smoothing,
    Peak_Date,
    N = Sample_Size,
    Number_of_Specifications,
    Correlation,
    BTC_Coefficient,
    NeweyWest_SE,
    CI_Lower,
    CI_Upper,
    NeweyWest_P_Value,
    Significant,
    Coefficient_Sign,
    Interpretation_Note = ifelse(
      Event_Window == "2014-03-03 to 2014-04-01",
      "This window follows the Mt. Gox collapse; the Bitcoin-volatility spike should not be attributed to Crimea alone.",
      ""
    ),
    Adj_R2,
    Residual_SE
  )

write_table(
  t40_distinct_windows,
  "T40_Bitcoin_J303_Event_Regression_21Day"
)


# Chapter 5 Table 5.4: compact display version of the same 10 distinct windows.
# Keep one row per window, omit the specifications-count column, and separate
# peak date(s) from GPR measure(s). The detailed T40 export retains specification counts.

format_event_window <- function(x) {
  parts <- trimws(strsplit(as.character(x), " to ", fixed = TRUE)[[1]])
  if (length(parts) != 2L) return(as.character(x))
  
  start_date <- as.Date(parts[1])
  end_date <- as.Date(parts[2])
  if (is.na(start_date) || is.na(end_date)) return(as.character(x))
  
  format_day_month <- function(date) {
    paste0(sub("^0", "", format(date, "%d")), " ", format(date, "%b"))
  }
  
  start_label <- format_day_month(start_date)
  end_label <- paste0(format_day_month(end_date), " ", format(end_date, "%Y"))
  
  if (format(start_date, "%Y") != format(end_date, "%Y")) {
    start_label <- paste0(start_label, " ", format(start_date, "%Y"))
  }
  
  paste0(start_label, "-", end_label)
}

format_peak_dates <- function(peak_value) {
  if (is.na(peak_value)) return(NA_character_)
  peaks <- trimws(unlist(strsplit(as.character(peak_value), ";", fixed = TRUE)))
  vapply_one_peak <- function(value) {
    date <- as.Date(value)
    if (is.na(date)) return(value)
    paste0(sub("^0", "", format(date, "%d")), " ", format(date, "%b %Y"))
  }
  paste(vapply(peaks, vapply_one_peak, character(1)), collapse = "; ")
}

format_gpr_measures <- function(measure_value) {
  if (is.na(measure_value)) return(NA_character_)
  measures <- trimws(unlist(strsplit(as.character(measure_value), ";", fixed = TRUE)))
  display_measures <- vapply(measures, function(value) {
    switch(
      value,
      "GPRD" = "GPRD",
      "GPRD_ACT" = "ACT",
      "GPRD_THREAT" = "THREAT",
      value
    )
  }, character(1))
  paste(unique(display_measures), collapse = ", ")
}

t54_display <- t40_distinct_windows %>%
  dplyr::transmute(
    Event_Window = vapply(Event_Window, format_event_window, character(1)),
    Peak_Date = vapply(
      seq_len(nrow(t40_distinct_windows)),
      function(i) format_peak_dates(t40_distinct_windows$Peak_Date[i]),
      character(1)
    ),
    GPR_Measure = vapply(
      seq_len(nrow(t40_distinct_windows)),
      function(i) format_gpr_measures(t40_distinct_windows$Measure[i]),
      character(1)
    ),
    Correlation,
    Bitcoin_Coefficient = BTC_Coefficient,
    NW_SE = NeweyWest_SE,
    NW_P_Value = NeweyWest_P_Value,
    Result = dplyr::case_when(
      NeweyWest_P_Value < 0.05 & BTC_Coefficient < 0 ~ "Negative",
      NeweyWest_P_Value < 0.05 & BTC_Coefficient > 0 ~ "Positive",
      TRUE ~ "Not significant"
    )
  )

if (
  nrow(t54_display) != 10L ||
  any(c("Specifications", "Number_of_Specifications") %in% names(t54_display)) ||
  !all(c("Peak_Date", "GPR_Measure") %in% names(t54_display))
) {
  stop("Table 5.4 display export failed: expected 10 rows, separate peak-date/measure columns, and no specifications column.")
}

write_table(
  t54_display,
  "T40b_Bitcoin_J303_Event_Regression_21Day_Display"
)


# Compact count summary accompanying Table 5.4: 15 specifications, 10 unique
# windows, five significant windows, comprising three negative and two positive.

write_table(
  e8$distinct.window.sign.summary.21,
  "T40a_Bitcoin_J303_Event_Unique_Window_Summary"
)


# Overall comparison across 11-, 21- and 31-observation specifications.
# Distinguish specification-level significance counts from counts of distinct
# windows with at least one significant specification.

count_distinct_significant_windows <- function(x, p_value_col) {
  needed <- c("News_Start_Row", "News_End_Row", p_value_col)
  if (!all(needed %in% names(x))) {
    stop(
      "Cannot count distinct significant windows; missing columns: ",
      paste(setdiff(needed, names(x)), collapse = ", ")
    )
  }
  
  x %>%
    group_by(News_Start_Row, News_End_Row) %>%
    summarise(
      Any_Significant = any(.data[[p_value_col]] < 0.05, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    summarise(Count = sum(Any_Significant)) %>%
    pull(Count)
}

correlations_with_windows <- function(event_result) {
  event_result$correlations %>%
    left_join(
      event_result$events %>%
        select(Event_ID, News_Start_Row, News_End_Row) %>%
        distinct(),
      by = "Event_ID"
    )
}

t41_unique_window_counts <- data.frame(
  Smoothing = c(11, 21, 31),
  Significant_Distinct_Correlation_Windows = c(
    count_distinct_significant_windows(
      correlations_with_windows(e8$event.results_11),
      "Correlation_P_Value"
    ),
    count_distinct_significant_windows(
      correlations_with_windows(e8$event.results_21),
      "Correlation_P_Value"
    ),
    count_distinct_significant_windows(
      correlations_with_windows(e8$event.results_31),
      "Correlation_P_Value"
    )
  ),
  Significant_Distinct_NW_Windows = c(
    count_distinct_significant_windows(
      e8$event.results_11$regression,
      "NeweyWest_P_Value"
    ),
    count_distinct_significant_windows(
      e8$event.results_21$regression,
      "NeweyWest_P_Value"
    ),
    count_distinct_significant_windows(
      e8$event.results_31$regression,
      "NeweyWest_P_Value"
    )
  )
)

t41_window_comparison <- e8$event.window.comparison %>%
  rename(
    Number_of_Specifications = Number_of_Events,
    Significant_Correlation_Specifications = Significant_Correlations,
    Significant_NW_Regression_Specifications = Significant_NW_Regressions
  ) %>%
  left_join(
    t41_unique_window_counts,
    by = "Smoothing"
  )

write_table(
  t41_window_comparison,
  "T41_Bitcoin_J303_Event_Window_Comparison",
  appendix = TRUE
)

if (
  nrow(t41_window_comparison) != 3 ||
  anyNA(t41_window_comparison$Significant_Distinct_Correlation_Windows) ||
  anyNA(t41_window_comparison$Significant_Distinct_NW_Windows)
) {
  stop("T41 validation failed: distinct-window significance counts are incomplete.")
}

# Retain the two specification-level significance grids in the appendix.
# Table 5.4 separately reports distinct event windows, as requested by Henco.
t41a <- tibble::rownames_to_column(
  e8$significant.correlation.grid,
  var = "Measure"
)

write_table(
  t41a,
  "T41a_Bitcoin_J303_Event_Significance_Correlation_Grid",
  appendix = TRUE
)

t41b <- tibble::rownames_to_column(
  e8$significant.regression.grid,
  var = "Measure"
)

write_table(
  t41b,
  "T41b_Bitcoin_J303_Event_Significance_NW_Grid",
  appendix = TRUE
)


# 18. GPR Event Source Audit: Sources sheet (C.1) and 135-row raw peak audit (C.2)

raw_peak_audit <- readxl::read_excel(
  file.path(
    script_dir,
    "GPR_Events.xlsx"
  ),
  sheet = "135 Raw Peak Audit"
)

# Appendix C.2: omit the requested uniqueness and event-description columns.
# The event ID remains so each audit row can be matched to Appendix C.1.
# All 135 audit records and the remaining audit fields are retained.
raw_peak_audit_display <- raw_peak_audit %>%
  select(-Uniqueness, -Underlying_Event) %>%
  rename(
    Start = Event_Start_Date,
    End = Event_End_Date,
    Rank = Raw_Rank,
    Peak_Date = Raw_Peak_Date,
    GPR_Value = Raw_GPR_Value,
    Story_Date = Story_Break_Date,
    Story_Flag = Story_Date_Flag,
    Window_Start = News_Start_Date,
    Window_End = News_End_Date,
    Window_Flag = Regression_Window_Flag
  )

write_table(
  raw_peak_audit_display,
  "T36_GPR_Raw_Peak_Audit",
  appendix = TRUE
)


event_sources <- readxl::read_excel(
  file.path(
    script_dir,
    "GPR_Events.xlsx"
  ),
  sheet = "Sources"
)

# Normalise story and publication dates for display; the workbook itself is
# never modified and the underlying date values are preserved.
source_date_columns <- names(event_sources)[
  grepl(
    "(story.*break.*date|(primary|support).*publication.*date)",
    names(event_sources),
    ignore.case = TRUE
  )
]

format_source_date <- function(x) {
  if (inherits(x, "Date")) {
    return(ifelse(is.na(x), NA_character_, format(x, "%Y-%m-%d")))
  }
  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    date_values <- as.Date(x)
    return(ifelse(is.na(date_values), NA_character_, format(date_values, "%Y-%m-%d")))
  }
  
  date_text <- as.character(x)
  serial_rows <- !is.na(date_text) & grepl("^[0-9]{4,6}(\\.[0-9]+)?$", date_text)
  if (any(serial_rows)) {
    date_text[serial_rows] <- format(
      as.Date(as.numeric(date_text[serial_rows]), origin = "1899-12-30"),
      "%Y-%m-%d"
    )
  }
  date_text
}

for (date_column in source_date_columns) {
  event_sources[[date_column]] <- format_source_date(
    event_sources[[date_column]]
  )
}

# Appendix C.1: keep one row per event and show only the primary source in the
# thesis table. Supporting-source information remains in GPR_Events.xlsx and
# the in-memory source data, but is omitted from this display export as requested.
# The four-column TSV can be wrapped into multiline cells in LaTeX; no hard line
# breaks or LaTeX markup are inserted into the CSV data.
source_detail_cell <- function(publisher, publication_date, headline, url) {
  clean_field <- function(value) {
    if (length(value) == 0L || is.na(value) || !nzchar(trimws(as.character(value)))) {
      return("")
    }
    trimws(as.character(value))
  }
  
  publisher <- clean_field(publisher)
  publication_date <- clean_field(publication_date)
  headline <- clean_field(headline)
  url <- clean_field(url)
  
  header <- if (nzchar(publisher) && nzchar(publication_date)) {
    paste0(publisher, " (", publication_date, ")")
  } else if (nzchar(publisher)) {
    publisher
  } else if (nzchar(publication_date)) {
    publication_date
  } else {
    ""
  }
  
  parts <- character(0)
  if (nzchar(header)) parts <- c(parts, header)
  if (nzchar(headline)) parts <- c(parts, headline)
  if (nzchar(url)) parts <- c(parts, paste0("URL: ", url))
  
  paste(parts, collapse = "; ")
}

event_sources_display <- event_sources %>%
  transmute(
    Source_ID,
    Underlying_Event,
    Story_Break_Date,
    Primary_Source_Details = mapply(
      source_detail_cell,
      Primary_Publisher,
      Primary_Publication_Date,
      Primary_Source_Headline,
      Primary_Source_URL,
      USE.NAMES = FALSE
    )
  )

if (
  nrow(event_sources_display) != nrow(event_sources) ||
  anyDuplicated(event_sources_display$Source_ID) > 0L ||
  ncol(event_sources_display) != 4L
) {
  stop("T36b validation failed: expected one row per event and four display columns, including primary-source details.")
}

write_table(
  event_sources_display,
  "T36b_GPR_Event_Sources",
  appendix = TRUE
)

# Appendix C.1: create a LaTeX-safe TSV with one row per event.
# The CSV produced by write_table() remains plain text. Only the LaTeX-facing
# TSV gets escaped text and \\url{...} links so csvsimple can read all rows safely.
latex_escape_c1 <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("_", " ", x, fixed = TRUE)
  for (special in c("&", "%", "$", "#", "_", "{", "}")) {
    x <- gsub(special, paste0("\\", special), x, fixed = TRUE)
  }
  x
}

format_source_date_c1 <- function(x) {
  if (inherits(x, "Date")) {
    return(ifelse(is.na(x), "", format(x, "%Y-%m-%d")))
  }
  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    d <- as.Date(x)
    return(ifelse(is.na(d), "", format(d, "%Y-%m-%d")))
  }
  out <- as.character(x)
  out[is.na(x)] <- ""
  out
}

source_detail_cell_latex <- function(publisher, publication_date, headline, url) {
  clean_field <- function(value) {
    if (length(value) == 0L || is.na(value) || !nzchar(trimws(as.character(value)))) {
      return("")
    }
    trimws(as.character(value))
  }
  
  publisher <- clean_field(publisher)
  publication_date <- clean_field(publication_date)
  headline <- clean_field(headline)
  url <- clean_field(url)
  
  # Strip only the two known tracking query strings; preserve the source page.
  url <- gsub(
    "?filterKeyEvents=false&page=with%3Ablock-5c8b6921e4b016d23425c518",
    "", url, fixed = TRUE
  )
  url <- gsub(
    "?page=with%3Ablock-585d2f0ee4b06265ff0d6175",
    "", url, fixed = TRUE
  )
  
  header <- if (nzchar(publisher) && nzchar(publication_date)) {
    paste0(publisher, " (", publication_date, ")")
  } else if (nzchar(publisher)) {
    publisher
  } else if (nzchar(publication_date)) {
    publication_date
  } else {
    ""
  }
  
  parts <- character(0)
  if (nzchar(header)) parts <- c(parts, latex_escape_c1(header))
  
  # Keep the printed source register compact: display the headline as a
  # clickable link rather than printing the full URL in every row. The URL
  # remains in the hyperlink target. If a headline is unavailable, show the
  # URL itself as a breakable link instead.
  if (nzchar(headline) && nzchar(url)) {
    parts <- c(
      parts,
      paste0("\\href{", url, "}{", latex_escape_c1(headline), "}")
    )
  } else if (nzchar(headline)) {
    parts <- c(parts, latex_escape_c1(headline))
  } else if (nzchar(url)) {
    parts <- c(parts, paste0("\\url{", url, "}"))
  }
  
  paste(parts, collapse = "; ")
}

source_register_tsv_data <- data.frame(
  `Source ID` = as.character(event_sources_display$Source_ID),
  `Event description` = latex_escape_c1(event_sources_display$Underlying_Event),
  `Story date` = format_source_date_c1(event_sources_display$Story_Break_Date),
  `Primary source` = mapply(
    source_detail_cell_latex,
    event_sources$Primary_Publisher,
    format_source_date_c1(event_sources$Primary_Publication_Date),
    event_sources$Primary_Source_Headline,
    event_sources$Primary_Source_URL,
    USE.NAMES = FALSE
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

source_register_tsv <- file.path(
  appendix_table_dir,
  "T36b_GPR_Event_Sources.tsv"
)
write.table(
  source_register_tsv_data,
  file = source_register_tsv,
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE,
  na = "",
  fileEncoding = "UTF-8"
)

# Verify the actual TSV that LaTeX will read, not just the source data frame.
source_register_tsv_check <- read.delim(
  source_register_tsv,
  sep = "\t",
  header = TRUE,
  check.names = FALSE,
  quote = "",
  colClasses = "character",
  fileEncoding = "UTF-8"
)
if (
  nrow(source_register_tsv_check) != nrow(event_sources) ||
  ncol(source_register_tsv_check) != 4L ||
  anyDuplicated(source_register_tsv_check[["Source ID"]]) > 0L ||
  "Supporting source" %in% names(source_register_tsv_check)
) {
  stop("T36b validation failed: expected one LaTeX-safe row per event and four columns, with no supporting-source display column.")
}


# Event identification robustness: overlap across GPR measures and windows.
# Scripts 6 and 8 use the same event-overlap object, so export it once.

write_table(
  e6$event.identification.robustness,
  "T36c_Event_Identification_Robustness",
  appendix = TRUE
)


# The separate episode-to-source-code map is omitted; the raw peak audit
# already contains the source codes, making that additional table redundant.


# Retain the previously exported event-method notes; Henco did not ask for this
# supporting methods summary to be removed.
event_method_notes <- data.frame(
  Item = c(
    "Event identification",
    "Smoothed event period",
    "Raw peak selection",
    "Episode source-code assignment",
    "Event analysis windows",
    "Overlapping 31-day windows",
    "11-day Newey-West inference"
  ),
  Description = c(
    "For each GPR measure and smoothing window, candidate peaks are identified and the five peaks with the greatest prominence are selected.",
    "Each smoothed event period is defined from the preceding trough to the following trough. This period is distinct from the fixed peak-centred regression window.",
    "The three highest observations from the unsmoothed GPR measure are selected within each event's fixed peak-centred regression window (News_Start_Row to News_End_Row).",
    "The source codes assigned to an episode describe the underlying stories associated with its own three selected raw peak days. Stories from other episodes or GPR measures are not added to that episode's codes.",
    "Event regressions use fixed peak-centred windows containing exactly 11, 21, or 31 daily observations, matching the smoothing-window length.",
    "The two 31-day GPRD windows centred on the May and June 2021 peaks overlap substantially (about two and a half weeks) and share top raw-peak days. Their event identifications are therefore not independent.",
    "With only 11 observations, Newey-West inference is treated as unreliable and should be interpreted descriptively rather than as strong inferential evidence."
  ),
  stringsAsFactors = FALSE
)

write_table(
  event_method_notes,
  "T36a_Event_Method_Notes",
  appendix = TRUE
)


# 20. Prepare Figure Data

plot_data <- data.frame(
  Date = data0$Date,
  Bitcoin = data0$BTC_log_returns,
  J303 = data0$Index_log_returns,
  GPRD = data0$GPRD,
  GPRD_ACT = data0$GPRD_ACT,
  GPRD_THREAT = data0$GPRD_THREAT,
  BTC_Volatility = data0$BTC_Volatility,
  J303_Volatility = data0$J303_Volatility
)


# 21. Create Figures

# Combine paired return series into a two-panel figure.

return_plot_data <- dplyr::bind_rows(
  data.frame(
    Date = plot_data$Date,
    Series = "Bitcoin",
    Log_Return = plot_data$Bitcoin
  ),
  data.frame(
    Date = plot_data$Date,
    Series = "J303",
    Log_Return = plot_data$J303
  )
)

p_returns <- ggplot(
  return_plot_data,
  aes(Date, Log_Return)
) +
  geom_line(
    linewidth = 0.35,
    colour = "black"
  ) +
  facet_wrap(
    ~Series,
    ncol = 1,
    scales = "free_y"
  ) +
  labs(
    title = "Daily Log Returns",
    x = "Date",
    y = "Log return"
  ) +
  theme_thesis


# Combine GPRD, ACT and THREAT into aligned panels.

gpr_plot_data <- dplyr::bind_rows(
  data.frame(
    Date = plot_data$Date,
    Measure = "GPRD",
    Value = plot_data$GPRD
  ),
  data.frame(
    Date = plot_data$Date,
    Measure = "GPRD_ACT",
    Value = plot_data$GPRD_ACT
  ),
  data.frame(
    Date = plot_data$Date,
    Measure = "GPRD_THREAT",
    Value = plot_data$GPRD_THREAT
  )
)

p_gpr_series <- ggplot(
  gpr_plot_data,
  aes(Date, Value)
) +
  geom_line(
    linewidth = 0.35,
    colour = "black"
  ) +
  facet_wrap(
    ~Measure,
    ncol = 1,
    scales = "free_y"
  ) +
  labs(
    title = "Geopolitical Risk Measures",
    x = "Date",
    y = "Index value"
  ) +
  theme_thesis


# Combine the two conditional volatility series into a single two-panel figure.

volatility_plot_data <- dplyr::bind_rows(
  data.frame(
    Date = plot_data$Date,
    Series = "Bitcoin (ZAR returns)",
    Conditional_Volatility = plot_data$BTC_Volatility
  ),
  data.frame(
    Date = plot_data$Date,
    Series = "J303",
    Conditional_Volatility = plot_data$J303_Volatility
  )
)

p_conditional_volatility <- ggplot(
  volatility_plot_data,
  aes(Date, Conditional_Volatility)
) +
  geom_line(
    linewidth = 0.35,
    colour = "black"
  ) +
  facet_wrap(
    ~Series,
    ncol = 1,
    scales = "free_y"
  ) +
  labs(
    title = "Estimated Conditional Volatility",
    x = "Date",
    y = "Conditional volatility"
  ) +
  theme_thesis


p_gpr_btc <- ggplot(
  data0,
  aes(
    GPRD,
    BTC_Volatility
  )
) +
  geom_point(
    alpha = 0.45,
    shape = 16,
    colour = "black"
  ) +
  geom_smooth(
    aes(
      colour = "Linear fit",
      linetype = "Linear fit"
    ),
    method = "lm",
    se = TRUE
  ) +
  geom_smooth(
    aes(
      colour = "LOESS smoother",
      linetype = "LOESS smoother"
    ),
    method = "loess",
    se = TRUE
  ) +
  scale_colour_manual(
    name = "Fitted line",
    values = c(
      "Linear fit" = "black",
      "LOESS smoother" = "grey40"
    )
  ) +
  scale_linetype_manual(
    name = "Fitted line",
    values = c(
      "Linear fit" = "dashed",
      "LOESS smoother" = "solid"
    )
  ) +
  labs(
    title = "Bitcoin Conditional Volatility and Geopolitical Risk",
    x = "Geopolitical Risk Index",
    y = "Bitcoin conditional volatility",
    colour = "Fitted line",
    linetype = "Fitted line"
  ) +
  theme_thesis


p_btc_j303 <- ggplot(
  data0,
  aes(
    BTC_Volatility,
    J303_Volatility
  )
) +
  geom_point(
    alpha = 0.45,
    shape = 16,
    colour = "black"
  ) +
  geom_smooth(
    aes(
      colour = "Linear fit",
      linetype = "Linear fit"
    ),
    method = "lm",
    se = TRUE
  ) +
  geom_smooth(
    aes(
      colour = "LOESS smoother",
      linetype = "LOESS smoother"
    ),
    method = "loess",
    se = TRUE
  ) +
  scale_colour_manual(
    name = "Fitted line",
    values = c(
      "Linear fit" = "black",
      "LOESS smoother" = "grey40"
    )
  ) +
  scale_linetype_manual(
    name = "Fitted line",
    values = c(
      "Linear fit" = "dashed",
      "LOESS smoother" = "solid"
    )
  ) +
  labs(
    title = "Bitcoin and J303 Conditional Volatility",
    x = "Bitcoin conditional volatility",
    y = "J303 conditional volatility",
    colour = "Fitted line",
    linetype = "Fitted line"
  ) +
  theme_thesis


p_regime <- ggplot(
  data0,
  aes(
    GPRD,
    BTC_Volatility,
    shape = GPR_Regime
  )
) +
  geom_point(
    alpha = 0.45,
    colour = "black"
  ) +
  geom_smooth(
    aes(
      linetype = GPR_Regime
    ),
    method = "lm",
    se = TRUE,
    colour = "black"
  ) +
  scale_shape_manual(
    values = c(
      16,
      1
    )
  ) +
  scale_linetype_manual(
    values = c(
      "solid",
      "dashed"
    )
  ) +
  labs(
    title = "Bitcoin Volatility and Geopolitical Risk by Regime",
    x = "Geopolitical Risk Index",
    y = "Bitcoin conditional volatility",
    shape = "GPR regime",
    linetype = "GPR regime"
  ) +
  theme_thesis


# Figure 4.1: plot the 21-observation GPR coefficients as dot-and-interval
# estimates, scaled to percentage-point effects per one-SD GPR increase.

event21_gpr <- e6$event.results_21$regression %>%
  mutate(
    GPR_SD = vapply(
      as.character(Measure),
      function(measure_name) {
        sd(data0[[measure_name]], na.rm = TRUE)
      },
      numeric(1)
    ),
    GPR_Coefficient_pp_per_SD = 100 * GPR_Coefficient * GPR_SD,
    CI_Lower_pp_per_SD = 100 * CI_Lower * GPR_SD,
    CI_Upper_pp_per_SD = 100 * CI_Upper * GPR_SD,
    Peak_Label = factor(
      as.character(Peak_Date),
      levels = rev(sort(unique(as.character(Peak_Date))))
    )
  )

p_event_gpr_btc <- ggplot(
  event21_gpr,
  aes(
    x = GPR_Coefficient_pp_per_SD,
    y = Peak_Label
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey40"
  ) +
  geom_segment(
    aes(
      x = CI_Lower_pp_per_SD,
      xend = CI_Upper_pp_per_SD,
      y = Peak_Label,
      yend = Peak_Label
    ),
    linewidth = 0.45,
    colour = "black"
  ) +
  geom_point(
    size = 2,
    colour = "black"
  ) +
  facet_wrap(
    ~Measure,
    ncol = 1,
    scales = "free_y"
  ) +
  labs(
    title = "GPR–Bitcoin Volatility Effects During Identified Events",
    x = "Effect on daily Bitcoin volatility (percentage points per 1-SD GPR increase)",
    y = "Peak date"
  ) +
  theme_thesis


# Figure 5.1: dot plot of each 21-observation Bitcoin-J303 coefficient with
# its Newey-West 95% confidence interval. Faceting prevents duplicate windows
# selected by different GPR measures from being stacked or summed.

event21_spill <- e8$event.results_21$regression %>%
  mutate(
    Peak_Label = factor(
      as.character(Peak_Date),
      levels = rev(sort(unique(as.character(Peak_Date))))
    )
  )

p_event_spill <- ggplot(
  event21_spill,
  aes(
    x = BTC_Coefficient,
    y = Peak_Label
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey40"
  ) +
  geom_segment(
    aes(
      x = CI_Lower,
      xend = CI_Upper,
      y = Peak_Label,
      yend = Peak_Label
    ),
    linewidth = 0.45,
    colour = "black"
  ) +
  geom_point(
    size = 2,
    colour = "black"
  ) +
  facet_wrap(
    ~Measure,
    ncol = 1,
    scales = "free_y"
  ) +
  labs(
    title = "Bitcoin–J303 Volatility Relationship During Identified Events",
    x = "Bitcoin-volatility coefficient with 95% Newey-West interval",
    y = "Peak date"
  ) +
  theme_thesis


# Combine the standardized residual time series into two aligned panels.

btc_std_resid <- data.frame(
  Date = data0$Date,
  Asset = "Bitcoin",
  Standardized_Residual = as.numeric(
    residuals(
      master$btc.fit,
      standardize = TRUE
    )
  )
)

j303_std_resid <- data.frame(
  Date = data0$Date,
  Asset = "J303",
  Standardized_Residual = as.numeric(
    residuals(
      master$j303.fit,
      standardize = TRUE
    )
  )
)

standardized_residuals <- dplyr::bind_rows(
  btc_std_resid,
  j303_std_resid
)

p_standardized_residuals <- ggplot(
  standardized_residuals,
  aes(
    Date,
    Standardized_Residual
  )
) +
  geom_line(
    linewidth = 0.35,
    colour = "black"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey40"
  ) +
  facet_wrap(
    ~Asset,
    ncol = 1,
    scales = "free_y"
  ) +
  labs(
    title = "Standardized EGARCH Residuals",
    x = "Date",
    y = "Standardized residual"
  ) +
  theme_thesis


# 22. Save Figures

save_plot(
  p_returns,
  "F01_Returns_Panel",
  width = 9,
  height = 7
)

save_plot(
  p_gpr_series,
  "F02_GPR_Measures_Panel",
  width = 9,
  height = 8
)

save_plot(
  p_conditional_volatility,
  "F04_Conditional_Volatility_Series",
  width = 9,
  height = 7
)

save_plot(
  p_gpr_btc,
  "F05_GPR_Bitcoin_Volatility"
)

save_plot(
  p_btc_j303,
  "F06_Bitcoin_J303_Volatility"
)

save_plot(
  p_regime,
  "F07_GPR_Regimes"
)

save_plot(
  p_event_gpr_btc,
  "F08_GPR_Bitcoin_Event_Effects_21Day",
  width = 10,
  height = 9
)

save_plot(
  p_event_spill,
  "F09_Bitcoin_J303_Event_Effects_21Day",
  width = 10,
  height = 9
)

save_plot(
  p_standardized_residuals,
  "F10_Standardized_Residuals_Panel",
  width = 9,
  height = 7
)


# 23. Validate Key Requested Outputs

# Validate post-2017 sample timing and that the updated event-window structure
# still matches Henco's 21-observation result.

if (
  min(e4$post2017.data$Date, na.rm = TRUE) <
  as.Date("2018-01-01")
) {
  stop("Post-2017 validation failed: the robustness sample starts before 1 January 2018.")
}

if (nrow(t10_gpr_main) != 10) {
  stop("T10 validation failed: expected five full-sample and five post-2017 GPR models.")
}

if (
  nrow(t11_component_dynamic) != 5 ||
  sum(t11_component_dynamic$Sample == "Post-2017") != 2 ||
  !all(c("GPRD_ACT", "GPRD_THREAT") %in% t11_component_dynamic$Measure[
    t11_component_dynamic$Sample == "Post-2017"
  ])
) {
  stop("T11 validation failed: expected full-sample GPRD, ACT and THREAT rows plus post-2017 ACT and THREAT rows.")
}

if (
  nrow(t22_gpr_regimes) != 6 ||
  anyNA(t22_gpr_regimes$BTC_Vol_Lag1_NW_p) ||
  anyNA(t22_gpr_regimes$BTC_Vol_Lag2_NW_p)
) {
  stop("T22 validation failed: BTC volatility lag-one or lag-two Newey-West p-values are missing.")
}

if (
  nrow(e7$formal.regime.tests.summary) != 4 ||
  anyNA(e7$formal.regime.tests.summary$P_Value)
) {
  stop("T29b validation failed: the four formal regime-difference tests are incomplete.")
}

if (nrow(t29_regime_interactions) != 12) {
  stop("T29 validation failed: expected 12 rows covering GPRD, ACT, THREAT and post-2017 ACT interactions.")
}

if (
  nrow(e8$event.results_21$regression.distinct) != 10 ||
  nrow(e8$distinct.window.sign.summary.21) != 1
) {
  stop("Table 5.4 validation failed: expected 10 distinct 21-observation windows and one summary row.")
}

if (
  e8$distinct.window.sign.summary.21$Specifications[1] != 15 ||
  e8$distinct.window.sign.summary.21$Distinct_Windows[1] != 10 ||
  e8$distinct.window.sign.summary.21$Significant_Distinct_Windows[1] != 5 ||
  e8$distinct.window.sign.summary.21$Significant_Negative_Windows[1] != 3 ||
  e8$distinct.window.sign.summary.21$Significant_Positive_Windows[1] != 2
) {
  stop("Table 5.4 validation failed: the 21-day distinct-window counts do not match the reviewed results.")
}


check_grid <- function(x, table_name) {
  required_columns <- c(
    "Measure",
    "Smoothing_11",
    "Smoothing_21",
    "Smoothing_31"
  )
  
  if (
    nrow(x) != 3 ||
    !all(required_columns %in% names(x)) ||
    !all(c("GPRD", "GPRD_ACT", "GPRD_THREAT") %in% x$Measure)
  ) {
    stop(paste0(table_name, " validation failed: grid structure or measure labels are incorrect."))
  }
}

check_grid(t35a, "T35a")
check_grid(t35b, "T35b")
check_grid(t41a, "T41a")
check_grid(t41b, "T41b")

if (nrow(e6$event.identification.robustness) == 0) {
  stop("T36c validation failed: event-identification robustness table is empty.")
}

if (
  nrow(raw_peak_audit) != 135 ||
  nrow(raw_peak_audit_display) != 135 ||
  "Uniqueness" %in% names(raw_peak_audit_display) ||
  "Event_Description" %in% names(raw_peak_audit_display) ||
  "Underlying_Event" %in% names(raw_peak_audit_display)
) {
  stop("Raw peak audit validation failed: expected 135 rows and no uniqueness or event-description column in the exported display table.")
}


# Validate a compact set of important CSV/TSV exports.

key_export_names <- c(
  "T09_GPR_Bitcoin_Correlation",
  "T11_GPR_Component_Dynamic_Regression",
  "T11_GPR_Component_Regression",
  "T13_Bitcoin_J303_Correlation",
  "T19_Bitcoin_J303_Dynamic_Serial_Correlation",
  "T10_GPR_Bitcoin_Main_Models",
  "T10a_GPR_Bitcoin_Table_41_Display",
  "T04_EGARCH_Parameter_Estimates_Display",
  "T14_Bitcoin_J303_Spillover_Models",
  "T15_Granger_Causality",
  "T14a_Bitcoin_J303_Table_51_Display",
  "T14c_Bitcoin_J303_Table_51_Coefficients_Display",
  "T14d_Bitcoin_J303_Table_51_Model_Fit_Display",
  "T14b_Bitcoin_J303_Table_52_Display",
  "T22a_GPR_Regime_Dynamic_Coefficients_Display",
  "T22b_GPR_Regime_Dynamic_Model_Fit_Display",
  "T28a_Bitcoin_J303_Regime_Coefficients_Display",
  "T28b_Bitcoin_J303_Regime_Model_Fit_Display",
  "T29_Bitcoin_J303_Regime_Interaction",
  "T29c_Bitcoin_J303_Regime_Interaction_Display",
  "T29b_Bitcoin_J303_Formal_Regime_Difference_Tests",
  "T35a_GPR_Bitcoin_Event_Significance_Correlation_Grid",
  "T35b_GPR_Bitcoin_Event_Significance_NW_Grid",
  "T40_Bitcoin_J303_Event_Regression_21Day",
  "T40b_Bitcoin_J303_Event_Regression_21Day_Display",
  "T40a_Bitcoin_J303_Event_Unique_Window_Summary",
  "T36_GPR_Raw_Peak_Audit",
  "T36a_Event_Method_Notes",
  "T36b_GPR_Event_Sources",
  "T36c_Event_Identification_Robustness",
  "T41a_Bitcoin_J303_Event_Significance_Correlation_Grid",
  "T41b_Bitcoin_J303_Event_Significance_NW_Grid"
)

key_export_dirs <- c(
  appendix_table_dir, # T09_GPR_Bitcoin_Correlation
  core_table_dir,     # T11_GPR_Component_Dynamic_Regression
  appendix_table_dir, # T11_GPR_Component_Regression
  appendix_table_dir, # T13_Bitcoin_J303_Correlation
  appendix_table_dir, # T19_Bitcoin_J303_Dynamic_Serial_Correlation
  core_table_dir,     # T10_GPR_Bitcoin_Main_Models
  core_table_dir,     # T10a_GPR_Bitcoin_Table_41_Display
  core_table_dir,     # T04_EGARCH_Parameter_Estimates_Display
  core_table_dir,     # T14_Bitcoin_J303_Spillover_Models
  core_table_dir,     # T15_Granger_Causality
  core_table_dir,     # T14a_Bitcoin_J303_Table_51_Display
  core_table_dir,     # T14c_Bitcoin_J303_Table_51_Coefficients_Display
  core_table_dir,     # T14d_Bitcoin_J303_Table_51_Model_Fit_Display
  core_table_dir,     # T14b_Bitcoin_J303_Table_52_Display,
  appendix_table_dir, # T22a_GPR_Regime_Dynamic_Coefficients_Display
  appendix_table_dir, # T22b_GPR_Regime_Dynamic_Model_Fit_Display
  appendix_table_dir, # T28a_Bitcoin_J303_Regime_Coefficients_Display
  appendix_table_dir, # T28b_Bitcoin_J303_Regime_Model_Fit_Display
  core_table_dir,     # T29_Bitcoin_J303_Regime_Interaction
  core_table_dir,     # T29c_Bitcoin_J303_Regime_Interaction_Display
  core_table_dir,     # T29b_Bitcoin_J303_Formal_Regime_Difference_Tests
  core_table_dir,     # T35a_GPR_Bitcoin_Event_Significance_Correlation_Grid
  core_table_dir,     # T35b_GPR_Bitcoin_Event_Significance_NW_Grid
  core_table_dir,     # T40_Bitcoin_J303_Event_Regression_21Day
  core_table_dir,     # T40b_Bitcoin_J303_Event_Regression_21Day_Display
  core_table_dir,     # T40a_Bitcoin_J303_Event_Unique_Window_Summary
  appendix_table_dir, # T36_GPR_Raw_Peak_Audit
  appendix_table_dir, # T36a_Event_Method_Notes
  appendix_table_dir, # T36b_GPR_Event_Sources
  appendix_table_dir, # T36c_Event_Identification_Robustness
  appendix_table_dir, # T41a_Bitcoin_J303_Event_Significance_Correlation_Grid
  appendix_table_dir  # T41b_Bitcoin_J303_Event_Significance_NW_Grid
)

key_export_rows <- c(
  nrow(e3$correlation.summary),
  nrow(t11_component_dynamic),
  nrow(e3$gpr.component.summary),
  nrow(e4$correlation.summary),
  nrow(e4$dynamic.serial.summary),
  nrow(t10_gpr_main),
  nrow(t10a_table41_display),
  nrow(t04_parameter_display),
  nrow(t14_spillover),
  nrow(t15_granger),
  nrow(t14a_table51_display),
  nrow(t51a_coefficients_csv),
  nrow(t51b_model_fit_csv),
  nrow(t14b_table52_display),
  nrow(t22b5_coefficients_csv),
  nrow(t22b5_model_fit_csv),
  nrow(t28b6_coefficients_csv),
  nrow(t28b6_model_fit_csv),
  nrow(t29_regime_interactions),
  nrow(t29c_table53_display),
  nrow(e7$formal.regime.tests.summary),
  nrow(t35a),
  nrow(t35b),
  nrow(t40_distinct_windows),
  nrow(t54_display),
  nrow(e8$distinct.window.sign.summary.21),
  nrow(raw_peak_audit),
  nrow(event_method_notes),
  nrow(event_sources_display),
  nrow(e6$event.identification.robustness),
  nrow(t41a),
  nrow(t41b)
)

key_export_check <- data.frame(
  Table = key_export_names,
  Rows = key_export_rows,
  CSV_Exists = file.exists(
    file.path(key_export_dirs, paste0(key_export_names, ".csv"))
  ),
  TSV_Exists = file.exists(
    file.path(key_export_dirs, paste0(key_export_names, ".tsv"))
  )
)

cat("\nKey export validation:\n")
print(key_export_check, row.names = FALSE)

if (!all(key_export_check$CSV_Exists) || !all(key_export_check$TSV_Exists)) {
  stop("One or more required tables were not exported in both CSV and TSV formats.")
}


# Guard against a script run leaving any of the appendix tables that are used
# directly in LaTeX as header-only or empty files.
appendix_exports_to_check <- c(
  "T11_GPR_Component_Regression",
  "T11_GPR_Component_Dynamic_Regression",
  "T20_GPR_Thresholds",
  "T21_GPR_Regime_Descriptives",
  "T22_GPR_Regime_Dynamic_Models",
  "T22a_GPR_Regime_Dynamic_Coefficients_Display",
  "T22b_GPR_Regime_Dynamic_Model_Fit_Display",
  "T28_Bitcoin_J303_Regime_Spillover",
  "T28a_Bitcoin_J303_Regime_Coefficients_Display",
  "T28b_Bitcoin_J303_Regime_Model_Fit_Display",
  "T29a_Bitcoin_J303_Fully_Interacted_Model",
  "T33_GPR_Bitcoin_Event_Statistics_21Day",
  "T36_GPR_Raw_Peak_Audit",
  "T36a_Event_Method_Notes",
  "T36b_GPR_Event_Sources",
  "T36c_Event_Identification_Robustness",
  "T39_Bitcoin_J303_Event_Statistics_21Day",
  "T41a_Bitcoin_J303_Event_Significance_Correlation_Grid",
  "T41b_Bitcoin_J303_Event_Significance_NW_Grid"
)
appendix_exports_to_check_dirs <- c(
  appendix_table_dir, # T11_GPR_Component_Regression
  core_table_dir,     # T11_GPR_Component_Dynamic_Regression
  appendix_table_dir, # T20_GPR_Thresholds
  appendix_table_dir, # T21_GPR_Regime_Descriptives
  appendix_table_dir, # T22_GPR_Regime_Dynamic_Models
  appendix_table_dir, # T22a_GPR_Regime_Dynamic_Coefficients_Display
  appendix_table_dir, # T22b_GPR_Regime_Dynamic_Model_Fit_Display
  appendix_table_dir, # T28_Bitcoin_J303_Regime_Spillover
  appendix_table_dir, # T28a_Bitcoin_J303_Regime_Coefficients_Display
  appendix_table_dir, # T28b_Bitcoin_J303_Regime_Model_Fit_Display
  appendix_table_dir, # T29a_Bitcoin_J303_Fully_Interacted_Model
  appendix_table_dir, # T33_GPR_Bitcoin_Event_Statistics_21Day
  appendix_table_dir, # T36_GPR_Raw_Peak_Audit
  appendix_table_dir, # T36a_Event_Method_Notes
  appendix_table_dir, # T36b_GPR_Event_Sources
  appendix_table_dir, # T36c_Event_Identification_Robustness
  appendix_table_dir, # T39_Bitcoin_J303_Event_Statistics_21Day
  appendix_table_dir, # T41a_Bitcoin_J303_Event_Significance_Correlation_Grid
  appendix_table_dir  # T41b_Bitcoin_J303_Event_Significance_NW_Grid
)

for (i in seq_along(appendix_exports_to_check)) {
  path <- file.path(
    appendix_exports_to_check_dirs[[i]],
    paste0(appendix_exports_to_check[[i]], ".tsv")
  )
  if (!file.exists(path)) {
    stop("Appendix export validation failed: missing ", basename(path))
  }
  exported <- tryCatch(
    read.delim(
      path,
      sep = "\t",
      header = TRUE,
      check.names = FALSE,
      quote = "",
      colClasses = "character",
      fileEncoding = "UTF-8"
    ),
    error = function(e) NULL
  )
  if (is.null(exported) || nrow(exported) < 1L || ncol(exported) < 1L) {
    stop("Appendix export validation failed: ", basename(path), " has no readable data rows.")
  }
}

# Schema checks for tables whose structures must remain compatible with the
# existing LaTeX calls. These validate the final files without changing values.
expected_table_shapes <- data.frame(
  Table = c(
    "T11_GPR_Component_Dynamic_Regression",
    "T20_GPR_Thresholds",
    "T21_GPR_Regime_Descriptives",
    "T22_GPR_Regime_Dynamic_Models",
    "T28_Bitcoin_J303_Regime_Spillover",
    "T22a_GPR_Regime_Dynamic_Coefficients_Display",
    "T22b_GPR_Regime_Dynamic_Model_Fit_Display",
    "T28a_Bitcoin_J303_Regime_Coefficients_Display",
    "T28b_Bitcoin_J303_Regime_Model_Fit_Display"
  ),
  Folder = c(
    core_table_dir,
    appendix_table_dir,
    appendix_table_dir,
    appendix_table_dir,
    appendix_table_dir,
    appendix_table_dir,
    appendix_table_dir,
    appendix_table_dir,
    appendix_table_dir
  ),
  Expected_Rows = c(5L, 3L, 2L, 6L, 6L, 6L, 6L, 6L, 6L),
  Expected_Columns = c(13L, 2L, 4L, 20L, 20L, 8L, 9L, 8L, 9L),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(expected_table_shapes))) {
  path <- file.path(
    expected_table_shapes$Folder[[i]],
    paste0(expected_table_shapes$Table[[i]], ".tsv")
  )
  exported <- read.delim(
    path,
    sep = "\t",
    header = TRUE,
    check.names = FALSE,
    quote = "",
    colClasses = "character",
    fileEncoding = "UTF-8"
  )
  if (
    nrow(exported) != expected_table_shapes$Expected_Rows[[i]] ||
    ncol(exported) != expected_table_shapes$Expected_Columns[[i]]
  ) {
    stop(
      "Appendix table structure validation failed for ",
      basename(path), ": expected ",
      expected_table_shapes$Expected_Rows[[i]], " rows x ",
      expected_table_shapes$Expected_Columns[[i]], " columns; found ",
      nrow(exported), " rows x ", ncol(exported), " columns."
    )
  }
}


# 24. Results Summary

cat(
  "\nResults written to:\n",
  output_dir,
  "\n\n"
)

cat(
  "Core tables:",
  length(
    list.files(
      core_table_dir,
      pattern = "\\.csv$"
    )
  ),
  "\n"
)

cat(
  "Appendix tables:",
  length(
    list.files(
      appendix_table_dir,
      pattern = "\\.csv$"
    )
  ),
  "\n"
)

cat(
  "Figures:",
  length(
    list.files(
      figure_dir,
      pattern = "\\.png$"
    )
  ),
  "\n"
)





