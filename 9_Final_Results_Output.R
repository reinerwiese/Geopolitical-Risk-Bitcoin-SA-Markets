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
  "T20_GPR_Thresholds",
  "T21_GPR_Regime_Descriptives",
  "T22_GPR_Regime_Dynamic_Models",
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
  "T34_GPR_Bitcoin_Event_Regression_21Day",
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


write_table <- function(x, name, digits = 4, appendix = FALSE) {
  
  x <- as.data.frame(x)

  p_value_cols <- names(x)[
    grepl(
      "(^p$|p.?value|p.?val|(^|_)p($|_))",
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
  

  for (nm in intersect(p_value_cols, original_names)) {
    if (is.character(x_tsv[[nm]])) {
      p_text <- trimws(x_tsv[[nm]])
      needs_math <- !is.na(p_text) &
        grepl("^[<>]", p_text) &
        !grepl("^\\$.*\\$$", p_text)
      x_tsv[[nm]][needs_math] <- paste0("$", p_text[needs_math], "$")
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
# Kept as a separate export so it can be placed directly beneath/alongside
# the descriptive statistics in Chapter 3, as requested by Henco.

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

write_table(
  preliminary_tests,
  "T02a_Preliminary_Tests"
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
  
  convergence_code <- tryCatch(
    as.integer(fit@fit$convergence),
    error = function(e) NA_integer_
  )
  
  convergence_status <- if (is.na(convergence_code)) {
    "Unavailable"
  } else if (convergence_code == 0L) {
    "Successful convergence"
  } else {
    paste0("Review convergence code ", convergence_code)
  }
  
  data.frame(
    Asset = asset,
    Model = model_name,
    Convergence_Code = convergence_code,
    Convergence_Status = convergence_status,
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
  "T05_Final_GARCH_Diagnostics"
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

write_table(
  t11_component_dynamic,
  "T11_GPR_Component_Dynamic_Regression"
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

write_table(
  t14_spillover,
  "T14_Bitcoin_J303_Spillover_Models"
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
  "T15_Granger_Causality"
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
  )

write_table(
  t22_gpr_regimes,
  "T22_GPR_Regime_Dynamic_Models",
  appendix = TRUE
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
write_table(
  t28_regime_comparison,
  "T28_Bitcoin_J303_Regime_Spillover",
  appendix = TRUE
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

# D.3: event-window descriptive statistics for the 21-observation window.
# Regression coefficients are used for the coefficient plot below, but the
# separate D.4 regression table is omitted as requested by Henco.

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

write_table(
  raw_peak_audit,
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

# readxl can import Excel serials in the supporting-publication-date column as
# numbers. Convert only that source-date field for display; the workbook itself
# is never modified and the underlying date values are preserved.
supporting_date_columns <- names(event_sources)[
  grepl("support.*publication.*date", names(event_sources), ignore.case = TRUE)
]

format_supporting_publication_date <- function(x) {
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

for (date_column in supporting_date_columns) {
  event_sources[[date_column]] <- format_supporting_publication_date(
    event_sources[[date_column]]
  )
}

write_table(
  event_sources,
  "T36b_GPR_Event_Sources",
  appendix = TRUE
)


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

if (nrow(raw_peak_audit) != 135) {
  stop("Raw peak audit validation failed: expected 135 audited raw-peak rows.")
}


# Validate a compact set of important CSV/TSV exports.

key_export_names <- c(
  "T09_GPR_Bitcoin_Correlation",
  "T11_GPR_Component_Dynamic_Regression",
  "T11_GPR_Component_Regression",
  "T13_Bitcoin_J303_Correlation",
  "T19_Bitcoin_J303_Dynamic_Serial_Correlation",
  "T10_GPR_Bitcoin_Main_Models",
  "T14_Bitcoin_J303_Spillover_Models",
  "T15_Granger_Causality",
  "T29_Bitcoin_J303_Regime_Interaction",
  "T29b_Bitcoin_J303_Formal_Regime_Difference_Tests",
  "T35a_GPR_Bitcoin_Event_Significance_Correlation_Grid",
  "T35b_GPR_Bitcoin_Event_Significance_NW_Grid",
  "T40_Bitcoin_J303_Event_Regression_21Day",
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
  core_table_dir,     # T14_Bitcoin_J303_Spillover_Models
  core_table_dir,     # T15_Granger_Causality
  core_table_dir,     # T29_Bitcoin_J303_Regime_Interaction
  core_table_dir,     # T29b_Bitcoin_J303_Formal_Regime_Difference_Tests
  core_table_dir,     # T35a_GPR_Bitcoin_Event_Significance_Correlation_Grid
  core_table_dir,     # T35b_GPR_Bitcoin_Event_Significance_NW_Grid
  core_table_dir,     # T40_Bitcoin_J303_Event_Regression_21Day
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
  nrow(t14_spillover),
  nrow(t15_granger),
  nrow(t29_regime_interactions),
  nrow(e7$formal.regime.tests.summary),
  nrow(t35a),
  nrow(t35b),
  nrow(t40_distinct_windows),
  nrow(e8$distinct.window.sign.summary.21),
  nrow(raw_peak_audit),
  nrow(event_method_notes),
  nrow(event_sources),
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