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


write_table <- function(x, name, digits = 4, appendix = FALSE) {
  
  x <- as.data.frame(x)
  
  x[] <- lapply(
    x,
    round_numeric,
    digits = digits
  )
  
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

data_audit <- data.frame(
  Item = c(
    "Observations",
    "Complete observations",
    "Missing observations",
    "Start date",
    "End date",
    "Unique dates",
    "Duplicated dates"
  ),
  Value = c(
    nrow(data0),
    sum(complete.cases(data0)),
    sum(!complete.cases(data0)),
    as.character(min(data0$Date)),
    as.character(max(data0$Date)),
    length(unique(data0$Date)),
    anyDuplicated(data0$Date)
  )
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


# 8. GARCH Model Selection

write_table(
  e21$comparison,
  "T03_Bitcoin_GARCH_Comparison"
)

write_table(
  e21$mean_comparison,
  "T04_Bitcoin_EGARCH_Mean_Comparison"
)

write_table(
  e22$comparison,
  "T05_J303_GARCH_Comparison"
)

write_table(
  e22$arma.comparison,
  "T06_J303_EGARCH_Mean_Comparison"
)


# 9. Final GARCH Diagnostics

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
    as.numeric(
      persistence(fit)[1]
    ),
    error = function(e) {
      NA_real_
    }
  )
  
  ny_joint <- tryCatch(
    as.numeric(
      nyblom(fit)$JointStat
    ),
    error = function(e) {
      NA_real_
    }
  )
  
  data.frame(
    Asset = asset,
    Model = model_name,
    Convergence = tryCatch(
      fit@fit$convergence,
      error = function(e) NA
    ),
    Persistence = persistence_value,
    Ljung_Box_Residual_p = lb1$p.value,
    Ljung_Box_Squared_p = lb2$p.value,
    ARCH_LM_p = arch$p.value,
    Nyblom_Joint = ny_joint
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
  "T07_Final_GARCH_Diagnostics"
)


# 10. Weekday Diagnostics

weekday_btc <- aggregate(
  BTC ~ Day,
  data = e1$weekday_summary,
  FUN = function(x) {
    sd(
      x,
      na.rm = TRUE
    )
  }
)

weekday_j303 <- aggregate(
  J303 ~ Day,
  data = e1$weekday_summary,
  FUN = function(x) {
    sd(
      x,
      na.rm = TRUE
    )
  }
)

weekday_return_sd <- merge(
  weekday_btc,
  weekday_j303,
  by = "Day",
  all = TRUE
)

names(weekday_return_sd) <- c(
  "Day",
  "Bitcoin_SD",
  "J303_SD"
)

write_table(
  weekday_return_sd,
  "T08_Weekday_Return_SD",
  appendix = TRUE
)


# 11. GPR -> Bitcoin Volatility

write_table(
  e3$correlation.summary,
  "T09_GPR_Bitcoin_Correlation"
)

write_table(
  e3$regression.summary,
  "T10_GPR_Bitcoin_Regression"
)

write_table(
  e3$gpr.component.summary,
  "T11_GPR_Component_Regression"
)

write_table(
  e3$gpr.component.dynamic.summary,
  "T11a_GPR_Component_Dynamic_Regression",
  appendix = TRUE
)

write_table(
  e3$post2017.comparison,
  "T12_GPR_Bitcoin_Post2017"
)


# 12. Bitcoin -> J303 Volatility

write_table(
  e4$correlation.summary,
  "T13_Bitcoin_J303_Correlation"
)


t14_spillover <- e4$regression.summary

t14_incremental <- e4$incremental.summary

t14_incremental <- t14_incremental %>%
  select(
    Restricted_R2,
    Full_R2,
    Delta_R2,
    Partial_F_Statistic,
    Partial_F_P_Value
  )

t14_spillover <- t14_spillover %>%
  mutate(
    Restricted_R2 = NA_real_,
    Full_R2 = NA_real_,
    Delta_R2 = NA_real_,
    Partial_F_Statistic = NA_real_,
    Partial_F_P_Value = NA_real_
  )

t14_dynamic <- which(
  t14_spillover$Model == "Dynamic 2 Lags"
)

if (length(t14_dynamic) == 1) {
  
  t14_spillover$Restricted_R2[t14_dynamic] <-
    t14_incremental$Restricted_R2[1]
  
  t14_spillover$Full_R2[t14_dynamic] <-
    t14_incremental$Full_R2[1]
  
  t14_spillover$Delta_R2[t14_dynamic] <-
    t14_incremental$Delta_R2[1]
  
  t14_spillover$Partial_F_Statistic[t14_dynamic] <-
    t14_incremental$Partial_F_Statistic[1]
  
  t14_spillover$Partial_F_P_Value[t14_dynamic] <-
    t14_incremental$Partial_F_P_Value[1]
}

t14_spillover <- t14_spillover %>%
  relocate(
    Restricted_R2,
    Full_R2,
    Delta_R2,
    Partial_F_Statistic,
    Partial_F_P_Value,
    .after = Adj_R2
  )

write_table(
  t14_spillover,
  "T14_Bitcoin_J303_Spillover_Models"
)

write_table(
  e4$granger.summary,
  "T15_Granger_Causality"
)


# Post-2017 incremental R-squared and partial F-test

post2017_data <- master$data0 %>%
  filter(
    Date >= as.Date("2017-01-01")
  ) %>%
  mutate(
    
    BTC_Lag1 = lag(
      BTC_Volatility,
      1
    ),
    
    BTC_Lag2 = lag(
      BTC_Volatility,
      2
    ),
    
    J303_Lag1 = lag(
      J303_Volatility,
      1
    ),
    
    J303_Lag2 = lag(
      J303_Volatility,
      2
    )
    
  ) %>%
  na.omit()


post2017_restricted <- lm(
  J303_Volatility ~
    J303_Lag1 +
    J303_Lag2,
  data = post2017_data
)


post2017_full <- lm(
  J303_Volatility ~
    BTC_Volatility +
    BTC_Lag1 +
    BTC_Lag2 +
    J303_Lag1 +
    J303_Lag2,
  data = post2017_data
)


post2017_partial_f <- anova(
  post2017_restricted,
  post2017_full
)


post2017_incremental <- data.frame(
  Restricted_R2 =
    summary(post2017_restricted)$r.squared,
  
  Full_R2 =
    summary(post2017_full)$r.squared,
  
  Delta_R2 =
    summary(post2017_full)$r.squared -
    summary(post2017_restricted)$r.squared,
  
  Partial_F_Statistic =
    post2017_partial_f$F[2],
  
  Partial_F_P_Value =
    post2017_partial_f$`Pr(>F)`[2]
)


t16_post2017 <- e4$post2017.summary %>%
  mutate(
    Restricted_R2 = NA_real_,
    Full_R2 = NA_real_,
    Delta_R2 = NA_real_,
    Partial_F_Statistic = NA_real_,
    Partial_F_P_Value = NA_real_
  )

t16_dynamic <- which(
  t16_post2017$Model == "Dynamic 2 Lags"
)

if (length(t16_dynamic) == 1) {
  
  t16_post2017$Restricted_R2[t16_dynamic] <-
    post2017_incremental$Restricted_R2
  
  t16_post2017$Full_R2[t16_dynamic] <-
    post2017_incremental$Full_R2
  
  t16_post2017$Delta_R2[t16_dynamic] <-
    post2017_incremental$Delta_R2
  
  t16_post2017$Partial_F_Statistic[t16_dynamic] <-
    post2017_incremental$Partial_F_Statistic
  
  t16_post2017$Partial_F_P_Value[t16_dynamic] <-
    post2017_incremental$Partial_F_P_Value
}

t16_post2017 <- t16_post2017 %>%
  relocate(
    Restricted_R2,
    Full_R2,
    Delta_R2,
    Partial_F_Statistic,
    Partial_F_P_Value,
    .after = Adj_R2
  )

write_table(
  t16_post2017,
  "T16_Bitcoin_J303_Post2017"
)

write_table(
  e4$granger.post2017.summary,
  "T17_Granger_Post2017"
)

write_table(
  e4$diagnostic.summary,
  "T18_Bitcoin_J303_Diagnostics",
  appendix = TRUE
)

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
  "T20_GPR_Thresholds"
)

write_table(
  e5$regime.summary,
  "T21_GPR_Regime_Descriptives"
)

write_table(
  e5$dynamic.comparison,
  "T22_GPR_Regime_Dynamic_Models"
)


t23_act <- e5$act.dynamic.comparison

t23_act_incremental <- e5$act.incremental.r2 %>%
  select(
    Regime,
    Restricted_R2,
    Full_R2,
    Delta_R2,
    Partial_F_Statistic,
    Partial_F_P_Value
  )

t23_act <- t23_act %>%
  left_join(
    t23_act_incremental,
    by = "Regime"
  ) %>%
  relocate(
    Restricted_R2,
    Full_R2,
    Delta_R2,
    Partial_F_Statistic,
    Partial_F_P_Value,
    .after = Adj_R2
  )

write_table(
  t23_act,
  "T23_GPR_ACT_Regime_Dynamic_Models",
  appendix = TRUE
)


t24_threat <- e5$threat.dynamic.comparison

t24_threat_incremental <- e5$threat.incremental.r2 %>%
  select(
    Regime,
    Restricted_R2,
    Full_R2,
    Delta_R2,
    Partial_F_Statistic,
    Partial_F_P_Value
  )

t24_threat <- t24_threat %>%
  left_join(
    t24_threat_incremental,
    by = "Regime"
  ) %>%
  relocate(
    Restricted_R2,
    Full_R2,
    Delta_R2,
    Partial_F_Statistic,
    Partial_F_P_Value,
    .after = Adj_R2
  )

write_table(
  t24_threat,
  "T24_GPR_THREAT_Regime_Dynamic_Models",
  appendix = TRUE
)


# 14. GPR Regime Interaction

gprd_interaction <- data.frame(
  Variable = c(
    "GPR",
    "Elevated GPR",
    "GPR x Elevated GPR"
  ),
  
  Estimate = c(
    e5$interaction.nw[
      "GPRD",
      "Estimate"
    ],
    
    e5$interaction.nw[
      "GPR_RegimeElevated GPR",
      "Estimate"
    ],
    
    e5$interaction.nw[
      "GPRD:GPR_RegimeElevated GPR",
      "Estimate"
    ]
  ),
  
  Robust_SE = c(
    e5$interaction.nw[
      "GPRD",
      "Std. Error"
    ],
    
    e5$interaction.nw[
      "GPR_RegimeElevated GPR",
      "Std. Error"
    ],
    
    e5$interaction.nw[
      "GPRD:GPR_RegimeElevated GPR",
      "Std. Error"
    ]
  ),
  
  P_Value = c(
    e5$interaction.nw[
      "GPRD",
      "Pr(>|t|)"
    ],
    
    e5$interaction.nw[
      "GPR_RegimeElevated GPR",
      "Pr(>|t|)"
    ],
    
    e5$interaction.nw[
      "GPRD:GPR_RegimeElevated GPR",
      "Pr(>|t|)"
    ]
  )
)

write_table(
  gprd_interaction,
  "T25_GPR_Regime_Interaction"
)

write_table(
  e5$interaction.summary,
  "T25a_GPR_Alternative_Measure_Interactions",
  appendix = TRUE
)


# 15. GPR Regimes: Bitcoin -> J303

write_table(
  e7$regime.summary,
  "T26_Bitcoin_J303_Regime_Descriptives"
)

write_table(
  e7$correlation.summary,
  "T27_Bitcoin_J303_Regime_Correlation"
)

write_table(
  e7$dynamic.comparison,
  "T28_Bitcoin_J303_Regime_Spillover"
)

write_table(
  e7$interaction.summary,
  "T29_Bitcoin_J303_Regime_Interaction"
)

write_table(
  e7$act.dynamic.comparison,
  "T30_Bitcoin_J303_GPR_ACT_Regime_Spillover",
  appendix = TRUE
)

write_table(
  e7$threat.dynamic.comparison,
  "T31_Bitcoin_J303_GPR_THREAT_Regime_Spillover",
  appendix = TRUE
)

write_table(
  e7$gprd.incremental.r2.partial,
  "T31a_Bitcoin_J303_GPRD_Regime_Incremental_R2"
)

write_table(
  e7$act.incremental.r2.partial,
  "T31b_Bitcoin_J303_ACT_Regime_Incremental_R2",
  appendix = TRUE
)

write_table(
  e7$threat.incremental.r2.partial,
  "T31c_Bitcoin_J303_THREAT_Regime_Incremental_R2",
  appendix = TRUE
)


# 16. GPR -> Bitcoin Event Analysis

write_table(
  e6$event.results_21$events,
  "T32_GPR_Bitcoin_Events_21Day"
)

write_table(
  e6$event.results_21$statistics,
  "T33_GPR_Bitcoin_Event_Statistics_21Day",
  appendix = TRUE
)

event21_regression <- e6$event.results_21$regression

write_table(
  event21_regression,
  "T34_GPR_Bitcoin_Event_Regression_21Day"
)

write_table(
  e6$event.window.comparison,
  "T35_GPR_Bitcoin_Event_Window_Comparison",
  appendix = TRUE
)

write_table(
  e6$significant.correlation.grid,
  "T35a_GPR_Bitcoin_Event_Significance_Correlation_Grid",
  appendix = TRUE
)

write_table(
  e6$significant.regression.grid,
  "T35b_GPR_Bitcoin_Event_Significance_NW_Grid",
  appendix = TRUE
)


# 17. GPR -> Bitcoin Event Regression Stability

write_table(
  e6$regression.stability,
  "T37_GPR_Bitcoin_Event_Regression_Stability",
  appendix = TRUE
)


# 18. Bitcoin -> J303 Event Analysis

write_table(
  e8$event.results_21$events,
  "T38_Bitcoin_J303_Events_21Day"
)

write_table(
  e8$event.results_21$statistics,
  "T39_Bitcoin_J303_Event_Statistics_21Day",
  appendix = TRUE
)

write_table(
  e8$event.results_21$regression,
  "T40_Bitcoin_J303_Event_Regression_21Day"
)

write_table(
  e8$event.window.comparison,
  "T41_Bitcoin_J303_Event_Window_Comparison",
  appendix = TRUE
)

write_table(
  e8$significant.correlation.grid,
  "T41a_Bitcoin_J303_Event_Significance_Correlation_Grid",
  appendix = TRUE
)

write_table(
  e8$significant.regression.grid,
  "T41b_Bitcoin_J303_Event_Significance_NW_Grid",
  appendix = TRUE
)

write_table(
  e8$regression.stability,
  "T42_Bitcoin_J303_Event_Regression_Stability",
  appendix = TRUE
)


# 19. Peak Date Source Audit

peak_date_source_audit <- readxl::read_excel(
  file.path(
    script_dir,
    "Thesis_GPR_Event_Audit_FINAL_VERIFIED.xlsx"
  ),
  sheet = "Peak Data Sources"
)

if (
  "Peak_Date" %in% names(peak_date_source_audit)
) {
  
  peak_date_duplicates <- peak_date_source_audit %>%
    count(
      Peak_Date
    ) %>%
    filter(
      n > 1
    )
  
  if (nrow(peak_date_duplicates) > 0) {
    stop(
      "Peak Data Sources contains duplicate peak dates."
    )
  }
  
}

write_table(
  peak_date_source_audit,
  "T36_GPR_Peak_Date_Source_Audit",
  appendix = TRUE
)


# 20. Event Method Notes

event_method_notes <- data.frame(
  
  Item = c(
    "Event identification",
    "Smoothed event period",
    "Raw peak selection",
    "Cross-window event rule",
    "Event analysis windows",
    "11-day Newey-West inference"
  ),
  
  Description = c(
    "For each GPR measure and smoothing window, the five largest smoothed peaks are identified.",
    
    "Each event period is defined from the preceding trough to the following trough in the smoothed GPR series.",
    
    "Within each full smoothed event period, the three highest observations from the unsmoothed GPR series are selected.",
    
    "All three selected raw peak dates are checked against the full smoothed event windows of the other tests. Any underlying event associated with a selected raw peak inside another event window is added as an additional event without replacing the original top-three classification.",
    
    "Event regressions use fixed peak-centred windows containing exactly 11, 21, or 31 trading observations.",
    
    "With only 11 observations, Newey-West inference is treated as unreliable and should be interpreted descriptively rather than as strong inferential evidence."
  )
)

write_table(
  event_method_notes,
  "T36a_Event_Method_Notes",
  appendix = TRUE
)


# 21. Prepare Figure Data

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


# 22. Create Figures

p_btc_return <- ggplot(
  plot_data,
  aes(
    Date,
    Bitcoin
  )
) +
  geom_line(
    linewidth = 0.35,
    colour = "black"
  ) +
  labs(
    title = "Bitcoin Daily Log Returns",
    x = "Date",
    y = "Log return"
  ) +
  theme_thesis


p_j303_return <- ggplot(
  plot_data,
  aes(
    Date,
    J303
  )
) +
  geom_line(
    linewidth = 0.35,
    colour = "black"
  ) +
  labs(
    title = "J303 Daily Log Returns",
    x = "Date",
    y = "Log return"
  ) +
  theme_thesis


p_gpr <- ggplot(
  plot_data,
  aes(
    Date,
    GPRD
  )
) +
  geom_line(
    linewidth = 0.4,
    colour = "black"
  ) +
  labs(
    title = "Daily Geopolitical Risk Index",
    x = "Date",
    y = "GPRD"
  ) +
  theme_thesis


p_gpr_act <- ggplot(
  plot_data,
  aes(
    Date,
    GPRD_ACT
  )
) +
  geom_line(
    linewidth = 0.4,
    colour = "black"
  ) +
  labs(
    title = "Daily Geopolitical Risk: Acts",
    x = "Date",
    y = "GPRD_ACT"
  ) +
  theme_thesis


p_gpr_threat <- ggplot(
  plot_data,
  aes(
    Date,
    GPRD_THREAT
  )
) +
  geom_line(
    linewidth = 0.4,
    colour = "black"
  ) +
  labs(
    title = "Daily Geopolitical Risk: Threats",
    x = "Date",
    y = "GPRD_THREAT"
  ) +
  theme_thesis


p_btc_vol <- ggplot(
  plot_data,
  aes(
    Date,
    BTC_Volatility
  )
) +
  geom_line(
    linewidth = 0.4,
    colour = "black"
  ) +
  labs(
    title = "Bitcoin Conditional Volatility",
    x = "Date",
    y = "Conditional volatility"
  ) +
  theme_thesis


p_j303_vol <- ggplot(
  plot_data,
  aes(
    Date,
    J303_Volatility
  )
) +
  geom_line(
    linewidth = 0.4,
    colour = "black"
  ) +
  labs(
    title = "J303 Conditional Volatility",
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
    method = "lm",
    se = TRUE,
    colour = "black",
    linetype = "dashed"
  ) +
  geom_smooth(
    method = "loess",
    se = TRUE,
    colour = "grey40"
  ) +
  labs(
    title = "Bitcoin Conditional Volatility and Geopolitical Risk",
    x = "Geopolitical Risk Index",
    y = "Bitcoin conditional volatility"
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
    method = "lm",
    se = TRUE,
    colour = "black",
    linetype = "dashed"
  ) +
  geom_smooth(
    method = "loess",
    se = TRUE,
    colour = "grey40"
  ) +
  labs(
    title = "Bitcoin and J303 Conditional Volatility",
    x = "Bitcoin conditional volatility",
    y = "J303 conditional volatility"
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


event21 <- e6$event.results_21$regression

event21$Event <- factor(
  event21$Event,
  levels = unique(event21$Event)
)


p_event_gpr_btc <- ggplot(
  event21,
  aes(
    Event,
    GPR_Coefficient,
    fill = Measure
  )
) +
  geom_col(
    colour = "black"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "black"
  ) +
  scale_fill_grey(
    start = 0.85,
    end = 0.35
  ) +
  labs(
    title = "GPR–Bitcoin Volatility Relationship During Identified Events",
    x = "Event",
    y = "GPR coefficient",
    fill = "GPR measure"
  ) +
  theme_thesis +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


event21_spill <- e8$event.results_21$regression

event21_spill$Event <- factor(
  event21_spill$Event,
  levels = unique(event21_spill$Event)
)


p_event_spill <- ggplot(
  event21_spill,
  aes(
    Event,
    BTC_Coefficient,
    fill = Measure
  )
) +
  geom_col(
    colour = "black"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "black"
  ) +
  scale_fill_grey(
    start = 0.85,
    end = 0.35
  ) +
  labs(
    title = "Bitcoin–J303 Volatility Relationship During Identified Events",
    x = "Event",
    y = "Bitcoin volatility coefficient",
    fill = "GPR measure"
  ) +
  theme_thesis


btc_std_resid <- data.frame(
  Date = data0$Date,
  Standardized_Residual = as.numeric(
    residuals(
      master$btc.fit,
      standardize = TRUE
    )
  )
)


j303_std_resid <- data.frame(
  Date = data0$Date,
  Standardized_Residual = as.numeric(
    residuals(
      master$j303.fit,
      standardize = TRUE
    )
  )
)


p_btc_resid <- ggplot(
  btc_std_resid,
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
  labs(
    title = "Bitcoin Standardized Residuals",
    x = "Date",
    y = "Standardized residual"
  ) +
  theme_thesis


p_j303_resid <- ggplot(
  j303_std_resid,
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
  labs(
    title = "J303 Standardized Residuals",
    x = "Date",
    y = "Standardized residual"
  ) +
  theme_thesis


# 23. Save Figures

save_plot(
  p_btc_return,
  "F01_Bitcoin_Returns"
)

save_plot(
  p_j303_return,
  "F02_J303_Returns"
)

save_plot(
  p_gpr,
  "F03_GPRD"
)

save_plot(
  p_gpr_act,
  "F03a_GPRD_ACT"
)

save_plot(
  p_gpr_threat,
  "F03b_GPRD_THREAT"
)

save_plot(
  p_btc_vol,
  "F04_Bitcoin_Conditional_Volatility"
)

save_plot(
  p_j303_vol,
  "F05_J303_Conditional_Volatility"
)

save_plot(
  p_gpr_btc,
  "F06_GPR_Bitcoin_Volatility"
)

save_plot(
  p_btc_j303,
  "F07_Bitcoin_J303_Volatility"
)

save_plot(
  p_regime,
  "F08_GPR_Regimes"
)

save_plot(
  p_event_gpr_btc,
  "F09_GPR_Bitcoin_Event_Coefficients_21Day",
  width = 8,
  height = 5
)

save_plot(
  p_event_spill,
  "F10_Bitcoin_J303_Event_Coefficients_21Day",
  width = 8,
  height = 5
)

save_plot(
  p_btc_resid,
  "F11_Bitcoin_Standardized_Residuals"
)

save_plot(
  p_j303_resid,
  "F12_J303_Standardized_Residuals"
)


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



