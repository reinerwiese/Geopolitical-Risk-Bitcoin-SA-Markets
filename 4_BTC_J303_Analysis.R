# 1. Load required packages

library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)
library(tseries)


# 2. Prepare data

prepare_spillover <- function(data) {
  data %>%
    mutate(
      BTC_Volatility_Lag1 = lag(BTC_Volatility, 1),
      BTC_Volatility_Lag2 = lag(BTC_Volatility, 2),
      BTC_Volatility_USD_Lag1 = lag(BTC_Volatility_USD, 1),
      BTC_Volatility_USD_Lag2 = lag(BTC_Volatility_USD, 2),
      J303_Volatility_Lag1 = lag(J303_Volatility, 1),
      J303_Volatility_Lag2 = lag(J303_Volatility, 2)
    ) %>%
    na.omit()
}

fit_spillover_models <- function(data) {
  list(
    "No Lag" = lm(
      J303_Volatility ~ BTC_Volatility,
      data = data
    ),
    
    "1 Lag" = lm(
      J303_Volatility ~
        BTC_Volatility +
        BTC_Volatility_Lag1,
      data = data
    ),
    
    "2 Lags" = lm(
      J303_Volatility ~
        BTC_Volatility +
        BTC_Volatility_Lag1 +
        BTC_Volatility_Lag2,
      data = data
    ),
    
    "Dynamic 2 Lags" = lm(
      J303_Volatility ~
        J303_Volatility_Lag1 +
        J303_Volatility_Lag2 +
        BTC_Volatility +
        BTC_Volatility_Lag1 +
        BTC_Volatility_Lag2,
      data = data
    )
  )
}


# USD Bitcoin volatility models

fit_spillover_models_usd <- function(data) {
  list(
    "No Lag" = lm(
      J303_Volatility ~ BTC_Volatility_USD,
      data = data
    ),
    
    "1 Lag" = lm(
      J303_Volatility ~
        BTC_Volatility_USD +
        BTC_Volatility_USD_Lag1,
      data = data
    ),
    
    "2 Lags" = lm(
      J303_Volatility ~
        BTC_Volatility_USD +
        BTC_Volatility_USD_Lag1 +
        BTC_Volatility_USD_Lag2,
      data = data
    ),
    
    "Dynamic 2 Lags" = lm(
      J303_Volatility ~
        J303_Volatility_Lag1 +
        J303_Volatility_Lag2 +
        BTC_Volatility_USD +
        BTC_Volatility_USD_Lag1 +
        BTC_Volatility_USD_Lag2,
      data = data
    )
  )
}


run_nw <- function(model) {
  coeftest(
    model,
    vcov = NeweyWest(
      model,
      prewhite = FALSE
    )
  )
}

get_coef <- function(model, variable) {
  if (variable %in% names(coef(model))) {
    unname(coef(model)[variable])
  } else {
    NA
  }
}

get_nw_p <- function(nw, variable) {
  if (variable %in% rownames(nw)) {
    nw[variable, "Pr(>|t|)"]
  } else {
    NA
  }
}

data <- prepare_spillover(data0)


###############################################################
# Section A: Exploratory Analysis
###############################################################

# 3. Correlation analysis

btc.j303.cor <- cor.test(
  data$BTC_Volatility,
  data$J303_Volatility
)

correlation.summary <- data.frame(
  Relationship = "Bitcoin vs J303 Volatility",
  Correlation = round(
    unname(btc.j303.cor$estimate),
    4
  ),
  P_Value = signif(
    btc.j303.cor$p.value,
    4
  )
)

correlation.summary


# 4. Bitcoin Volatility vs J303 Volatility

ggplot(
  data,
  aes(x = BTC_Volatility, y = J303_Volatility)
) +
  geom_point(alpha = 0.6) +
  geom_smooth(
    aes(colour = "Linear"),
    method = "lm",
    se = TRUE
  ) +
  geom_smooth(
    aes(colour = "LOESS"),
    method = "loess",
    se = TRUE
  ) +
  scale_colour_manual(
    values = c(
      "Linear" = "blue",
      "LOESS" = "red"
    )
  ) +
  theme_minimal() +
  labs(
    title = "Bitcoin Volatility vs J303 Volatility",
    x = "Bitcoin Conditional Volatility",
    y = "J303 Conditional Volatility",
    colour = "Trend"
  )


###############################################################
# Section B: Volatility Regression Analysis
###############################################################

# 5. Estimate ZAR models

models <- fit_spillover_models(data)

lapply(
  models,
  summary
)


# 6. Newey-West inference

nw.models <- lapply(
  models,
  run_nw
)

names(nw.models) <- names(models)

nw.models


# 6.1 Estimate USD models

usd.models <- fit_spillover_models_usd(data)

lapply(
  usd.models,
  summary
)

usd.nw <- lapply(
  usd.models,
  run_nw
)

names(usd.nw) <- names(usd.models)

usd.nw


# 7. Model diagnostics

run_diagnostics <- function(model) {
  
  list(
    JB = jarque.bera.test(residuals(model)),
    
    Ljung_Box = Box.test(
      residuals(model),
      lag = 20,
      type = "Ljung-Box"
    ),
    
    Breusch_Pagan = bptest(model),
    
    Breusch_Godfrey = bgtest(
      model,
      order = 2
    )
  )
}

diagnostics <- lapply(
  models,
  run_diagnostics
)

names(diagnostics) <- names(models)

diagnostics


# 8. Restricted and full dynamic models

restricted.dynamic <- lm(
  J303_Volatility ~
    J303_Volatility_Lag1 +
    J303_Volatility_Lag2,
  data = data
)

full.dynamic <- models[["Dynamic 2 Lags"]]


# 9. Incremental contribution of Bitcoin volatility

r2.restricted <- summary(
  restricted.dynamic
)$r.squared

r2.full <- summary(
  full.dynamic
)$r.squared

delta.r2 <- r2.full - r2.restricted

incremental.summary <- data.frame(
  Restricted_R2 = round(
    r2.restricted,
    4
  ),
  Full_R2 = round(
    r2.full,
    4
  ),
  Delta_R2 = round(
    delta.r2,
    4
  )
)

incremental.summary


# 9.1 Partial F-test using Newey-West covariance

partial.F.dynamic <- waldtest(
  restricted.dynamic,
  full.dynamic,
  vcov = NeweyWest(
    full.dynamic,
    prewhite = FALSE
  )
)

partial.F.dynamic


# Add partial F-test results to incremental summary

incremental.summary <- data.frame(
  Restricted_R2 = round(
    r2.restricted,
    4
  ),
  Full_R2 = round(
    r2.full,
    4
  ),
  Delta_R2 = round(
    delta.r2,
    4
  ),
  Partial_F_Statistic = round(
    partial.F.dynamic$F[2],
    4
  ),
  Partial_F_P_Value = signif(
    partial.F.dynamic$`Pr(>F)`[2],
    4
  )
)

incremental.summary


# 9.2 USD dynamic model

restricted.dynamic.usd <- lm(
  J303_Volatility ~
    J303_Volatility_Lag1 +
    J303_Volatility_Lag2,
  data = data
)

full.dynamic.usd <- usd.models[["Dynamic 2 Lags"]]

r2.restricted.usd <- summary(
  restricted.dynamic.usd
)$r.squared

r2.full.usd <- summary(
  full.dynamic.usd
)$r.squared

delta.r2.usd <- r2.full.usd - r2.restricted.usd

partial.F.dynamic.usd <- waldtest(
  restricted.dynamic.usd,
  full.dynamic.usd,
  vcov = NeweyWest(
    full.dynamic.usd,
    prewhite = FALSE
  ),
  test = "F"
)

incremental.summary.usd <- data.frame(
  Bitcoin_Measure = "USD",
  Restricted_R2 = round(
    r2.restricted.usd,
    4
  ),
  Full_R2 = round(
    r2.full.usd,
    4
  ),
  Delta_R2 = round(
    delta.r2.usd,
    4
  ),
  Partial_F_Statistic = round(
    partial.F.dynamic.usd$F[2],
    4
  ),
  Partial_F_P_Value = signif(
    partial.F.dynamic.usd$`Pr(>F)`[2],
    4
  )
)

incremental.summary.usd


# 10. Dynamic model serial correlation

bg.dynamic.2 <- bgtest(
  full.dynamic,
  order = 2
)

bg.dynamic.2


###############################################################
# Section C: Granger Causality
###############################################################

# 11. Newey-West Granger causality tests

run_granger_nw <- function(
    data,
    btc_var,
    btc_lag1,
    btc_lag2,
    measure
) {
  
  restricted.j303 <- lm(
    J303_Volatility ~
      J303_Volatility_Lag1 +
      J303_Volatility_Lag2,
    data = data
  )
  
  full.j303 <- lm(
    as.formula(
      paste(
        "J303_Volatility ~ J303_Volatility_Lag1 +",
        "J303_Volatility_Lag2 +",
        btc_lag1, "+", btc_lag2
      )
    ),
    data = data
  )
  
  test.btc.to.j303 <- waldtest(
    restricted.j303,
    full.j303,
    vcov = NeweyWest(
      full.j303,
      prewhite = FALSE
    ),
    test = "F"
  )
  
  
  restricted.btc <- lm(
    as.formula(
      paste(
        btc_var, "~",
        btc_lag1, "+", btc_lag2
      )
    ),
    data = data
  )
  
  full.btc <- lm(
    as.formula(
      paste(
        btc_var, "~",
        btc_lag1, "+", btc_lag2, "+",
        "J303_Volatility_Lag1 + J303_Volatility_Lag2"
      )
    ),
    data = data
  )
  
  test.j303.to.btc <- waldtest(
    restricted.btc,
    full.btc,
    vcov = NeweyWest(
      full.btc,
      prewhite = FALSE
    ),
    test = "F"
  )
  
  
  data.frame(
    Bitcoin_Measure = measure,
    
    Direction = c(
      "Bitcoin Volatility -> J303 Volatility",
      "J303 Volatility -> Bitcoin Volatility"
    ),
    
    F_Statistic = round(
      c(
        test.btc.to.j303$F[2],
        test.j303.to.btc$F[2]
      ),
      4
    ),
    
    P_Value = signif(
      c(
        test.btc.to.j303$`Pr(>F)`[2],
        test.j303.to.btc$`Pr(>F)`[2]
      ),
      4
    )
  )
}


granger.summary <- bind_rows(
  
  run_granger_nw(
    data,
    "BTC_Volatility",
    "BTC_Volatility_Lag1",
    "BTC_Volatility_Lag2",
    "ZAR"
  ),
  
  run_granger_nw(
    data,
    "BTC_Volatility_USD",
    "BTC_Volatility_USD_Lag1",
    "BTC_Volatility_USD_Lag2",
    "USD"
  )
  
)

granger.summary


###############################################################
# Section D: Post-2017 Robustness Analysis
###############################################################

# 12. Estimate post-2017 models
# Lags were generated on the full sample before filtering.
# This retains valid lag values for the first post-2017 observations.

post2017.data <- data %>%
  filter(
    Date >= as.Date("2018-01-01")
  )

post2017.models <- fit_spillover_models(
  post2017.data
)

post2017.nw <- lapply(
  post2017.models,
  run_nw
)

names(post2017.nw) <- names(post2017.models)


# 12.1 Post-2017 USD models

post2017.usd.models <- fit_spillover_models_usd(
  post2017.data
)

post2017.usd.nw <- lapply(
  post2017.usd.models,
  run_nw
)

names(post2017.usd.nw) <- names(post2017.usd.models)


# 12.2 Post-2017 incremental contribution of Bitcoin volatility
# Restricted models include J303 volatility lags only.
# Full models add contemporaneous Bitcoin volatility and its two lags.

post2017.restricted.dynamic <- lm(
  J303_Volatility ~
    J303_Volatility_Lag1 +
    J303_Volatility_Lag2,
  data = post2017.data
)

post2017.full.dynamic <- post2017.models[["Dynamic 2 Lags"]]

post2017.r2.restricted <- summary(
  post2017.restricted.dynamic
)$r.squared

post2017.r2.full <- summary(
  post2017.full.dynamic
)$r.squared

post2017.delta.r2 <- post2017.r2.full - post2017.r2.restricted

post2017.partial.F.dynamic <- waldtest(
  post2017.restricted.dynamic,
  post2017.full.dynamic,
  vcov = NeweyWest(
    post2017.full.dynamic,
    prewhite = FALSE
  ),
  test = "F"
)

post2017.restricted.dynamic.usd <- lm(
  J303_Volatility ~
    J303_Volatility_Lag1 +
    J303_Volatility_Lag2,
  data = post2017.data
)

post2017.full.dynamic.usd <- post2017.usd.models[["Dynamic 2 Lags"]]

post2017.r2.restricted.usd <- summary(
  post2017.restricted.dynamic.usd
)$r.squared

post2017.r2.full.usd <- summary(
  post2017.full.dynamic.usd
)$r.squared

post2017.delta.r2.usd <- post2017.r2.full.usd -
  post2017.r2.restricted.usd

post2017.partial.F.dynamic.usd <- waldtest(
  post2017.restricted.dynamic.usd,
  post2017.full.dynamic.usd,
  vcov = NeweyWest(
    post2017.full.dynamic.usd,
    prewhite = FALSE
  ),
  test = "F"
)

post2017.incremental.summary <- data.frame(
  Bitcoin_Measure = c("ZAR", "USD"),
  Restricted_R2 = c(
    post2017.r2.restricted,
    post2017.r2.restricted.usd
  ),
  Full_R2 = c(
    post2017.r2.full,
    post2017.r2.full.usd
  ),
  Delta_R2 = c(
    post2017.delta.r2,
    post2017.delta.r2.usd
  ),
  Partial_F_Statistic = c(
    post2017.partial.F.dynamic$F[2],
    post2017.partial.F.dynamic.usd$F[2]
  ),
  Partial_F_P_Value = c(
    post2017.partial.F.dynamic$`Pr(>F)`[2],
    post2017.partial.F.dynamic.usd$`Pr(>F)`[2]
  )
) %>%
  mutate(
    across(
      c(Restricted_R2, Full_R2, Delta_R2, Partial_F_Statistic),
      ~ round(.x, 4)
    ),
    Partial_F_P_Value = signif(Partial_F_P_Value, 4)
  )

post2017.incremental.summary


# 13. Post-2017 regression summary

post2017.summary <- data.frame(
  
  Model = names(post2017.models),
  
  BTC_Coefficient = sapply(
    post2017.models,
    get_coef,
    variable = "BTC_Volatility"
  ),
  
  BTC_Lag1_Coefficient = sapply(
    post2017.models,
    get_coef,
    variable = "BTC_Volatility_Lag1"
  ),
  
  BTC_Lag2_Coefficient = sapply(
    post2017.models,
    get_coef,
    variable = "BTC_Volatility_Lag2"
  ),
  
  Adj_R2 = sapply(
    post2017.models,
    function(x) summary(x)$adj.r.squared
  ),
  
  BTC_NW_pvalue = sapply(
    post2017.nw,
    get_nw_p,
    variable = "BTC_Volatility"
  ),
  
  BTC_Lag1_NW_pvalue = sapply(
    post2017.nw,
    get_nw_p,
    variable = "BTC_Volatility_Lag1"
  ),
  
  BTC_Lag2_NW_pvalue = sapply(
    post2017.nw,
    get_nw_p,
    variable = "BTC_Volatility_Lag2"
  )
) %>%
  mutate(
    across(
      contains("Coefficient"),
      ~ signif(.x, 4)
    ),
    Adj_R2 = round(
      Adj_R2,
      4
    ),
    across(
      contains("pvalue"),
      ~ signif(.x, 4)
    )
  )

rownames(post2017.summary) <- NULL

post2017.summary


# 13.1 Post-2017 USD regression summary

post2017.usd.summary <- data.frame(
  
  Model = names(post2017.usd.models),
  
  BTC_USD_Coefficient = sapply(
    post2017.usd.models,
    get_coef,
    variable = "BTC_Volatility_USD"
  ),
  
  BTC_USD_Lag1_Coefficient = sapply(
    post2017.usd.models,
    get_coef,
    variable = "BTC_Volatility_USD_Lag1"
  ),
  
  BTC_USD_Lag2_Coefficient = sapply(
    post2017.usd.models,
    get_coef,
    variable = "BTC_Volatility_USD_Lag2"
  ),
  
  Adj_R2 = sapply(
    post2017.usd.models,
    function(x) summary(x)$adj.r.squared
  ),
  
  BTC_USD_NW_pvalue = sapply(
    post2017.usd.nw,
    get_nw_p,
    variable = "BTC_Volatility_USD"
  ),
  
  BTC_USD_Lag1_NW_pvalue = sapply(
    post2017.usd.nw,
    get_nw_p,
    variable = "BTC_Volatility_USD_Lag1"
  ),
  
  BTC_USD_Lag2_NW_pvalue = sapply(
    post2017.usd.nw,
    get_nw_p,
    variable = "BTC_Volatility_USD_Lag2"
  )
) %>%
  mutate(
    across(
      contains("Coefficient"),
      ~ signif(.x, 4)
    ),
    Adj_R2 = round(
      Adj_R2,
      4
    ),
    across(
      contains("pvalue"),
      ~ signif(.x, 4)
    )
  )

rownames(post2017.usd.summary) <- NULL

post2017.usd.summary


# 14. Post-2017 Granger causality

granger.post2017.summary <- bind_rows(
  
  run_granger_nw(
    post2017.data,
    "BTC_Volatility",
    "BTC_Volatility_Lag1",
    "BTC_Volatility_Lag2",
    "ZAR"
  ),
  
  run_granger_nw(
    post2017.data,
    "BTC_Volatility_USD",
    "BTC_Volatility_USD_Lag1",
    "BTC_Volatility_USD_Lag2",
    "USD"
  )
  
)

granger.post2017.summary


###############################################################
# Section E: Summary Tables
###############################################################

# 15. ZAR regression summary

regression.summary <- data.frame(
  
  Model = names(models),
  
  BTC_Coefficient = sapply(
    models,
    get_coef,
    variable = "BTC_Volatility"
  ),
  
  BTC_Lag1_Coefficient = sapply(
    models,
    get_coef,
    variable = "BTC_Volatility_Lag1"
  ),
  
  BTC_Lag2_Coefficient = sapply(
    models,
    get_coef,
    variable = "BTC_Volatility_Lag2"
  ),
  
  J303_Lag1_Coefficient = sapply(
    models,
    get_coef,
    variable = "J303_Volatility_Lag1"
  ),
  
  J303_Lag2_Coefficient = sapply(
    models,
    get_coef,
    variable = "J303_Volatility_Lag2"
  ),
  
  Adj_R2 = sapply(
    models,
    function(x) summary(x)$adj.r.squared
  ),
  
  BTC_NW_pvalue = sapply(
    nw.models,
    get_nw_p,
    variable = "BTC_Volatility"
  ),
  
  BTC_Lag1_NW_pvalue = sapply(
    nw.models,
    get_nw_p,
    variable = "BTC_Volatility_Lag1"
  ),
  
  BTC_Lag2_NW_pvalue = sapply(
    nw.models,
    get_nw_p,
    variable = "BTC_Volatility_Lag2"
  )
) %>%
  mutate(
    across(
      contains("Coefficient"),
      ~ signif(.x, 4)
    ),
    Adj_R2 = round(
      Adj_R2,
      4
    ),
    across(
      contains("pvalue"),
      ~ signif(.x, 4)
    )
  )

rownames(regression.summary) <- NULL

regression.summary


# 15.1 USD regression summary

usd.regression.summary <- data.frame(
  
  Model = names(usd.models),
  
  BTC_USD_Coefficient = sapply(
    usd.models,
    get_coef,
    variable = "BTC_Volatility_USD"
  ),
  
  BTC_USD_Lag1_Coefficient = sapply(
    usd.models,
    get_coef,
    variable = "BTC_Volatility_USD_Lag1"
  ),
  
  BTC_USD_Lag2_Coefficient = sapply(
    usd.models,
    get_coef,
    variable = "BTC_Volatility_USD_Lag2"
  ),
  
  J303_Lag1_Coefficient = sapply(
    usd.models,
    get_coef,
    variable = "J303_Volatility_Lag1"
  ),
  
  J303_Lag2_Coefficient = sapply(
    usd.models,
    get_coef,
    variable = "J303_Volatility_Lag2"
  ),
  
  Adj_R2 = sapply(
    usd.models,
    function(x) summary(x)$adj.r.squared
  ),
  
  BTC_USD_NW_pvalue = sapply(
    usd.nw,
    get_nw_p,
    variable = "BTC_Volatility_USD"
  ),
  
  BTC_USD_Lag1_NW_pvalue = sapply(
    usd.nw,
    get_nw_p,
    variable = "BTC_Volatility_USD_Lag1"
  ),
  
  BTC_USD_Lag2_NW_pvalue = sapply(
    usd.nw,
    get_nw_p,
    variable = "BTC_Volatility_USD_Lag2"
  )
) %>%
  mutate(
    across(
      contains("Coefficient"),
      ~ signif(.x, 4)
    ),
    Adj_R2 = round(
      Adj_R2,
      4
    ),
    across(
      contains("pvalue"),
      ~ signif(.x, 4)
    )
  )

rownames(usd.regression.summary) <- NULL

usd.regression.summary


# 16. Diagnostic summary

diagnostic.summary <- data.frame(
  
  Model = names(models),
  
  JB_P_Value = sapply(
    diagnostics,
    function(x) x$JB$p.value
  ),
  
  Ljung_Box_P_Value = sapply(
    diagnostics,
    function(x) x$Ljung_Box$p.value
  ),
  
  BP_P_Value = sapply(
    diagnostics,
    function(x) x$Breusch_Pagan$p.value
  ),
  
  BG_2_P_Value = sapply(
    diagnostics,
    function(x) x$Breusch_Godfrey$p.value
  )
) %>%
  mutate(
    across(
      contains("P_Value"),
      ~ signif(.x, 4)
    )
  )

rownames(diagnostic.summary) <- NULL

diagnostic.summary


# 17. Incremental Bitcoin contribution summaries

incremental.summary

incremental.summary.usd


# 18. Dynamic serial correlation summary

dynamic.serial.summary <- data.frame(
  Test = "Breusch-Godfrey (2 lags)",
  Statistic = round(
    unname(bg.dynamic.2$statistic),
    4
  ),
  P_Value = signif(
    bg.dynamic.2$p.value,
    4
  )
)

dynamic.serial.summary


# 19. Combined incremental Bitcoin contribution summary
# Joint Newey-West tests assess whether contemporaneous Bitcoin volatility
# and its two lags are jointly zero, conditional on J303 volatility lags.

incremental.comparison.summary <- bind_rows(
  data.frame(
    Bitcoin_Measure = "ZAR",
    Sample = "Full sample",
    Restricted_R2 = r2.restricted,
    Full_R2 = r2.full,
    Delta_R2 = delta.r2,
    Partial_F_Statistic = partial.F.dynamic$F[2],
    Partial_F_P_Value = partial.F.dynamic$`Pr(>F)`[2]
  ),
  data.frame(
    Bitcoin_Measure = "USD",
    Sample = "Full sample",
    Restricted_R2 = r2.restricted.usd,
    Full_R2 = r2.full.usd,
    Delta_R2 = delta.r2.usd,
    Partial_F_Statistic = partial.F.dynamic.usd$F[2],
    Partial_F_P_Value = partial.F.dynamic.usd$`Pr(>F)`[2]
  ),
  data.frame(
    Bitcoin_Measure = "ZAR",
    Sample = "Post-2017",
    Restricted_R2 = post2017.r2.restricted,
    Full_R2 = post2017.r2.full,
    Delta_R2 = post2017.delta.r2,
    Partial_F_Statistic = post2017.partial.F.dynamic$F[2],
    Partial_F_P_Value = post2017.partial.F.dynamic$`Pr(>F)`[2]
  ),
  data.frame(
    Bitcoin_Measure = "USD",
    Sample = "Post-2017",
    Restricted_R2 = post2017.r2.restricted.usd,
    Full_R2 = post2017.r2.full.usd,
    Delta_R2 = post2017.delta.r2.usd,
    Partial_F_Statistic = post2017.partial.F.dynamic.usd$F[2],
    Partial_F_P_Value = post2017.partial.F.dynamic.usd$`Pr(>F)`[2]
  )
) %>%
  mutate(
    across(
      c(Restricted_R2, Full_R2, Delta_R2, Partial_F_Statistic),
      ~ round(.x, 4)
    ),
    Partial_F_P_Value = signif(Partial_F_P_Value, 4)
  )

incremental.comparison.summary







