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

# 5. Estimate models

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

partial.f <- anova(
  restricted.dynamic,
  full.dynamic
)

incremental.summary <- data.frame(
  Partial_F = round(
    partial.f$F[2],
    4
  ),
  P_Value = signif(
    partial.f$`Pr(>F)`[2],
    4
  )
)

incremental.summary


# 10. Dynamic model serial correlation

bg.dynamic.2 <- bgtest(
  full.dynamic,
  order = 2
)

bg.dynamic.2


###############################################################
# Section C: Granger Causality
###############################################################

# 11. Granger causality

run_granger <- function(data) {
  
  btc.j303 <- grangertest(
    J303_Volatility ~ BTC_Volatility,
    order = 2,
    data = data
  )
  
  j303.btc <- grangertest(
    BTC_Volatility ~ J303_Volatility,
    order = 2,
    data = data
  )
  
  data.frame(
    Direction = c(
      "Bitcoin Volatility -> J303 Volatility",
      "J303 Volatility -> Bitcoin Volatility"
    ),
    F_Statistic = round(
      c(
        btc.j303$F[2],
        j303.btc$F[2]
      ),
      4
    ),
    P_Value = signif(
      c(
        btc.j303$`Pr(>F)`[2],
        j303.btc$`Pr(>F)`[2]
      ),
      4
    )
  )
}

granger.summary <- run_granger(data)

granger.summary


###############################################################
# Section D: Post-2017 Robustness Analysis
###############################################################

# 12. Estimate post-2017 models

post2017.data <- data0 %>%
  filter(
    Date >= as.Date("2018-01-01")
  ) %>%
  prepare_spillover()

post2017.models <- fit_spillover_models(
  post2017.data
)

post2017.nw <- lapply(
  post2017.models,
  run_nw
)

names(post2017.nw) <- names(post2017.models)


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
    across(
      contains("pvalue"),
      ~ signif(.x, 4)
    )
  )

rownames(post2017.summary) <- NULL

post2017.summary


# 14. Post-2017 Granger causality

granger.post2017.summary <- run_granger(
  post2017.data
)

granger.post2017.summary


###############################################################
# Section E: Summary Tables
###############################################################

# 15. Regression summary

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
    across(
      contains("pvalue"),
      ~ signif(.x, 4)
    )
  )

rownames(regression.summary) <- NULL

regression.summary


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


# 17. Incremental Bitcoin contribution summary

incremental.summary


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







