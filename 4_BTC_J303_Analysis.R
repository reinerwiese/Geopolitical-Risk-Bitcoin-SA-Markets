# 1. Load required packages
library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)
library(tseries)

# 2. Prepare data
data <- na.omit(data0)

# Create lagged variables
data <- data %>%
  mutate(
    BTC_Volatility_Lag1 = lag(BTC_Volatility, 1),
    BTC_Volatility_Lag2 = lag(BTC_Volatility, 2),
    J303_Volatility_Lag1 = lag(J303_Volatility, 1),
    J303_Volatility_Lag2 = lag(J303_Volatility, 2)
  )

###############################################################
# Section A: Exploratory Analysis
###############################################################

# 3. Correlation analysis
btc.j303.cor <- cor.test(
  data$BTC_Volatility,
  data$J303_Volatility,
  method = "pearson"
)

correlation.summary <- data.frame(
  Relationship = "Bitcoin vs J303 Volatility",
  Correlation = round(unname(btc.j303.cor$estimate), 4),
  P_Value = signif(btc.j303.cor$p.value, 4)
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
    values = c("Linear" = "blue", "LOESS" = "red")
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

# 5. Estimate regression models
j303.btc.0 <- lm(
  J303_Volatility ~ BTC_Volatility,
  data = data
)

j303.btc.1 <- lm(
  J303_Volatility ~ BTC_Volatility + BTC_Volatility_Lag1,
  data = data
)

j303.btc.2 <- lm(
  J303_Volatility ~
    BTC_Volatility +
    BTC_Volatility_Lag1 +
    BTC_Volatility_Lag2,
  data = data
)

j303.btc.dynamic <- lm(
  J303_Volatility ~
    J303_Volatility_Lag1 +
    J303_Volatility_Lag2 +
    BTC_Volatility +
    BTC_Volatility_Lag1 +
    BTC_Volatility_Lag2,
  data = data
)

models <- list(
  "No Lag" = j303.btc.0,
  "1 Lag" = j303.btc.1,
  "2 Lags" = j303.btc.2,
  "Dynamic 2 Lags" = j303.btc.dynamic
)

# 6. Newey-West inference
run_nw <- function(model) {
  coeftest(
    model,
    vcov = NeweyWest(
      model,
      prewhite = FALSE
    )
  )
}

nw.models <- lapply(models, run_nw)

# 7. Model diagnostics
run_diagnostics <- function(model) {
  list(
    JB = jarque.bera.test(residuals(model)),
    Ljung_Box = Box.test(
      residuals(model),
      lag = 20,
      type = "Ljung-Box"
    ),
    Breusch_Pagan = bptest(model)
  )
}

diagnostics <- lapply(models, run_diagnostics)

# 8. Dynamic model serial correlation
bg.dynamic.2 <- bgtest(
  j303.btc.dynamic,
  order = 2
)

bg.dynamic.20 <- bgtest(
  j303.btc.dynamic,
  order = 20
)

###############################################################
# Section C: Granger Causality
###############################################################

# 9. Granger causality tests
granger.btc.j303 <- grangertest(
  J303_Volatility ~ BTC_Volatility,
  order = 2,
  data = data
)

granger.j303.btc <- grangertest(
  BTC_Volatility ~ J303_Volatility,
  order = 2,
  data = data
)

granger.summary <- data.frame(
  Direction = c(
    "Bitcoin Volatility -> J303 Volatility",
    "J303 Volatility -> Bitcoin Volatility"
  ),
  Lags = 2,
  F_Statistic = c(
    granger.btc.j303$F[2],
    granger.j303.btc$F[2]
  ),
  P_Value = c(
    granger.btc.j303$`Pr(>F)`[2],
    granger.j303.btc$`Pr(>F)`[2]
  )
)

granger.summary <- granger.summary %>%
  mutate(
    F_Statistic = round(F_Statistic, 4),
    P_Value = signif(P_Value, 4)
  )

granger.summary

###############################################################
# Section D: Summary Tables
###############################################################

# 10. Regression summary
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
)

regression.summary <- regression.summary %>%
  mutate(
    across(
      contains("Coefficient"),
      ~ signif(.x, 4)
    ),
    Adj_R2 = round(Adj_R2, 4),
    across(
      contains("pvalue"),
      ~ signif(.x, 4)
    )
  )

rownames(regression.summary) <- NULL

regression.summary

# 11. Diagnostic summary
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
  )
)

diagnostic.summary <- diagnostic.summary %>%
  mutate(
    across(
      contains("P_Value"),
      ~ signif(.x, 4)
    )
  )

rownames(diagnostic.summary) <- NULL

diagnostic.summary

# 12. Dynamic serial correlation summary
dynamic.serial.summary <- data.frame(
  Test = c(
    "Breusch-Godfrey (2 lags)",
    "Breusch-Godfrey (20 lags)"
  ),
  
  Statistic = c(
    unname(bg.dynamic.2$statistic),
    unname(bg.dynamic.20$statistic)
  ),
  
  P_Value = c(
    bg.dynamic.2$p.value,
    bg.dynamic.20$p.value
  )
)

dynamic.serial.summary <- dynamic.serial.summary %>%
  mutate(
    Statistic = round(Statistic, 4),
    P_Value = signif(P_Value, 4)
  )

rownames(dynamic.serial.summary) <- NULL

dynamic.serial.summary






