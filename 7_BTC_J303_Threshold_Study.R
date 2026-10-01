# 1. LOAD PACKAGES

library(ggplot2)
library(lmtest)
library(sandwich)
library(tseries)


# 2. PREPARE DATA

data <- data0


# HELPER FUNCTIONS

add_lags <- function(data, variable) {
  data[[paste0(variable, "_Lag1")]] <- dplyr::lag(
    data[[variable]],
    1
  )
  
  data[[paste0(variable, "_Lag2")]] <- dplyr::lag(
    data[[variable]],
    2
  )
  
  data
}


nw_test <- function(model) {
  coeftest(
    model,
    vcov = NeweyWest(
      model,
      prewhite = FALSE
    )
  )
}


model_diagnostics <- function(model) {
  list(
    JB = jarque.bera.test(residuals(model)),
    BG = bgtest(model, order = 2),
    BP = bptest(model)
  )
}


format_p <- function(p) {
  ifelse(
    p < 2.2e-16,
    "<2.2e-16",
    formatC(
      p,
      format = "g",
      digits = 4
    )
  )
}


static_model <- function(data, btc_lags = 0) {
  
  formula <- switch(
    as.character(btc_lags),
    
    "0" = J303_Volatility ~
      BTC_Volatility,
    
    "1" = J303_Volatility ~
      BTC_Volatility +
      BTC_Volatility_Lag1,
    
    "2" = J303_Volatility ~
      BTC_Volatility +
      BTC_Volatility_Lag1 +
      BTC_Volatility_Lag2
  )
  
  lm(
    formula,
    data = data
  )
}


dynamic_model <- function(data) {
  
  lm(
    J303_Volatility ~
      J303_Volatility_Lag1 +
      J303_Volatility_Lag2 +
      BTC_Volatility +
      BTC_Volatility_Lag1 +
      BTC_Volatility_Lag2,
    data = data
  )
}


dynamic_comparison <- function(
    model.low,
    model.high,
    nw.low,
    nw.high
) {
  
  comparison <- data.frame(
    
    Regime = c(
      "Lower GPR",
      "Elevated GPR"
    ),
    
    J303_Vol_Lag1 = c(
      coef(model.low)["J303_Volatility_Lag1"],
      coef(model.high)["J303_Volatility_Lag1"]
    ),
    
    J303_Vol_Lag1_NW_p = c(
      nw.low[
        "J303_Volatility_Lag1",
        "Pr(>|t|)"
      ],
      nw.high[
        "J303_Volatility_Lag1",
        "Pr(>|t|)"
      ]
    ),
    
    J303_Vol_Lag2 = c(
      coef(model.low)["J303_Volatility_Lag2"],
      coef(model.high)["J303_Volatility_Lag2"]
    ),
    
    J303_Vol_Lag2_NW_p = c(
      nw.low[
        "J303_Volatility_Lag2",
        "Pr(>|t|)"
      ],
      nw.high[
        "J303_Volatility_Lag2",
        "Pr(>|t|)"
      ]
    ),
    
    BTC_Volatility = c(
      coef(model.low)["BTC_Volatility"],
      coef(model.high)["BTC_Volatility"]
    ),
    
    BTC_Volatility_NW_p = c(
      nw.low[
        "BTC_Volatility",
        "Pr(>|t|)"
      ],
      nw.high[
        "BTC_Volatility",
        "Pr(>|t|)"
      ]
    ),
    
    BTC_Volatility_Lag1 = c(
      coef(model.low)["BTC_Volatility_Lag1"],
      coef(model.high)["BTC_Volatility_Lag1"]
    ),
    
    BTC_Volatility_Lag1_NW_p = c(
      nw.low[
        "BTC_Volatility_Lag1",
        "Pr(>|t|)"
      ],
      nw.high[
        "BTC_Volatility_Lag1",
        "Pr(>|t|)"
      ]
    ),
    
    BTC_Volatility_Lag2 = c(
      coef(model.low)["BTC_Volatility_Lag2"],
      coef(model.high)["BTC_Volatility_Lag2"]
    ),
    
    BTC_Volatility_Lag2_NW_p = c(
      nw.low[
        "BTC_Volatility_Lag2",
        "Pr(>|t|)"
      ],
      nw.high[
        "BTC_Volatility_Lag2",
        "Pr(>|t|)"
      ]
    )
  )
  
  comparison[-1] <- lapply(
    comparison[-1],
    signif,
    digits = 4
  )
  
  comparison
}


###############################################################
# Section A: GPRD Regime Analysis
###############################################################

# 3. Create BTC and J303 volatility lags

data <- add_lags(data, "BTC_Volatility")
data <- add_lags(data, "J303_Volatility")


# 4. Split the dataset

low.gpr <- subset(
  data,
  GPR_Regime == "Lower GPR"
)

high.gpr <- subset(
  data,
  GPR_Regime == "Elevated GPR"
)


# 5. Descriptive statistics by regime

regime.summary <- data.frame(
  
  Regime = c(
    "Lower GPR",
    "Elevated GPR"
  ),
  
  Observations = c(
    nrow(low.gpr),
    nrow(high.gpr)
  ),
  
  Mean_GPR = c(
    mean(low.gpr$GPRD),
    mean(high.gpr$GPRD)
  ),
  
  Mean_BTC_Volatility = c(
    mean(low.gpr$BTC_Volatility),
    mean(high.gpr$BTC_Volatility)
  ),
  
  Mean_J303_Volatility = c(
    mean(low.gpr$J303_Volatility),
    mean(high.gpr$J303_Volatility)
  )
  
)

regime.summary[-1] <-
  round(
    regime.summary[-1],
    4
  )

regime.summary


# 6. Correlation analysis by regime

cor.low <- cor.test(
  low.gpr$BTC_Volatility,
  low.gpr$J303_Volatility
)

cor.high <- cor.test(
  high.gpr$BTC_Volatility,
  high.gpr$J303_Volatility
)

cor.low
cor.high


# Correlation summary

correlation.summary <- data.frame(
  
  Regime = c(
    "Lower GPR",
    "Elevated GPR"
  ),
  
  Sample_Size = c(
    nrow(low.gpr),
    nrow(high.gpr)
  ),
  
  Correlation = c(
    unname(cor.low$estimate),
    unname(cor.high$estimate)
  ),
  
  Correlation_P_Value = c(
    cor.low$p.value,
    cor.high$p.value
  )
  
)

correlation.summary$Correlation <-
  round(
    correlation.summary$Correlation,
    4
  )

correlation.summary$Correlation_P_Value <-
  signif(
    correlation.summary$Correlation_P_Value,
    4
  )

correlation.summary


# 7. Scatterplots

ggplot(
  low.gpr,
  aes(
    x = BTC_Volatility,
    y = J303_Volatility
  )
) +
  geom_point(alpha = 0.6) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  geom_smooth(
    method = "loess",
    se = TRUE
  ) +
  theme_minimal() +
  labs(
    title = "J303 Conditional Volatility vs Bitcoin Conditional Volatility (Lower GPR)",
    x = "Bitcoin Conditional Volatility",
    y = "J303 Conditional Volatility"
  )


ggplot(
  high.gpr,
  aes(
    x = BTC_Volatility,
    y = J303_Volatility
  )
) +
  geom_point(alpha = 0.6) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  geom_smooth(
    method = "loess",
    se = TRUE
  ) +
  theme_minimal() +
  labs(
    title = "J303 Conditional Volatility vs Bitcoin Conditional Volatility (Elevated GPR)",
    x = "Bitcoin Conditional Volatility",
    y = "J303 Conditional Volatility"
  )


###############################################################
# Section B: Static Regime Models
###############################################################

# 8. Static regression models

model.low.0 <- static_model(low.gpr, 0)
model.low.1 <- static_model(low.gpr, 1)
model.low.2 <- static_model(low.gpr, 2)

model.high.0 <- static_model(high.gpr, 0)
model.high.1 <- static_model(high.gpr, 1)
model.high.2 <- static_model(high.gpr, 2)


# 9. Static model diagnostics

diag.low.0 <- model_diagnostics(
  model.low.0
)

diag.low.1 <- model_diagnostics(
  model.low.1
)

diag.low.2 <- model_diagnostics(
  model.low.2
)

diag.high.0 <- model_diagnostics(
  model.high.0
)

diag.high.1 <- model_diagnostics(
  model.high.1
)

diag.high.2 <- model_diagnostics(
  model.high.2
)


static.diagnostics <- data.frame(
  
  Regime = c(
    "Lower GPR",
    "Lower GPR",
    "Lower GPR",
    "Elevated GPR",
    "Elevated GPR",
    "Elevated GPR"
  ),
  
  BTC_Lags = c(
    0, 1, 2,
    0, 1, 2
  ),
  
  JB_p = c(
    diag.low.0$JB$p.value,
    diag.low.1$JB$p.value,
    diag.low.2$JB$p.value,
    diag.high.0$JB$p.value,
    diag.high.1$JB$p.value,
    diag.high.2$JB$p.value
  ),
  
  BG_2_p = c(
    diag.low.0$BG$p.value,
    diag.low.1$BG$p.value,
    diag.low.2$BG$p.value,
    diag.high.0$BG$p.value,
    diag.high.1$BG$p.value,
    diag.high.2$BG$p.value
  ),
  
  BP_p = c(
    diag.low.0$BP$p.value,
    diag.low.1$BP$p.value,
    diag.low.2$BP$p.value,
    diag.high.0$BP$p.value,
    diag.high.1$BP$p.value,
    diag.high.2$BP$p.value
  )
  
)

static.diagnostics$JB_p <-
  format_p(
    static.diagnostics$JB_p
  )

static.diagnostics$BG_2_p <-
  format_p(
    static.diagnostics$BG_2_p
  )

static.diagnostics$BP_p <-
  format_p(
    static.diagnostics$BP_p
  )

static.diagnostics


# 10. Newey-West robust inference

nw.low.0 <- nw_test(
  model.low.0
)

nw.low.1 <- nw_test(
  model.low.1
)

nw.low.2 <- nw_test(
  model.low.2
)

nw.high.0 <- nw_test(
  model.high.0
)

nw.high.1 <- nw_test(
  model.high.1
)

nw.high.2 <- nw_test(
  model.high.2
)

nw.low.0
nw.low.1
nw.low.2

nw.high.0
nw.high.1
nw.high.2


###############################################################
# Section C: Dynamic Regime Models
###############################################################

# 11. Dynamic regime models

model.low.dynamic <- dynamic_model(low.gpr)
model.high.dynamic <- dynamic_model(high.gpr)


# 12. Dynamic model diagnostics

diag.low.dynamic <- model_diagnostics(
  model.low.dynamic
)

diag.high.dynamic <- model_diagnostics(
  model.high.dynamic
)

dynamic.diagnostics <- data.frame(
  
  Regime = c(
    "Lower GPR",
    "Elevated GPR"
  ),
  
  JB_p = c(
    diag.low.dynamic$JB$p.value,
    diag.high.dynamic$JB$p.value
  ),
  
  BG_2_p = c(
    diag.low.dynamic$BG$p.value,
    diag.high.dynamic$BG$p.value
  ),
  
  BP_p = c(
    diag.low.dynamic$BP$p.value,
    diag.high.dynamic$BP$p.value
  )
  
)

dynamic.diagnostics$JB_p <-
  format_p(
    dynamic.diagnostics$JB_p
  )

dynamic.diagnostics$BG_2_p <-
  format_p(
    dynamic.diagnostics$BG_2_p
  )

dynamic.diagnostics$BP_p <-
  format_p(
    dynamic.diagnostics$BP_p
  )

dynamic.diagnostics


# 13. Newey-West robust inference

nw.low.dynamic <- nw_test(
  model.low.dynamic
)

nw.high.dynamic <- nw_test(
  model.high.dynamic
)

nw.low.dynamic
nw.high.dynamic


# 14. Dynamic model comparison

dynamic.comparison <- dynamic_comparison(
  model.low.dynamic,
  model.high.dynamic,
  nw.low.dynamic,
  nw.high.dynamic
)

dynamic.comparison


###############################################################
# Section D: Dynamic GPRD Interaction Model
###############################################################

# 15. Dynamic interaction model

interaction.model <- lm(
  J303_Volatility ~
    J303_Volatility_Lag1 +
    J303_Volatility_Lag2 +
    BTC_Volatility *
    GPR_Regime +
    BTC_Volatility_Lag1 +
    BTC_Volatility_Lag2,
  data = data
)


# 16. Newey-West robust inference

interaction.nw <- nw_test(
  interaction.model
)

interaction.nw


# 17. Interaction model diagnostics

interaction.diagnostics <- model_diagnostics(
  interaction.model
)

interaction.diagnostics$JB
interaction.diagnostics$BG
interaction.diagnostics$BP


# 18. Dynamic interaction summary

interaction.summary <- data.frame(
  
  Variable = c(
    "BTC Volatility",
    "Elevated GPR",
    "BTC Volatility × Elevated GPR"
  ),
  
  Estimate = c(
    interaction.nw[
      "BTC_Volatility",
      "Estimate"
    ],
    interaction.nw[
      "GPR_RegimeElevated GPR",
      "Estimate"
    ],
    interaction.nw[
      "BTC_Volatility:GPR_RegimeElevated GPR",
      "Estimate"
    ]
  ),
  
  Robust_SE = c(
    interaction.nw[
      "BTC_Volatility",
      "Std. Error"
    ],
    interaction.nw[
      "GPR_RegimeElevated GPR",
      "Std. Error"
    ],
    interaction.nw[
      "BTC_Volatility:GPR_RegimeElevated GPR",
      "Std. Error"
    ]
  ),
  
  P_Value = c(
    interaction.nw[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    interaction.nw[
      "GPR_RegimeElevated GPR",
      "Pr(>|t|)"
    ],
    interaction.nw[
      "BTC_Volatility:GPR_RegimeElevated GPR",
      "Pr(>|t|)"
    ]
  )
  
)

interaction.summary[-1] <- lapply(
  interaction.summary[-1],
  signif,
  digits = 4
)

interaction.summary


# 19. Fully interacted dynamic model

interaction.lag.model <- lm(
  J303_Volatility ~
    J303_Volatility_Lag1 +
    J303_Volatility_Lag2 +
    BTC_Volatility *
    GPR_Regime +
    BTC_Volatility_Lag1 *
    GPR_Regime +
    BTC_Volatility_Lag2 *
    GPR_Regime,
  data = data
)


# 20. Newey-West robust inference

interaction.lag.nw <- nw_test(
  interaction.lag.model
)

interaction.lag.nw


# 21. Interaction model diagnostics

interaction.lag.diagnostics <- model_diagnostics(
  interaction.lag.model
)

interaction.lag.diagnostics$JB
interaction.lag.diagnostics$BG
interaction.lag.diagnostics$BP


# 22. Interaction model summary

interaction.lag.summary <- data.frame(
  
  Variable = rownames(
    interaction.lag.nw
  ),
  
  Estimate = interaction.lag.nw[
    ,
    "Estimate"
  ],
  
  Robust_SE = interaction.lag.nw[
    ,
    "Std. Error"
  ],
  
  P_Value = interaction.lag.nw[
    ,
    "Pr(>|t|)"
  ]
  
)

interaction.lag.summary[-1] <- lapply(
  interaction.lag.summary[-1],
  signif,
  digits = 4
)

interaction.lag.summary


###############################################################
# Section E: Alternative GPR Measures
###############################################################

# 23. ACT regime splits

low.act <- subset(
  data,
  GPRD_ACT_Regime == "Lower GPR"
)

high.act <- subset(
  data,
  GPRD_ACT_Regime == "Elevated GPR"
)


# 24. ACT dynamic regime models

model.low.act.dynamic <- dynamic_model(low.act)
model.high.act.dynamic <- dynamic_model(high.act)


# 25. ACT diagnostics

diag.low.act.dynamic <- model_diagnostics(
  model.low.act.dynamic
)

diag.high.act.dynamic <- model_diagnostics(
  model.high.act.dynamic
)

act.dynamic.diagnostics <- data.frame(
  
  Regime = c(
    "Lower GPR",
    "Elevated GPR"
  ),
  
  JB_p = c(
    diag.low.act.dynamic$JB$p.value,
    diag.high.act.dynamic$JB$p.value
  ),
  
  BG_2_p = c(
    diag.low.act.dynamic$BG$p.value,
    diag.high.act.dynamic$BG$p.value
  ),
  
  BP_p = c(
    diag.low.act.dynamic$BP$p.value,
    diag.high.act.dynamic$BP$p.value
  )
  
)

act.dynamic.diagnostics$JB_p <-
  format_p(
    act.dynamic.diagnostics$JB_p
  )

act.dynamic.diagnostics$BG_2_p <-
  format_p(
    act.dynamic.diagnostics$BG_2_p
  )

act.dynamic.diagnostics$BP_p <-
  format_p(
    act.dynamic.diagnostics$BP_p
  )

act.dynamic.diagnostics


# 26. ACT Newey-West inference

nw.low.act.dynamic <- nw_test(
  model.low.act.dynamic
)

nw.high.act.dynamic <- nw_test(
  model.high.act.dynamic
)

nw.low.act.dynamic
nw.high.act.dynamic


# 27. ACT dynamic comparison

act.dynamic.comparison <- dynamic_comparison(
  model.low.act.dynamic,
  model.high.act.dynamic,
  nw.low.act.dynamic,
  nw.high.act.dynamic
)

act.dynamic.comparison


# 28. THREAT regime splits

low.threat <- subset(
  data,
  GPRD_THREAT_Regime == "Lower GPR"
)

high.threat <- subset(
  data,
  GPRD_THREAT_Regime == "Elevated GPR"
)


# 29. THREAT dynamic regime models

model.low.threat.dynamic <- dynamic_model(low.threat)
model.high.threat.dynamic <- dynamic_model(high.threat)


# 30. THREAT diagnostics

diag.low.threat.dynamic <- model_diagnostics(
  model.low.threat.dynamic
)

diag.high.threat.dynamic <- model_diagnostics(
  model.high.threat.dynamic
)

threat.dynamic.diagnostics <- data.frame(
  
  Regime = c(
    "Lower GPR",
    "Elevated GPR"
  ),
  
  JB_p = c(
    diag.low.threat.dynamic$JB$p.value,
    diag.high.threat.dynamic$JB$p.value
  ),
  
  BG_2_p = c(
    diag.low.threat.dynamic$BG$p.value,
    diag.high.threat.dynamic$BG$p.value
  ),
  
  BP_p = c(
    diag.low.threat.dynamic$BP$p.value,
    diag.high.threat.dynamic$BP$p.value
  )
  
)

threat.dynamic.diagnostics$JB_p <-
  format_p(
    threat.dynamic.diagnostics$JB_p
  )

threat.dynamic.diagnostics$BG_2_p <-
  format_p(
    threat.dynamic.diagnostics$BG_2_p
  )

threat.dynamic.diagnostics$BP_p <-
  format_p(
    threat.dynamic.diagnostics$BP_p
  )

threat.dynamic.diagnostics


# 31. THREAT Newey-West inference

nw.low.threat.dynamic <- nw_test(
  model.low.threat.dynamic
)

nw.high.threat.dynamic <- nw_test(
  model.high.threat.dynamic
)

nw.low.threat.dynamic
nw.high.threat.dynamic


# 32. THREAT dynamic comparison

threat.dynamic.comparison <- dynamic_comparison(
  model.low.threat.dynamic,
  model.high.threat.dynamic,
  nw.low.threat.dynamic,
  nw.high.threat.dynamic
)

threat.dynamic.comparison


###############################################################
# Section F: Alternative GPR Interaction Models
###############################################################

# 33. ACT dynamic interaction model

act.interaction.model <- lm(
  J303_Volatility ~
    J303_Volatility_Lag1 +
    J303_Volatility_Lag2 +
    BTC_Volatility * GPRD_ACT_Regime +
    BTC_Volatility_Lag1 +
    BTC_Volatility_Lag2,
  data = data
)


# 34. ACT Newey-West inference

nw.act.interaction <- nw_test(
  act.interaction.model
)

nw.act.interaction


# 35. ACT interaction diagnostics

diag.act.interaction <- model_diagnostics(
  act.interaction.model
)

act.interaction.diagnostics <- data.frame(
  
  JB_p = diag.act.interaction$JB$p.value,
  BG_2_p = diag.act.interaction$BG$p.value,
  BP_p = diag.act.interaction$BP$p.value
  
)

act.interaction.diagnostics$JB_p <-
  format_p(
    act.interaction.diagnostics$JB_p
  )

act.interaction.diagnostics$BG_2_p <-
  format_p(
    act.interaction.diagnostics$BG_2_p
  )

act.interaction.diagnostics$BP_p <-
  format_p(
    act.interaction.diagnostics$BP_p
  )

act.interaction.diagnostics


# 36. ACT interaction summary

act.interaction.summary <- data.frame(
  
  Variable = c(
    "BTC Volatility",
    "Elevated GPR",
    "BTC Volatility × Elevated GPR"
  ),
  
  Estimate = c(
    coef(act.interaction.model)[
      "BTC_Volatility"
    ],
    coef(act.interaction.model)[
      "GPRD_ACT_RegimeElevated GPR"
    ],
    coef(act.interaction.model)[
      "BTC_Volatility:GPRD_ACT_RegimeElevated GPR"
    ]
  ),
  
  Robust_SE = c(
    nw.act.interaction[
      "BTC_Volatility",
      "Std. Error"
    ],
    nw.act.interaction[
      "GPRD_ACT_RegimeElevated GPR",
      "Std. Error"
    ],
    nw.act.interaction[
      "BTC_Volatility:GPRD_ACT_RegimeElevated GPR",
      "Std. Error"
    ]
  ),
  
  P_Value = c(
    nw.act.interaction[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    nw.act.interaction[
      "GPRD_ACT_RegimeElevated GPR",
      "Pr(>|t|)"
    ],
    nw.act.interaction[
      "BTC_Volatility:GPRD_ACT_RegimeElevated GPR",
      "Pr(>|t|)"
    ]
  )
  
)

act.interaction.summary[-1] <- lapply(
  act.interaction.summary[-1],
  signif,
  digits = 4
)

act.interaction.summary


# 37. THREAT dynamic interaction model

threat.interaction.model <- lm(
  J303_Volatility ~
    J303_Volatility_Lag1 +
    J303_Volatility_Lag2 +
    BTC_Volatility * GPRD_THREAT_Regime +
    BTC_Volatility_Lag1 +
    BTC_Volatility_Lag2,
  data = data
)


# 38. THREAT Newey-West inference

nw.threat.interaction <- nw_test(
  threat.interaction.model
)

nw.threat.interaction


# 39. THREAT interaction diagnostics

diag.threat.interaction <- model_diagnostics(
  threat.interaction.model
)

threat.interaction.diagnostics <- data.frame(
  
  JB_p = diag.threat.interaction$JB$p.value,
  BG_2_p = diag.threat.interaction$BG$p.value,
  BP_p = diag.threat.interaction$BP$p.value
  
)

threat.interaction.diagnostics$JB_p <-
  format_p(
    threat.interaction.diagnostics$JB_p
  )

threat.interaction.diagnostics$BG_2_p <-
  format_p(
    threat.interaction.diagnostics$BG_2_p
  )

threat.interaction.diagnostics$BP_p <-
  format_p(
    threat.interaction.diagnostics$BP_p
  )

threat.interaction.diagnostics


# 40. THREAT interaction summary

threat.interaction.summary <- data.frame(
  
  Variable = c(
    "BTC Volatility",
    "Elevated GPR",
    "BTC Volatility × Elevated GPR"
  ),
  
  Estimate = c(
    coef(threat.interaction.model)[
      "BTC_Volatility"
    ],
    coef(threat.interaction.model)[
      "GPRD_THREAT_RegimeElevated GPR"
    ],
    coef(threat.interaction.model)[
      "BTC_Volatility:GPRD_THREAT_RegimeElevated GPR"
    ]
  ),
  
  Robust_SE = c(
    nw.threat.interaction[
      "BTC_Volatility",
      "Std. Error"
    ],
    nw.threat.interaction[
      "GPRD_THREAT_RegimeElevated GPR",
      "Std. Error"
    ],
    nw.threat.interaction[
      "BTC_Volatility:GPRD_THREAT_RegimeElevated GPR",
      "Std. Error"
    ]
  ),
  
  P_Value = c(
    nw.threat.interaction[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    nw.threat.interaction[
      "GPRD_THREAT_RegimeElevated GPR",
      "Pr(>|t|)"
    ],
    nw.threat.interaction[
      "BTC_Volatility:GPRD_THREAT_RegimeElevated GPR",
      "Pr(>|t|)"
    ]
  )
  
)

threat.interaction.summary[-1] <- lapply(
  threat.interaction.summary[-1],
  signif,
  digits = 4
)

threat.interaction.summary


###############################################################
# Section G: Post-2017 ACT Robustness Check
###############################################################

# 41. Create post-2017 sample

post2017.data <- subset(
  data,
  Date >= as.Date("2018-01-01")
)


# 42. Post-2017 ACT dynamic interaction model

post2017.act.interaction <- lm(
  J303_Volatility ~
    J303_Volatility_Lag1 +
    J303_Volatility_Lag2 +
    BTC_Volatility * GPRD_ACT_Regime +
    BTC_Volatility_Lag1 +
    BTC_Volatility_Lag2,
  data = post2017.data
)


# 43. Post-2017 ACT Newey-West inference

post2017.act.interaction.nw <- nw_test(
  post2017.act.interaction
)

post2017.act.interaction.nw


# 44. Post-2017 ACT interaction summary

post2017.act.interaction.summary <- data.frame(
  
  Variable = c(
    "BTC Volatility",
    "Elevated GPR",
    "BTC Volatility × Elevated GPR"
  ),
  
  Estimate = c(
    post2017.act.interaction.nw[
      "BTC_Volatility",
      "Estimate"
    ],
    post2017.act.interaction.nw[
      "GPRD_ACT_RegimeElevated GPR",
      "Estimate"
    ],
    post2017.act.interaction.nw[
      "BTC_Volatility:GPRD_ACT_RegimeElevated GPR",
      "Estimate"
    ]
  ),
  
  Robust_SE = c(
    post2017.act.interaction.nw[
      "BTC_Volatility",
      "Std. Error"
    ],
    post2017.act.interaction.nw[
      "GPRD_ACT_RegimeElevated GPR",
      "Std. Error"
    ],
    post2017.act.interaction.nw[
      "BTC_Volatility:GPRD_ACT_RegimeElevated GPR",
      "Std. Error"
    ]
  ),
  
  P_Value = c(
    post2017.act.interaction.nw[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    post2017.act.interaction.nw[
      "GPRD_ACT_RegimeElevated GPR",
      "Pr(>|t|)"
    ],
    post2017.act.interaction.nw[
      "BTC_Volatility:GPRD_ACT_RegimeElevated GPR",
      "Pr(>|t|)"
    ]
  )
  
)

post2017.act.interaction.summary$Estimate <-
  signif(
    post2017.act.interaction.summary$Estimate,
    4
  )

post2017.act.interaction.summary$Robust_SE <-
  signif(
    post2017.act.interaction.summary$Robust_SE,
    4
  )

post2017.act.interaction.summary$P_Value <-
  format_p(
    post2017.act.interaction.summary$P_Value
  )

post2017.act.interaction.summary


###############################################################
# Section H: Final Results
###############################################################

# 45. Dynamic regime results

dynamic.comparison
act.dynamic.comparison
threat.dynamic.comparison


# 46. GPRD interaction results

interaction.summary


# 47. ACT interaction results

act.interaction.summary


# 48. THREAT interaction results

threat.interaction.summary

