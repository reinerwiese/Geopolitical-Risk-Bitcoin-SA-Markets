# 1. LOAD PACKAGES

library(ggplot2)
library(lmtest)
library(sandwich)
library(tseries)


# 2. PREPARE DATA

data <- data0


# HELPER FUNCTIONS

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
    JB = jarque.bera.test(
      residuals(model)
    ),
    BG = bgtest(
      model,
      order = 2
    ),
    BP = bptest(
      model
    )
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


###############################################################
# Section A: GPRD Regime Analysis
###############################################################

# 3. Create GPRD and BTC volatility lags

data$GPRD_Lag1 <- dplyr::lag(
  data$GPRD,
  1
)

data$GPRD_Lag2 <- dplyr::lag(
  data$GPRD,
  2
)

data$BTC_Volatility_Lag1 <- dplyr::lag(
  data$BTC_Volatility,
  1
)

data$BTC_Volatility_Lag2 <- dplyr::lag(
  data$BTC_Volatility,
  2
)


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
  
  Mean_BTC_Volatility = c(
    mean(low.gpr$BTC_Volatility),
    mean(high.gpr$BTC_Volatility)
  ),
  
  Mean_GPR = c(
    mean(low.gpr$GPRD),
    mean(high.gpr$GPRD)
  ),
  
  Observations = c(
    nrow(low.gpr),
    nrow(high.gpr)
  )
  
)

regime.summary$Mean_BTC_Volatility <-
  round(regime.summary$Mean_BTC_Volatility, 4)

regime.summary$Mean_GPR <-
  round(regime.summary$Mean_GPR, 4)

regime.summary


# 6. Scatterplots

ggplot(
  low.gpr,
  aes(
    x = GPRD,
    y = BTC_Volatility
  )
) +
  geom_point(alpha = 0.6) +
  geom_smooth(
    method = "lm",
    colour = "blue",
    se = TRUE
  ) +
  geom_smooth(
    method = "loess",
    se = TRUE
  ) +
  theme_minimal() +
  labs(
    title = "Bitcoin Conditional Volatility vs Geopolitical Risk (Lower GPR)",
    x = "Geopolitical Risk Index",
    y = "Conditional Volatility"
  )


ggplot(
  high.gpr,
  aes(
    x = GPRD,
    y = BTC_Volatility
  )
) +
  geom_point(alpha = 0.6) +
  geom_smooth(
    method = "lm",
    colour = "blue",
    se = TRUE
  ) +
  geom_smooth(
    method = "loess",
    se = TRUE
  ) +
  theme_minimal() +
  labs(
    title = "Bitcoin Conditional Volatility vs Geopolitical Risk (Elevated GPR)",
    x = "Geopolitical Risk Index",
    y = "Conditional Volatility"
  )


# 7. Static regression models

model.low.0 <- lm(
  BTC_Volatility ~ GPRD,
  data = low.gpr
)

model.low.1 <- lm(
  BTC_Volatility ~ GPRD + GPRD_Lag1,
  data = low.gpr
)

model.low.2 <- lm(
  BTC_Volatility ~ GPRD + GPRD_Lag1 + GPRD_Lag2,
  data = low.gpr
)

model.high.0 <- lm(
  BTC_Volatility ~ GPRD,
  data = high.gpr
)

model.high.1 <- lm(
  BTC_Volatility ~ GPRD + GPRD_Lag1,
  data = high.gpr
)

model.high.2 <- lm(
  BTC_Volatility ~ GPRD + GPRD_Lag1 + GPRD_Lag2,
  data = high.gpr
)


# 8. Static model diagnostics

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


# Static diagnostic summary

static.diagnostics <- data.frame(
  
  Regime = c(
    "Lower GPR",
    "Lower GPR",
    "Lower GPR",
    "Elevated GPR",
    "Elevated GPR",
    "Elevated GPR"
  ),
  
  GPR_Lags = c(
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


# 9. Newey-West robust inference

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


# 10. Dynamic regime models

model.low.dynamic <- lm(
  BTC_Volatility ~
    BTC_Volatility_Lag1 +
    BTC_Volatility_Lag2 +
    GPRD +
    GPRD_Lag1 +
    GPRD_Lag2,
  data = low.gpr
)

model.high.dynamic <- lm(
  BTC_Volatility ~
    BTC_Volatility_Lag1 +
    BTC_Volatility_Lag2 +
    GPRD +
    GPRD_Lag1 +
    GPRD_Lag2,
  data = high.gpr
)

# 10a. Restricted models: Bitcoin volatility persistence only

restricted.low.dynamic <- lm(
  BTC_Volatility ~
    BTC_Volatility_Lag1 +
    BTC_Volatility_Lag2,
  data = low.gpr
)

restricted.high.dynamic <- lm(
  BTC_Volatility ~
    BTC_Volatility_Lag1 +
    BTC_Volatility_Lag2,
  data = high.gpr
)

# 10b. Incremental contribution of GPR

r2.restricted.low <- summary(
  restricted.low.dynamic
)$r.squared

r2.full.low <- summary(
  model.low.dynamic
)$r.squared

delta.r2.low <- r2.full.low - r2.restricted.low

r2.restricted.high <- summary(
  restricted.high.dynamic
)$r.squared

r2.full.high <- summary(
  model.high.dynamic
)$r.squared

delta.r2.high <- r2.full.high - r2.restricted.high

dynamic.incremental.r2 <- data.frame(
  Regime = c(
    "Lower GPR",
    "Elevated GPR"
  ),
  Restricted_R2 = c(
    r2.restricted.low,
    r2.restricted.high
  ),
  Full_R2 = c(
    r2.full.low,
    r2.full.high
  ),
  Delta_R2 = c(
    delta.r2.low,
    delta.r2.high
  )
)

dynamic.incremental.r2[-1] <-
  round(
    dynamic.incremental.r2[-1],
    4
  )

dynamic.incremental.r2

# 11. Dynamic model diagnostics

diag.low.dynamic <- model_diagnostics(
  model.low.dynamic
)

diag.high.dynamic <- model_diagnostics(
  model.high.dynamic
)


# Dynamic diagnostic summary

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


# 12. Newey-West robust inference for dynamic models

nw.low.dynamic <- nw_test(
  model.low.dynamic
)

nw.high.dynamic <- nw_test(
  model.high.dynamic
)

nw.low.dynamic
nw.high.dynamic


# 13. Dynamic model comparison

dynamic.comparison <- data.frame(
  
  Regime = c(
    "Lower GPR",
    "Elevated GPR"
  ),
  
  BTC_Vol_Lag1 = c(
    coef(model.low.dynamic)["BTC_Volatility_Lag1"],
    coef(model.high.dynamic)["BTC_Volatility_Lag1"]
  ),
  
  BTC_Vol_Lag1_NW_p = c(
    nw.low.dynamic[
      "BTC_Volatility_Lag1",
      "Pr(>|t|)"
    ],
    nw.high.dynamic[
      "BTC_Volatility_Lag1",
      "Pr(>|t|)"
    ]
  ),
  
  GPRD = c(
    coef(model.low.dynamic)["GPRD"],
    coef(model.high.dynamic)["GPRD"]
  ),
  
  GPRD_NW_p = c(
    nw.low.dynamic[
      "GPRD",
      "Pr(>|t|)"
    ],
    nw.high.dynamic[
      "GPRD",
      "Pr(>|t|)"
    ]
  ),
  
  GPRD_Lag1 = c(
    coef(model.low.dynamic)["GPRD_Lag1"],
    coef(model.high.dynamic)["GPRD_Lag1"]
  ),
  
  GPRD_Lag1_NW_p = c(
    nw.low.dynamic[
      "GPRD_Lag1",
      "Pr(>|t|)"
    ],
    nw.high.dynamic[
      "GPRD_Lag1",
      "Pr(>|t|)"
    ]
  ),
  
  GPRD_Lag2 = c(
    coef(model.low.dynamic)["GPRD_Lag2"],
    coef(model.high.dynamic)["GPRD_Lag2"]
  ),
  
  GPRD_Lag2_NW_p = c(
    nw.low.dynamic[
      "GPRD_Lag2",
      "Pr(>|t|)"
    ],
    nw.high.dynamic[
      "GPRD_Lag2",
      "Pr(>|t|)"
    ]
  )
  
)

dynamic.comparison$BTC_Vol_Lag1 <-
  signif(
    dynamic.comparison$BTC_Vol_Lag1,
    4
  )

dynamic.comparison$BTC_Vol_Lag1_NW_p <-
  signif(
    dynamic.comparison$BTC_Vol_Lag1_NW_p,
    4
  )

dynamic.comparison$GPRD <-
  signif(
    dynamic.comparison$GPRD,
    4
  )

dynamic.comparison$GPRD_NW_p <-
  signif(
    dynamic.comparison$GPRD_NW_p,
    4
  )

dynamic.comparison$GPRD_Lag1 <-
  signif(
    dynamic.comparison$GPRD_Lag1,
    4
  )

dynamic.comparison$GPRD_Lag1_NW_p <-
  signif(
    dynamic.comparison$GPRD_Lag1_NW_p,
    4
  )

dynamic.comparison$GPRD_Lag2 <-
  signif(
    dynamic.comparison$GPRD_Lag2,
    4
  )

dynamic.comparison$GPRD_Lag2_NW_p <-
  signif(
    dynamic.comparison$GPRD_Lag2_NW_p,
    4
  )

dynamic.comparison


# 14. GPRD interaction model

interaction.model <- lm(
  BTC_Volatility ~ GPRD * GPR_Regime,
  data = data
)


# 15. Newey-West robust interaction model

interaction.nw <- nw_test(
  interaction.model
)

interaction.nw


# 16. Interaction model summary

interaction.summary <- data.frame(
  
  Variable = c(
    "GPR",
    "Elevated GPR",
    "GPR × Elevated GPR"
  ),
  
  Estimate = c(
    interaction.nw[
      "GPRD",
      "Estimate"
    ],
    interaction.nw[
      "GPR_RegimeElevated GPR",
      "Estimate"
    ],
    interaction.nw[
      "GPRD:GPR_RegimeElevated GPR",
      "Estimate"
    ]
  ),
  
  Robust_SE = c(
    interaction.nw[
      "GPRD",
      "Std. Error"
    ],
    interaction.nw[
      "GPR_RegimeElevated GPR",
      "Std. Error"
    ],
    interaction.nw[
      "GPRD:GPR_RegimeElevated GPR",
      "Std. Error"
    ]
  ),
  
  P_Value = c(
    interaction.nw[
      "GPRD",
      "Pr(>|t|)"
    ],
    interaction.nw[
      "GPR_RegimeElevated GPR",
      "Pr(>|t|)"
    ],
    interaction.nw[
      "GPRD:GPR_RegimeElevated GPR",
      "Pr(>|t|)"
    ]
  )
  
)

interaction.summary$Estimate <-
  signif(
    interaction.summary$Estimate,
    4
  )

interaction.summary$Robust_SE <-
  signif(
    interaction.summary$Robust_SE,
    4
  )

interaction.summary$P_Value <-
  signif(
    interaction.summary$P_Value,
    4
  )

interaction.summary

###############################################################
# Section B: Alternative GPR Regime Analysis
###############################################################

# 20. Create alternative GPR lags

data$GPRD_ACT_Lag1 <- dplyr::lag(data$GPRD_ACT, 1)
data$GPRD_ACT_Lag2 <- dplyr::lag(data$GPRD_ACT, 2)

data$GPRD_THREAT_Lag1 <- dplyr::lag(data$GPRD_THREAT, 1)
data$GPRD_THREAT_Lag2 <- dplyr::lag(data$GPRD_THREAT, 2)


# 21. GPRD_ACT regime splits

low.act <- subset(
  data,
  GPRD_ACT_Regime == "Lower GPR"
)

high.act <- subset(
  data,
  GPRD_ACT_Regime == "Elevated GPR"
)


# 22. GPRD_ACT dynamic models

model.low.act.dynamic <- lm(
  BTC_Volatility ~
    BTC_Volatility_Lag1 +
    BTC_Volatility_Lag2 +
    GPRD_ACT +
    GPRD_ACT_Lag1 +
    GPRD_ACT_Lag2,
  data = low.act
)

model.high.act.dynamic <- lm(
  BTC_Volatility ~
    BTC_Volatility_Lag1 +
    BTC_Volatility_Lag2 +
    GPRD_ACT +
    GPRD_ACT_Lag1 +
    GPRD_ACT_Lag2,
  data = high.act
)

# 22a. ACT restricted models: Bitcoin persistence only

restricted.low.act.dynamic <- lm(
  BTC_Volatility ~
    BTC_Volatility_Lag1 +
    BTC_Volatility_Lag2,
  data = low.act
)

restricted.high.act.dynamic <- lm(
  BTC_Volatility ~
    BTC_Volatility_Lag1 +
    BTC_Volatility_Lag2,
  data = high.act
)

# 22b. ACT incremental GPR contribution

act.r2.restricted <- c(
  summary(restricted.low.act.dynamic)$r.squared,
  summary(restricted.high.act.dynamic)$r.squared
)

act.r2.full <- c(
  summary(model.low.act.dynamic)$r.squared,
  summary(model.high.act.dynamic)$r.squared
)

act.delta.r2 <- act.r2.full - act.r2.restricted

act.incremental.r2 <- data.frame(
  Regime = c(
    "Lower GPR",
    "Elevated GPR"
  ),
  Restricted_R2 = act.r2.restricted,
  Full_R2 = act.r2.full,
  Delta_R2 = act.delta.r2
)

act.incremental.r2[-1] <-
  round(
    act.incremental.r2[-1],
    4
  )

act.incremental.r2


# 23. GPRD_ACT diagnostics

summary(model.low.act.dynamic)
summary(model.high.act.dynamic)

diag.low.act.dynamic <- model_diagnostics(
  model.low.act.dynamic
)

diag.high.act.dynamic <- model_diagnostics(
  model.high.act.dynamic
)

diag.low.act.dynamic$JB
diag.high.act.dynamic$JB

diag.low.act.dynamic$BG
diag.high.act.dynamic$BG

diag.low.act.dynamic$BP
diag.high.act.dynamic$BP


# 24. GPRD_ACT Newey-West inference

nw.low.act.dynamic <- nw_test(
  model.low.act.dynamic
)

nw.high.act.dynamic <- nw_test(
  model.high.act.dynamic
)

nw.low.act.dynamic
nw.high.act.dynamic


# 25. GPRD_ACT dynamic comparison

act.dynamic.comparison <- data.frame(
  
  Regime = c(
    "Lower GPR",
    "Elevated GPR"
  ),
  
  BTC_Vol_Lag1 = c(
    coef(model.low.act.dynamic)["BTC_Volatility_Lag1"],
    coef(model.high.act.dynamic)["BTC_Volatility_Lag1"]
  ),
  
  BTC_Vol_Lag2 = c(
    coef(model.low.act.dynamic)["BTC_Volatility_Lag2"],
    coef(model.high.act.dynamic)["BTC_Volatility_Lag2"]
  ),
  
  GPR_Lag0 = c(
    coef(model.low.act.dynamic)["GPRD_ACT"],
    coef(model.high.act.dynamic)["GPRD_ACT"]
  ),
  
  GPR_Lag1 = c(
    coef(model.low.act.dynamic)["GPRD_ACT_Lag1"],
    coef(model.high.act.dynamic)["GPRD_ACT_Lag1"]
  ),
  
  GPR_Lag2 = c(
    coef(model.low.act.dynamic)["GPRD_ACT_Lag2"],
    coef(model.high.act.dynamic)["GPRD_ACT_Lag2"]
  ),
  
  Adj_R2 = c(
    summary(model.low.act.dynamic)$adj.r.squared,
    summary(model.high.act.dynamic)$adj.r.squared
  ),
  
  Residual_SE = c(
    summary(model.low.act.dynamic)$sigma,
    summary(model.high.act.dynamic)$sigma
  ),
  
  GPR_Lag0_NW_p = c(
    nw.low.act.dynamic["GPRD_ACT", "Pr(>|t|)"],
    nw.high.act.dynamic["GPRD_ACT", "Pr(>|t|)"]
  ),
  
  GPR_Lag1_NW_p = c(
    nw.low.act.dynamic["GPRD_ACT_Lag1", "Pr(>|t|)"],
    nw.high.act.dynamic["GPRD_ACT_Lag1", "Pr(>|t|)"]
  ),
  
  GPR_Lag2_NW_p = c(
    nw.low.act.dynamic["GPRD_ACT_Lag2", "Pr(>|t|)"],
    nw.high.act.dynamic["GPRD_ACT_Lag2", "Pr(>|t|)"]
  )
)

act.dynamic.comparison


# 26. GPRD_ACT interaction model

interaction.act.model <- lm(
  BTC_Volatility ~ GPRD_ACT * GPRD_ACT_Regime,
  data = data
)

interaction.act.nw <- nw_test(
  interaction.act.model
)

interaction.act.nw


# 27. GPRD_THREAT regime splits

low.threat <- subset(
  data,
  GPRD_THREAT_Regime == "Lower GPR"
)

high.threat <- subset(
  data,
  GPRD_THREAT_Regime == "Elevated GPR"
)


# 28. GPRD_THREAT dynamic models

model.low.threat.dynamic <- lm(
  BTC_Volatility ~
    BTC_Volatility_Lag1 +
    BTC_Volatility_Lag2 +
    GPRD_THREAT +
    GPRD_THREAT_Lag1 +
    GPRD_THREAT_Lag2,
  data = low.threat
)

model.high.threat.dynamic <- lm(
  BTC_Volatility ~
    BTC_Volatility_Lag1 +
    BTC_Volatility_Lag2 +
    GPRD_THREAT +
    GPRD_THREAT_Lag1 +
    GPRD_THREAT_Lag2,
  data = high.threat
)

# 29a. THREAT restricted models: Bitcoin persistence only

restricted.low.threat.dynamic <- lm(
  BTC_Volatility ~
    BTC_Volatility_Lag1 +
    BTC_Volatility_Lag2,
  data = low.threat
)

restricted.high.threat.dynamic <- lm(
  BTC_Volatility ~
    BTC_Volatility_Lag1 +
    BTC_Volatility_Lag2,
  data = high.threat
)

# 29b. THREAT incremental GPR contribution

threat.r2.restricted <- c(
  summary(restricted.low.threat.dynamic)$r.squared,
  summary(restricted.high.threat.dynamic)$r.squared
)

threat.r2.full <- c(
  summary(model.low.threat.dynamic)$r.squared,
  summary(model.high.threat.dynamic)$r.squared
)

threat.delta.r2 <- threat.r2.full - threat.r2.restricted

threat.incremental.r2 <- data.frame(
  Regime = c(
    "Lower GPR",
    "Elevated GPR"
  ),
  Restricted_R2 = threat.r2.restricted,
  Full_R2 = threat.r2.full,
  Delta_R2 = threat.delta.r2
)

threat.incremental.r2[-1] <-
  round(
    threat.incremental.r2[-1],
    4
  )

threat.incremental.r2

# 29. GPRD_THREAT diagnostics

summary(model.low.threat.dynamic)
summary(model.high.threat.dynamic)

diag.low.threat.dynamic <- model_diagnostics(
  model.low.threat.dynamic
)

diag.high.threat.dynamic <- model_diagnostics(
  model.high.threat.dynamic
)

diag.low.threat.dynamic$JB
diag.high.threat.dynamic$JB

diag.low.threat.dynamic$BG
diag.high.threat.dynamic$BG

diag.low.threat.dynamic$BP
diag.high.threat.dynamic$BP


# 30. GPRD_THREAT Newey-West inference

nw.low.threat.dynamic <- nw_test(
  model.low.threat.dynamic
)

nw.high.threat.dynamic <- nw_test(
  model.high.threat.dynamic
)

nw.low.threat.dynamic
nw.high.threat.dynamic


# 31. GPRD_THREAT dynamic comparison

threat.dynamic.comparison <- data.frame(
  
  Regime = c(
    "Lower GPR",
    "Elevated GPR"
  ),
  
  BTC_Vol_Lag1 = c(
    coef(model.low.threat.dynamic)["BTC_Volatility_Lag1"],
    coef(model.high.threat.dynamic)["BTC_Volatility_Lag1"]
  ),
  
  BTC_Vol_Lag2 = c(
    coef(model.low.threat.dynamic)["BTC_Volatility_Lag2"],
    coef(model.high.threat.dynamic)["BTC_Volatility_Lag2"]
  ),
  
  GPR_Lag0 = c(
    coef(model.low.threat.dynamic)["GPRD_THREAT"],
    coef(model.high.threat.dynamic)["GPRD_THREAT"]
  ),
  
  GPR_Lag1 = c(
    coef(model.low.threat.dynamic)["GPRD_THREAT_Lag1"],
    coef(model.high.threat.dynamic)["GPRD_THREAT_Lag1"]
  ),
  
  GPR_Lag2 = c(
    coef(model.low.threat.dynamic)["GPRD_THREAT_Lag2"],
    coef(model.high.threat.dynamic)["GPRD_THREAT_Lag2"]
  ),
  
  Adj_R2 = c(
    summary(model.low.threat.dynamic)$adj.r.squared,
    summary(model.high.threat.dynamic)$adj.r.squared
  ),
  
  Residual_SE = c(
    summary(model.low.threat.dynamic)$sigma,
    summary(model.high.threat.dynamic)$sigma
  ),
  
  GPR_Lag0_NW_p = c(
    nw.low.threat.dynamic["GPRD_THREAT", "Pr(>|t|)"],
    nw.high.threat.dynamic["GPRD_THREAT", "Pr(>|t|)"]
  ),
  
  GPR_Lag1_NW_p = c(
    nw.low.threat.dynamic["GPRD_THREAT_Lag1", "Pr(>|t|)"],
    nw.high.threat.dynamic["GPRD_THREAT_Lag1", "Pr(>|t|)"]
  ),
  
  GPR_Lag2_NW_p = c(
    nw.low.threat.dynamic["GPRD_THREAT_Lag2", "Pr(>|t|)"],
    nw.high.threat.dynamic["GPRD_THREAT_Lag2", "Pr(>|t|)"]
  )
)

threat.dynamic.comparison


# 32. GPRD_THREAT interaction model

interaction.threat.model <- lm(
  BTC_Volatility ~ GPRD_THREAT * GPRD_THREAT_Regime,
  data = data
)

interaction.threat.nw <- nw_test(
  interaction.threat.model
)

interaction.threat.nw


###############################################################
# Section C: Post-2017 ACT Robustness Check
###############################################################

# 33. Create post-2017 sample

post2017.data <- subset(
  data,
  Date >= as.Date("2018-01-01")
)


# 34. Post-2017 ACT interaction model

post2017.act.interaction <- lm(
  BTC_Volatility ~ GPRD_ACT * GPRD_ACT_Regime,
  data = post2017.data
)


# 35. Post-2017 ACT Newey-West inference

post2017.act.interaction.nw <- nw_test(
  post2017.act.interaction
)

post2017.act.interaction.nw


# 36. Post-2017 ACT interaction summary

post2017.act.interaction.summary <- data.frame(
  
  Variable = c(
    "GPRD_ACT",
    "Elevated GPR",
    "GPRD_ACT × Elevated GPR"
  ),
  
  Estimate = c(
    post2017.act.interaction.nw[
      "GPRD_ACT",
      "Estimate"
    ],
    post2017.act.interaction.nw[
      "GPRD_ACT_RegimeElevated GPR",
      "Estimate"
    ],
    post2017.act.interaction.nw[
      "GPRD_ACT:GPRD_ACT_RegimeElevated GPR",
      "Estimate"
    ]
  ),
  
  Robust_SE = c(
    post2017.act.interaction.nw[
      "GPRD_ACT",
      "Std. Error"
    ],
    post2017.act.interaction.nw[
      "GPRD_ACT_RegimeElevated GPR",
      "Std. Error"
    ],
    post2017.act.interaction.nw[
      "GPRD_ACT:GPRD_ACT_RegimeElevated GPR",
      "Std. Error"
    ]
  ),
  
  P_Value = c(
    post2017.act.interaction.nw[
      "GPRD_ACT",
      "Pr(>|t|)"
    ],
    post2017.act.interaction.nw[
      "GPRD_ACT_RegimeElevated GPR",
      "Pr(>|t|)"
    ],
    post2017.act.interaction.nw[
      "GPRD_ACT:GPRD_ACT_RegimeElevated GPR",
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
# Section D: Summary
###############################################################

# 37. Final regime results

dynamic.comparison
act.dynamic.comparison
threat.dynamic.comparison

dynamic.incremental.r2
act.incremental.r2
threat.incremental.r2

# 38. Interaction results

interaction.summary <- data.frame(
  
  Measure = c(
    "GPRD_ACT",
    "GPRD_THREAT"
  ),
  
  GPR_Effect = c(
    interaction.act.nw["GPRD_ACT", "Estimate"],
    interaction.threat.nw["GPRD_THREAT", "Estimate"]
  ),
  
  GPR_P_Value = c(
    interaction.act.nw["GPRD_ACT", "Pr(>|t|)"],
    interaction.threat.nw["GPRD_THREAT", "Pr(>|t|)"]
  ),
  
  Regime_Interaction = c(
    interaction.act.nw[
      "GPRD_ACT:GPRD_ACT_RegimeElevated GPR",
      "Estimate"
    ],
    interaction.threat.nw[
      "GPRD_THREAT:GPRD_THREAT_RegimeElevated GPR",
      "Estimate"
    ]
  ),
  
  Interaction_P_Value = c(
    interaction.act.nw[
      "GPRD_ACT:GPRD_ACT_RegimeElevated GPR",
      "Pr(>|t|)"
    ],
    interaction.threat.nw[
      "GPRD_THREAT:GPRD_THREAT_RegimeElevated GPR",
      "Pr(>|t|)"
    ]
  )
)

interaction.summary$GPR_Effect <-
  signif(interaction.summary$GPR_Effect, 4)

interaction.summary$Regime_Interaction <-
  signif(interaction.summary$Regime_Interaction, 4)

interaction.summary$GPR_P_Value <-
  format_p(interaction.summary$GPR_P_Value)

interaction.summary$Interaction_P_Value <-
  format_p(interaction.summary$Interaction_P_Value)

interaction.summary

