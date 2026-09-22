# 1. Load required packages
library(dplyr)
library(ggplot2)
library(lmtest)
library(car)
library(sandwich)
library(tseries)


# 2. Create Lagged GPR Variables
data <- data %>%
  
  mutate(
    
    GPR_Lag1 = lag(GPRD, 1),
    
    GPR_Lag2 = lag(GPRD, 2)
    
  )

data <- na.omit(data)


###############################################################
# Section A: Exploratory Analysis
###############################################################

# 3. Bitcoin Conditional Volatility vs Geopolitical Risk

ggplot(
  data,
  aes(
    x = GPRD,
    y = BTC_Volatility
  )
) +
  
  geom_point(
    alpha = 0.6
  ) +
  
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  
  geom_smooth(
    method = "loess",
    se = TRUE
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Bitcoin Conditional Volatility vs Geopolitical Risk",
    x = "Geopolitical Risk Index",
    y = "Bitcoin Conditional Volatility"
  )


# 4. Correlation analysis

volatility.cor <- cor.test(
  
  data$BTC_Volatility,
  
  data$GPRD,
  
  method = "pearson"
  
)

volatility.cor


# 5. Correlation summary table

correlation.summary <- data.frame(
  
  Relationship = "Bitcoin Volatility vs GPR",
  
  Correlation = unname(
    volatility.cor$estimate
  ),
  
  P_Value = volatility.cor$p.value
  
)

correlation.summary$Correlation <-
  round(
    correlation.summary$Correlation,
    4
  )

correlation.summary$P_Value <-
  signif(
    correlation.summary$P_Value,
    4
  )

correlation.summary


###############################################################
# Section B: Volatility Regression Analysis
###############################################################

# 6. Estimate volatility regression models

# Volatility Model 1: Contemporaneous GPR
volatility.model1 <- lm(
  
  BTC_Volatility ~
    
    GPRD,
  
  data = data
  
)


# Volatility Model 2: Contemporaneous GPR + 1-day lag
volatility.model2 <- lm(
  
  BTC_Volatility ~
    
    GPRD +
    
    GPR_Lag1,
  
  data = data
  
)


# Volatility Model 3: Contemporaneous GPR + 1-day and 2-day lags
volatility.model3 <- lm(
  
  BTC_Volatility ~
    
    GPRD +
    
    GPR_Lag1 +
    
    GPR_Lag2,
  
  data = data
  
)


# 7. Model summaries

summary(volatility.model1)

summary(volatility.model2)

summary(volatility.model3)


# 8. 95% confidence intervals

confint(volatility.model1)

confint(volatility.model2)

confint(volatility.model3)


# 9. Model Comparison

model.comparison <- data.frame(
  
  Model = c(
    
    "Volatility: No Lag",
    
    "Volatility: 1 Lag",
    
    "Volatility: 2 Lags"
    
  ),
  
  LogLikelihood = c(
    
    as.numeric(logLik(volatility.model1)),
    
    as.numeric(logLik(volatility.model2)),
    
    as.numeric(logLik(volatility.model3))
    
  ),
  
  Adj_R2 = c(
    
    summary(volatility.model1)$adj.r.squared,
    
    summary(volatility.model2)$adj.r.squared,
    
    summary(volatility.model3)$adj.r.squared
    
  ),
  
  AIC = c(
    
    AIC(volatility.model1),
    
    AIC(volatility.model2),
    
    AIC(volatility.model3)
    
  ),
  
  BIC = c(
    
    BIC(volatility.model1),
    
    BIC(volatility.model2),
    
    BIC(volatility.model3)
    
  ),
  
  Residual_SE = c(
    
    summary(volatility.model1)$sigma,
    
    summary(volatility.model2)$sigma,
    
    summary(volatility.model3)$sigma
    
  ),
  
  F_Statistic = c(
    
    summary(volatility.model1)$fstatistic[1],
    
    summary(volatility.model2)$fstatistic[1],
    
    summary(volatility.model3)$fstatistic[1]
    
  ),
  
  Model_pvalue = c(
    
    pf(
      summary(volatility.model1)$fstatistic[1],
      summary(volatility.model1)$fstatistic[2],
      summary(volatility.model1)$fstatistic[3],
      lower.tail = FALSE
    ),
    
    pf(
      summary(volatility.model2)$fstatistic[1],
      summary(volatility.model2)$fstatistic[2],
      summary(volatility.model2)$fstatistic[3],
      lower.tail = FALSE
    ),
    
    pf(
      summary(volatility.model3)$fstatistic[1],
      summary(volatility.model3)$fstatistic[2],
      summary(volatility.model3)$fstatistic[3],
      lower.tail = FALSE
    )
    
  )
  
)

model.comparison$Model_pvalue <-
  signif(
    model.comparison$Model_pvalue,
    4
  )

model.comparison


# 10. Model Diagnostics

# Diagnostic plots

par(mfrow = c(2,2))

plot(volatility.model1)

plot(volatility.model2)

plot(volatility.model3)

par(mfrow = c(1,1))


# Jarque-Bera tests

jb.model1 <- jarque.bera.test(
  residuals(volatility.model1)
)

jb.model2 <- jarque.bera.test(
  residuals(volatility.model2)
)

jb.model3 <- jarque.bera.test(
  residuals(volatility.model3)
)

jb.model1
jb.model2
jb.model3


# Durbin-Watson tests

dw.model1 <- dwtest(
  volatility.model1
)

dw.model2 <- dwtest(
  volatility.model2
)

dw.model3 <- dwtest(
  volatility.model3
)

dw.model1
dw.model2
dw.model3


# Ljung-Box tests

lb.model1 <- Box.test(
  residuals(volatility.model1),
  lag = 20,
  type = "Ljung-Box"
)

lb.model2 <- Box.test(
  residuals(volatility.model2),
  lag = 20,
  type = "Ljung-Box"
)

lb.model3 <- Box.test(
  residuals(volatility.model3),
  lag = 20,
  type = "Ljung-Box"
)

lb.model1
lb.model2
lb.model3


# Breusch-Pagan tests

bp.model1 <- bptest(
  volatility.model1
)

bp.model2 <- bptest(
  volatility.model2
)

bp.model3 <- bptest(
  volatility.model3
)

bp.model1
bp.model2
bp.model3


# Multicollinearity

vif.model2 <- vif(
  volatility.model2
)

vif.model3 <- vif(
  volatility.model3
)

vif.model2
vif.model3


# 11. Newey-West Robust Inference

# Newey-West robust standard errors

nw.model1 <- coeftest(
  
  volatility.model1,
  
  vcov = NeweyWest(
    
    volatility.model1,
    
    prewhite = FALSE
    
  )
  
)


nw.model2 <- coeftest(
  
  volatility.model2,
  
  vcov = NeweyWest(
    
    volatility.model2,
    
    prewhite = FALSE
    
  )
  
)


nw.model3 <- coeftest(
  
  volatility.model3,
  
  vcov = NeweyWest(
    
    volatility.model3,
    
    prewhite = FALSE
    
  )
  
)


# Display Newey-West results

nw.model1

nw.model2

nw.model3


###############################################################
# Section C: GPR Component Analysis
###############################################################

# 12. Estimate GPR component models

# Geopolitical Acts
gprd.act.model <- lm(
  
  BTC_Volatility ~
    
    GPRD_ACT,
  
  data = data
  
)


# Geopolitical Threats
gprd.threat.model <- lm(
  
  BTC_Volatility ~
    
    GPRD_THREAT,
  
  data = data
  
)


# Geopolitical Acts + Threats
gprd.components.model <- lm(
  
  BTC_Volatility ~
    
    GPRD_ACT +
    
    GPRD_THREAT,
  
  data = data
  
)


# 13. Model summaries

summary(gprd.act.model)

summary(gprd.threat.model)

summary(gprd.components.model)


# 14. Newey-West Robust Inference for GPR Components

# Geopolitical Acts

nw.gprd.act <- coeftest(
  
  gprd.act.model,
  
  vcov = NeweyWest(
    
    gprd.act.model,
    
    prewhite = FALSE
    
  )
  
)


# Geopolitical Threats

nw.gprd.threat <- coeftest(
  
  gprd.threat.model,
  
  vcov = NeweyWest(
    
    gprd.threat.model,
    
    prewhite = FALSE
    
  )
  
)


# Geopolitical Acts + Threats

nw.gprd.components <- coeftest(
  
  gprd.components.model,
  
  vcov = NeweyWest(
    
    gprd.components.model,
    
    prewhite = FALSE
    
  )
  
)


# Display Newey-West results

nw.gprd.act

nw.gprd.threat

nw.gprd.components


###############################################################
# Section D: Regression Summary
###############################################################

# 15. Main GPR regression summary

regression.summary <- data.frame(
  
  Model = c(
    
    "Volatility: No Lag",
    
    "Volatility: 1 Lag",
    
    "Volatility: 2 Lags"
    
  ),
  
  GPR_Coefficient = c(
    
    unname(
      coef(volatility.model1)["GPRD"]
    ),
    
    unname(
      coef(volatility.model2)["GPRD"]
    ),
    
    unname(
      coef(volatility.model3)["GPRD"]
    )
    
  ),
  
  GPR_Lag1_Coefficient = c(
    
    NA,
    
    unname(
      coef(volatility.model2)["GPR_Lag1"]
    ),
    
    unname(
      coef(volatility.model3)["GPR_Lag1"]
    )
    
  ),
  
  GPR_Lag2_Coefficient = c(
    
    NA,
    
    NA,
    
    unname(
      coef(volatility.model3)["GPR_Lag2"]
    )
    
  ),
  
  Adj_R2 = c(
    
    summary(volatility.model1)$adj.r.squared,
    
    summary(volatility.model2)$adj.r.squared,
    
    summary(volatility.model3)$adj.r.squared
    
  ),
  
  GPR_NW_pvalue = c(
    
    nw.model1["GPRD", "Pr(>|t|)"],
    
    nw.model2["GPRD", "Pr(>|t|)"],
    
    nw.model3["GPRD", "Pr(>|t|)"]
    
  ),
  
  GPR_Lag1_NW_pvalue = c(
    
    NA,
    
    nw.model2["GPR_Lag1", "Pr(>|t|)"],
    
    nw.model3["GPR_Lag1", "Pr(>|t|)"]
    
  ),
  
  GPR_Lag2_NW_pvalue = c(
    
    NA,
    
    NA,
    
    nw.model3["GPR_Lag2", "Pr(>|t|)"]
    
  )
  
)


# Round coefficients

regression.summary$GPR_Coefficient <-
  signif(
    regression.summary$GPR_Coefficient,
    4
  )

regression.summary$GPR_Lag1_Coefficient <-
  signif(
    regression.summary$GPR_Lag1_Coefficient,
    4
  )

regression.summary$GPR_Lag2_Coefficient <-
  signif(
    regression.summary$GPR_Lag2_Coefficient,
    4
  )


# Round adjusted R-squared

regression.summary$Adj_R2 <-
  round(
    regression.summary$Adj_R2,
    4
  )


# Round Newey-West p-values

regression.summary$GPR_NW_pvalue <-
  signif(
    regression.summary$GPR_NW_pvalue,
    4
  )

regression.summary$GPR_Lag1_NW_pvalue <-
  signif(
    regression.summary$GPR_Lag1_NW_pvalue,
    4
  )

regression.summary$GPR_Lag2_NW_pvalue <-
  signif(
    regression.summary$GPR_Lag2_NW_pvalue,
    4
  )


regression.summary


# 16. GPR component regression summary

gpr.component.summary <- data.frame(
  
  Model = c(
    
    "GPRD_ACT",
    
    "GPRD_THREAT",
    
    "GPRD_ACT + GPRD_THREAT"
    
  ),
  
  GPRD_ACT_Coefficient = c(
    
    unname(
      coef(gprd.act.model)["GPRD_ACT"]
    ),
    
    NA,
    
    unname(
      coef(gprd.components.model)["GPRD_ACT"]
    )
    
  ),
  
  GPRD_THREAT_Coefficient = c(
    
    NA,
    
    unname(
      coef(gprd.threat.model)["GPRD_THREAT"]
    ),
    
    unname(
      coef(gprd.components.model)["GPRD_THREAT"]
    )
    
  ),
  
  Adj_R2 = c(
    
    summary(gprd.act.model)$adj.r.squared,
    
    summary(gprd.threat.model)$adj.r.squared,
    
    summary(gprd.components.model)$adj.r.squared
    
  ),
  
  GPRD_ACT_NW_pvalue = c(
    
    nw.gprd.act["GPRD_ACT", "Pr(>|t|)"],
    
    NA,
    
    nw.gprd.components["GPRD_ACT", "Pr(>|t|)"]
    
  ),
  
  GPRD_THREAT_NW_pvalue = c(
    
    NA,
    
    nw.gprd.threat["GPRD_THREAT", "Pr(>|t|)"],
    
    nw.gprd.components["GPRD_THREAT", "Pr(>|t|)"]
    
  )
  
)


# Round coefficients

gpr.component.summary$GPRD_ACT_Coefficient <-
  signif(
    gpr.component.summary$GPRD_ACT_Coefficient,
    4
  )

gpr.component.summary$GPRD_THREAT_Coefficient <-
  signif(
    gpr.component.summary$GPRD_THREAT_Coefficient,
    4
  )


# Round adjusted R-squared

gpr.component.summary$Adj_R2 <-
  round(
    gpr.component.summary$Adj_R2,
    4
  )


# Round Newey-West p-values

gpr.component.summary$GPRD_ACT_NW_pvalue <-
  signif(
    gpr.component.summary$GPRD_ACT_NW_pvalue,
    4
  )

gpr.component.summary$GPRD_THREAT_NW_pvalue <-
  signif(
    gpr.component.summary$GPRD_THREAT_NW_pvalue,
    4
  )


gpr.component.summary
