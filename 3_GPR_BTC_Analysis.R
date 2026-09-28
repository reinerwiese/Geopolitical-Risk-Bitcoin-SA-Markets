# 1. Load required packages

library(dplyr)
library(ggplot2)
library(lmtest)
library(car)
library(sandwich)
library(tseries)


# 2. Create Lagged Variables

data <- data0 %>%
  mutate(
    GPR_Lag1 = lag(GPRD, 1),
    GPR_Lag2 = lag(GPRD, 2),
    BTC_Volatility_Lag1 = lag(BTC_Volatility, 1),
    BTC_Volatility_Lag2 = lag(BTC_Volatility, 2)
  ) %>%
  na.omit()


###############################################################
# Section A: Exploratory Analysis
###############################################################

# 3. Bitcoin Conditional Volatility vs Geopolitical Risk

ggplot(
  data,
  aes(x = GPRD, y = BTC_Volatility)
) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "loess", se = TRUE) +
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
  Correlation = round(unname(volatility.cor$estimate), 4),
  P_Value = signif(volatility.cor$p.value, 4)
)

correlation.summary


###############################################################
# Section B: Volatility Regression Analysis
###############################################################

# 6. Estimate volatility regression models

volatility.model1 <- lm(
  BTC_Volatility ~ GPRD,
  data = data
)

volatility.model2 <- lm(
  BTC_Volatility ~ GPRD + GPR_Lag1,
  data = data
)

volatility.model3 <- lm(
  BTC_Volatility ~ GPRD + GPR_Lag1 + GPR_Lag2,
  data = data
)

volatility.dynamic1 <- lm(
  BTC_Volatility ~ BTC_Volatility_Lag1 + GPRD,
  data = data
)

volatility.dynamic2 <- lm(
  BTC_Volatility ~ BTC_Volatility_Lag1 + BTC_Volatility_Lag2 + GPRD,
  data = data
)

models <- list(
  volatility.model1,
  volatility.model2,
  volatility.model3,
  volatility.dynamic1,
  volatility.dynamic2
)


# 7. Model summaries

lapply(models, summary)


# 8. 95% confidence intervals

lapply(models, confint)


# 9. Model comparison

model.names <- c(
  "Volatility: No Lag",
  "Volatility: 1 Lag",
  "Volatility: 2 Lags",
  "Dynamic: BTC Volatility Lag1 + GPR",
  "Dynamic: BTC Volatility Lags1-2 + GPR"
)

model_stats <- function(model) {
  
  fstat <- summary(model)$fstatistic
  
  data.frame(
    LogLikelihood = as.numeric(logLik(model)),
    Adj_R2 = summary(model)$adj.r.squared,
    AIC = AIC(model),
    BIC = BIC(model),
    Residual_SE = summary(model)$sigma,
    F_Statistic = fstat[1],
    Model_pvalue = pf(
      fstat[1],
      fstat[2],
      fstat[3],
      lower.tail = FALSE
    )
  )
}

model.comparison <- bind_rows(
  lapply(models, model_stats)
)

model.comparison$Model <- model.names

model.comparison <- model.comparison[
  ,
  c(
    "Model",
    setdiff(names(model.comparison), "Model")
  )
]

rownames(model.comparison) <- NULL

model.comparison$Model_pvalue <-
  signif(model.comparison$Model_pvalue, 4)

model.comparison


# 10. Model Diagnostics

run_diagnostics <- function(model, dynamic = FALSE) {
  
  results <- list(
    JB = jarque.bera.test(residuals(model)),
    
    Ljung_Box = Box.test(
      residuals(model),
      lag = 20,
      type = "Ljung-Box"
    ),
    
    Breusch_Pagan = bptest(model)
  )
  
  if (dynamic) {
    
    results$Breusch_Godfrey <- bgtest(
      model,
      order = 2
    )
    
  } else {
    
    results$Durbin_Watson <- dwtest(model)
    
  }
  
  return(results)
}


diagnostics.model1 <- run_diagnostics(volatility.model1)
diagnostics.model2 <- run_diagnostics(volatility.model2)
diagnostics.model3 <- run_diagnostics(volatility.model3)

diagnostics.dynamic1 <- run_diagnostics(
  volatility.dynamic1,
  dynamic = TRUE
)

diagnostics.dynamic2 <- run_diagnostics(
  volatility.dynamic2,
  dynamic = TRUE
)


diagnostics.model1
diagnostics.model2
diagnostics.model3
diagnostics.dynamic1
diagnostics.dynamic2


# Multicollinearity

vif.model2 <- vif(volatility.model2)
vif.model3 <- vif(volatility.model3)
vif.dynamic2 <- vif(volatility.dynamic2)

vif.model2
vif.model3
vif.dynamic2


# Diagnostic plots

par(mfrow = c(3, 2))

for (model in models) {
  plot(model)
}

par(mfrow = c(1, 1))


# 11. Newey-West Robust Inference

run_nw <- function(model) {
  
  coeftest(
    model,
    vcov = NeweyWest(
      model,
      prewhite = FALSE
    )
  )
}


nw.model1 <- run_nw(volatility.model1)
nw.model2 <- run_nw(volatility.model2)
nw.model3 <- run_nw(volatility.model3)
nw.dynamic1 <- run_nw(volatility.dynamic1)
nw.dynamic2 <- run_nw(volatility.dynamic2)


nw.model1
nw.model2
nw.model3
nw.dynamic1
nw.dynamic2


###############################################################
# Section C: GPR Component Analysis
###############################################################

# 12. Estimate GPR component models

gprd.act.model <- lm(
  BTC_Volatility ~ GPRD_ACT,
  data = data
)

gprd.threat.model <- lm(
  BTC_Volatility ~ GPRD_THREAT,
  data = data
)

gprd.components.model <- lm(
  BTC_Volatility ~ GPRD_ACT + GPRD_THREAT,
  data = data
)

component.models <- list(
  gprd.act.model,
  gprd.threat.model,
  gprd.components.model
)


# 13. Model summaries

lapply(component.models, summary)


# 14. Newey-West Robust Inference

nw.gprd.act <- run_nw(gprd.act.model)
nw.gprd.threat <- run_nw(gprd.threat.model)
nw.gprd.components <- run_nw(gprd.components.model)

nw.gprd.act
nw.gprd.threat
nw.gprd.components


###############################################################
# Section D: Regression Summary
###############################################################

# 15. Main GPR regression summary

regression.summary <- data.frame(
  
  Model = model.names,
  
  GPR_Coefficient = c(
    coef(volatility.model1)["GPRD"],
    coef(volatility.model2)["GPRD"],
    coef(volatility.model3)["GPRD"],
    coef(volatility.dynamic1)["GPRD"],
    coef(volatility.dynamic2)["GPRD"]
  ),
  
  GPR_Lag1_Coefficient = c(
    NA,
    coef(volatility.model2)["GPR_Lag1"],
    coef(volatility.model3)["GPR_Lag1"],
    NA,
    NA
  ),
  
  GPR_Lag2_Coefficient = c(
    NA,
    NA,
    coef(volatility.model3)["GPR_Lag2"],
    NA,
    NA
  ),
  
  BTC_Volatility_Lag1_Coefficient = c(
    NA,
    NA,
    NA,
    coef(volatility.dynamic1)["BTC_Volatility_Lag1"],
    coef(volatility.dynamic2)["BTC_Volatility_Lag1"]
  ),
  
  BTC_Volatility_Lag2_Coefficient = c(
    NA,
    NA,
    NA,
    NA,
    coef(volatility.dynamic2)["BTC_Volatility_Lag2"]
  ),
  
  Adj_R2 = sapply(
    models,
    function(x) summary(x)$adj.r.squared
  ),
  
  GPR_NW_pvalue = c(
    nw.model1["GPRD", "Pr(>|t|)"],
    nw.model2["GPRD", "Pr(>|t|)"],
    nw.model3["GPRD", "Pr(>|t|)"],
    nw.dynamic1["GPRD", "Pr(>|t|)"],
    nw.dynamic2["GPRD", "Pr(>|t|)"]
  ),
  
  GPR_Lag1_NW_pvalue = c(
    NA,
    nw.model2["GPR_Lag1", "Pr(>|t|)"],
    nw.model3["GPR_Lag1", "Pr(>|t|)"],
    NA,
    NA
  ),
  
  GPR_Lag2_NW_pvalue = c(
    NA,
    NA,
    nw.model3["GPR_Lag2", "Pr(>|t|)"],
    NA,
    NA
  ),
  
  BTC_Volatility_Lag1_NW_pvalue = c(
    NA,
    NA,
    NA,
    nw.dynamic1[
      "BTC_Volatility_Lag1",
      "Pr(>|t|)"
    ],
    nw.dynamic2[
      "BTC_Volatility_Lag1",
      "Pr(>|t|)"
    ]
  ),
  
  BTC_Volatility_Lag2_NW_pvalue = c(
    NA,
    NA,
    NA,
    NA,
    nw.dynamic2[
      "BTC_Volatility_Lag2",
      "Pr(>|t|)"
    ]
  )
)


# Round regression summary

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

regression.summary


# 16. GPR component regression summary

gpr.component.summary <- data.frame(
  
  Model = c(
    "GPRD_ACT",
    "GPRD_THREAT",
    "GPRD_ACT + GPRD_THREAT"
  ),
  
  GPRD_ACT_Coefficient = c(
    coef(gprd.act.model)["GPRD_ACT"],
    NA,
    coef(gprd.components.model)["GPRD_ACT"]
  ),
  
  GPRD_THREAT_Coefficient = c(
    NA,
    coef(gprd.threat.model)["GPRD_THREAT"],
    coef(gprd.components.model)["GPRD_THREAT"]
  ),
  
  Adj_R2 = sapply(
    component.models,
    function(x) summary(x)$adj.r.squared
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


# Round component summary

gpr.component.summary <- gpr.component.summary %>%
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

gpr.component.summary
