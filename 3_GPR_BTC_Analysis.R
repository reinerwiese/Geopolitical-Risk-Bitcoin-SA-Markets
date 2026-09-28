# 1. Load required packages

library(dplyr)
library(ggplot2)
library(lmtest)
library(car)
library(sandwich)
library(tseries)


# 2. Prepare data

add_lags <- function(data) {
  data %>%
    mutate(
      GPR_Lag1 = lag(GPRD, 1),
      GPR_Lag2 = lag(GPRD, 2),
      BTC_Volatility_Lag1 = lag(BTC_Volatility, 1),
      BTC_Volatility_Lag2 = lag(BTC_Volatility, 2)
    ) %>%
    na.omit()
}

data <- add_lags(data0)


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

correlation.summary <- data.frame(
  Relationship = "Bitcoin Volatility vs GPR",
  Correlation = round(
    unname(volatility.cor$estimate),
    4
  ),
  P_Value = signif(
    volatility.cor$p.value,
    4
  )
)

correlation.summary


###############################################################
# Section B: Volatility Regression Analysis
###############################################################

# 5. Estimate volatility regression models

fit_gpr_models <- function(data) {
  list(
    "Volatility: No Lag" = lm(
      BTC_Volatility ~ GPRD,
      data = data
    ),
    
    "Volatility: 1 Lag" = lm(
      BTC_Volatility ~
        GPRD +
        GPR_Lag1,
      data = data
    ),
    
    "Volatility: 2 Lags" = lm(
      BTC_Volatility ~
        GPRD +
        GPR_Lag1 +
        GPR_Lag2,
      data = data
    ),
    
    "Dynamic: BTC Volatility Lag1 + GPR" = lm(
      BTC_Volatility ~
        BTC_Volatility_Lag1 +
        GPRD,
      data = data
    ),
    
    "Dynamic: BTC Volatility Lags1-2 + GPR" = lm(
      BTC_Volatility ~
        BTC_Volatility_Lag1 +
        BTC_Volatility_Lag2 +
        GPRD,
      data = data
    )
  )
}

models <- fit_gpr_models(data)


# 6. Model summaries

lapply(models, summary)


# 7. 95% confidence intervals

lapply(models, confint)


# 8. Model comparison

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
  lapply(
    names(models),
    function(name) {
      cbind(
        Model = name,
        model_stats(models[[name]])
      )
    }
  )
)

model.comparison <- model.comparison %>%
  mutate(
    Model_pvalue = signif(
      Model_pvalue,
      4
    )
  )

rownames(model.comparison) <- NULL

model.comparison


# 9. Model diagnostics

run_diagnostics <- function(model, dynamic = FALSE) {
  
  results <- list(
    JB = jarque.bera.test(
      residuals(model)
    ),
    
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
  
  results
}

diagnostics <- lapply(
  seq_along(models),
  function(i) {
    run_diagnostics(
      models[[i]],
      dynamic = grepl(
        "Dynamic",
        names(models)[i]
      )
    )
  }
)

names(diagnostics) <- names(models)

diagnostics


# Multicollinearity

vif.model2 <- vif(models[[2]])
vif.model3 <- vif(models[[3]])
vif.dynamic2 <- vif(models[[5]])

vif.model2
vif.model3
vif.dynamic2


# Diagnostic plots

par(mfrow = c(3, 2))

for (model in models) {
  plot(model)
}

par(mfrow = c(1, 1))


# 10. Newey-West Robust Inference

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

nw.models <- lapply(
  models,
  run_nw
)

names(nw.models) <- names(models)

nw.models


###############################################################
# Section C: GPR Component Analysis
###############################################################

# 11. Estimate GPR component models

component.models <- list(
  
  "GPRD_ACT" = lm(
    BTC_Volatility ~ GPRD_ACT,
    data = data
  ),
  
  "GPRD_THREAT" = lm(
    BTC_Volatility ~ GPRD_THREAT,
    data = data
  ),
  
  "GPRD_ACT + GPRD_THREAT" = lm(
    BTC_Volatility ~
      GPRD_ACT +
      GPRD_THREAT,
    data = data
  )
)

lapply(
  component.models,
  summary
)


# 12. Newey-West Robust Inference

component.nw <- lapply(
  component.models,
  run_nw
)

names(component.nw) <- names(component.models)

component.nw


###############################################################
# Section D: Post-2017 Robustness Analysis
###############################################################

# 13. Prepare post-2017 data

post2017.data <- data0 %>%
  filter(
    Date >= as.Date("2018-01-01")
  ) %>%
  add_lags()


# 14. Estimate post-2017 models

post2017.models <- fit_gpr_models(
  post2017.data
)

post2017.nw <- lapply(
  post2017.models,
  run_nw
)


# 15. Post-2017 robustness comparison

post2017.comparison <- data.frame(
  
  Model = names(post2017.models),
  
  N = sapply(
    post2017.models,
    nobs
  ),
  
  GPR_Coefficient = sapply(
    post2017.models,
    get_coef,
    variable = "GPRD"
  ),
  
  GPR_NW_pvalue = sapply(
    post2017.nw,
    get_nw_p,
    variable = "GPRD"
  )
) %>%
  mutate(
    GPR_Coefficient = signif(
      GPR_Coefficient,
      4
    ),
    GPR_NW_pvalue = signif(
      GPR_NW_pvalue,
      4
    )
  )

rownames(post2017.comparison) <- NULL

post2017.comparison


###############################################################
# Section E: Regression Summary
###############################################################

# 16. Main GPR regression summary

regression.summary <- data.frame(
  
  Model = names(models),
  
  GPR_Coefficient = sapply(
    models,
    get_coef,
    variable = "GPRD"
  ),
  
  GPR_Lag1_Coefficient = sapply(
    models,
    get_coef,
    variable = "GPR_Lag1"
  ),
  
  GPR_Lag2_Coefficient = sapply(
    models,
    get_coef,
    variable = "GPR_Lag2"
  ),
  
  BTC_Volatility_Lag1_Coefficient = sapply(
    models,
    get_coef,
    variable = "BTC_Volatility_Lag1"
  ),
  
  BTC_Volatility_Lag2_Coefficient = sapply(
    models,
    get_coef,
    variable = "BTC_Volatility_Lag2"
  ),
  
  Adj_R2 = sapply(
    models,
    function(x) summary(x)$adj.r.squared
  ),
  
  GPR_NW_pvalue = sapply(
    nw.models,
    get_nw_p,
    variable = "GPRD"
  ),
  
  GPR_Lag1_NW_pvalue = sapply(
    nw.models,
    get_nw_p,
    variable = "GPR_Lag1"
  ),
  
  GPR_Lag2_NW_pvalue = sapply(
    nw.models,
    get_nw_p,
    variable = "GPR_Lag2"
  ),
  
  BTC_Volatility_Lag1_NW_pvalue = sapply(
    nw.models,
    get_nw_p,
    variable = "BTC_Volatility_Lag1"
  ),
  
  BTC_Volatility_Lag2_NW_pvalue = sapply(
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


# 17. GPR component regression summary

gpr.component.summary <- data.frame(
  
  Model = names(component.models),
  
  GPRD_ACT_Coefficient = sapply(
    component.models,
    get_coef,
    variable = "GPRD_ACT"
  ),
  
  GPRD_THREAT_Coefficient = sapply(
    component.models,
    get_coef,
    variable = "GPRD_THREAT"
  ),
  
  Adj_R2 = sapply(
    component.models,
    function(x) summary(x)$adj.r.squared
  ),
  
  GPRD_ACT_NW_pvalue = sapply(
    component.nw,
    get_nw_p,
    variable = "GPRD_ACT"
  ),
  
  GPRD_THREAT_NW_pvalue = sapply(
    component.nw,
    get_nw_p,
    variable = "GPRD_THREAT"
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

rownames(gpr.component.summary) <- NULL

gpr.component.summary
