# 1. Load required packages
library(readxl)
library(rugarch)
library(dplyr)
library(ggplot2)
library(lmtest)
library(car)
library(sandwich)
library(tseries)


# 2. Import data
data <- read_excel(
  "Thesis_Data.xlsx",
  sheet = "Data",
  na = "NA"
)

data <- na.omit(data)


# 3. Estimate Bitcoin EGARCH model
spec.egarch <- ugarchspec(
  
  variance.model = list(
    model = "eGARCH",
    garchOrder = c(1,1)
  ),
  
  mean.model = list(
    armaOrder = c(0,0),
    include.mean = TRUE
  ),
  
  distribution.model = "std"
  
)

fit.egarch <- ugarchfit(
  
  spec = spec.egarch,
  data = data$BTC_log_returns
  
)


# 4. Extract Bitcoin Conditional Volatility
data$BTC_Volatility <- as.numeric(
  sigma(fit.egarch)
)


# 5. Create Lagged GPR Variables
data <- data %>%
  
  mutate(
    
    GPR_Lag1 = lag(GPRD, 1),
    
    GPR_Lag2 = lag(GPRD, 2)
    
  )

data <- na.omit(data)


###############################################################
# Section A: Exploratory Analysis
###############################################################

# 6. Exploratory plots

# Bitcoin Returns vs Geopolitical Risk
ggplot(
  data,
  aes(
    x = GPRD,
    y = BTC_log_returns
  )
) +
  
  geom_point(
    alpha = 0.6
  ) +
  
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Bitcoin Returns vs Geopolitical Risk",
    x = "Geopolitical Risk Index",
    y = "Bitcoin Log Returns"
  )


# Bitcoin Conditional Volatility vs Geopolitical Risk
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


# 7. Correlation analysis
volatility.cor <- cor.test(
  
  data$BTC_Volatility,
  
  data$GPRD,
  
  method = "pearson"
  
)

volatility.cor


# 8. Correlation summary table
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

# 9. Estimate volatility regression models

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


# Volatility Model 3: Contemporaneous GPR + 2-day lags
volatility.model3 <- lm(
  
  BTC_Volatility ~
    
    GPRD +
    
    GPR_Lag1 +
    
    GPR_Lag2,
  
  data = data
  
)


# 10. Model summaries
summary(volatility.model1)

summary(volatility.model2)

summary(volatility.model3)


# 11. 95% confidence intervals
confint(volatility.model1)

confint(volatility.model2)

confint(volatility.model3)


###############################################################
# Section C: Model Comparison
###############################################################

# 12. Compare volatility regression models
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


###############################################################
# Section D: Model Diagnostics
###############################################################

# 13. Diagnostic plots

par(mfrow = c(2,2))

plot(volatility.model1)

plot(volatility.model2)

plot(volatility.model3)

par(mfrow = c(1,1))


# 14. Jarque-Bera tests
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


# 15. Autocorrelation tests

# Durbin-Watson
dw.model1 <- dwtest(volatility.model1)

dw.model2 <- dwtest(volatility.model2)

dw.model3 <- dwtest(volatility.model3)

dw.model1
dw.model2
dw.model3


# Ljung-Box
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


# 16. Heteroskedasticity tests

bp.model1 <- bptest(volatility.model1)

bp.model2 <- bptest(volatility.model2)

bp.model3 <- bptest(volatility.model3)

bp.model1
bp.model2
bp.model3


# 17. Multicollinearity

vif.model2 <- vif(
  volatility.model2
)

vif.model3 <- vif(
  volatility.model3
)

vif.model2
vif.model3


###############################################################
# Section E: Robust Inference
###############################################################

# 18. Newey-West robust standard errors

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


# 19. Display Newey-West results

nw.model1

nw.model2

nw.model3


###############################################################
# Section F: Regression Summary
###############################################################

# 20. Regression summary table
regression.summary <- data.frame(
  
  Model = c(
    
    "Volatility: No Lag",
    
    "Volatility: 1 Lag",
    
    "Volatility: 2 Lags"
    
  ),
  
  GPR_Coefficient = c(
    unname(coef(volatility.model1)["GPRD"]),
    unname(coef(volatility.model2)["GPRD"]),
    unname(coef(volatility.model3)["GPRD"])
  ),
  
  GPR_Lag1_Coefficient = c(
    NA,
    unname(coef(volatility.model2)["GPR_Lag1"]),
    unname(coef(volatility.model3)["GPR_Lag1"])
  ),
  
  GPR_Lag2_Coefficient = c(
    NA,
    NA,
    unname(coef(volatility.model3)["GPR_Lag2"])
  ),
  
  Adj_R2 = c(
    summary(volatility.model1)$adj.r.squared,
    summary(volatility.model2)$adj.r.squared,
    summary(volatility.model3)$adj.r.squared
  ),
  
  Final_GPR_pvalue = c(
    nw.model1["GPRD","Pr(>|t|)"],
    nw.model2["GPRD","Pr(>|t|)"],
    nw.model3["GPRD","Pr(>|t|)"]
  )
  
)

regression.summary$Significant_5pct <- ifelse(
  
  regression.summary$Final_GPR_pvalue < 0.05,
  
  "Yes",
  
  "No"
  
)

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

regression.summary$Adj_R2 <-
  round(
    regression.summary$Adj_R2,
    4
  )

regression.summary$Final_GPR_pvalue <-
  signif(
    regression.summary$Final_GPR_pvalue,
    4
  )

regression.summary


###############################################################
# Section G: Overall Summary
###############################################################

# 21. Overall findings table
overall.summary <- data.frame(
  
  Model = c(
    
    "Volatility: No Lag",
    
    "Volatility: 1 Lag",
    
    "Volatility: 2 Lags"
    
  ),
  
  Correlation = rep(
    unname(volatility.cor$estimate),
    3
  ),
  
  Correlation_pvalue = rep(
    volatility.cor$p.value,
    3
  ),
  
  Adj_R2 = c(
    summary(volatility.model1)$adj.r.squared,
    summary(volatility.model2)$adj.r.squared,
    summary(volatility.model3)$adj.r.squared
  ),
  
  Final_GPR_pvalue = c(
    nw.model1["GPRD","Pr(>|t|)"],
    nw.model2["GPRD","Pr(>|t|)"],
    nw.model3["GPRD","Pr(>|t|)"]
  )
  
)

overall.summary$Significant_5pct <- ifelse(
  
  overall.summary$Final_GPR_pvalue < 0.05,
  
  "Yes",
  
  "No"
  
)

overall.summary$Correlation <-
  round(
    overall.summary$Correlation,
    4
  )

overall.summary$Correlation_pvalue <-
  signif(
    overall.summary$Correlation_pvalue,
    4
  )

overall.summary$Adj_R2 <-
  round(
    overall.summary$Adj_R2,
    4
  )

overall.summary$Final_GPR_pvalue <-
  signif(
    overall.summary$Final_GPR_pvalue,
    4
  )

overall.summary







  


