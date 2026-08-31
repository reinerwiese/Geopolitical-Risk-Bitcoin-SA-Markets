# 1. Load Packages
library(readxl)
library(rugarch)
library(dplyr)
library(ggplot2)
library(lmtest)
library(car)
library(sandwich)
library(tseries)


###############################################################
# Section A: Bitcoin and Geopolitical Risk Analysis
###############################################################

# 2. Import Data
data <- read_excel(
  "Thesis_Data.xlsx",
  sheet = "Data"
)

data <- na.omit(data)


# 3. Estimate EGARCH Model
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


# 4. Extract Conditional Volatility
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


# 6. Exploratory Plots

# Bitcoin Returns vs GPR
ggplot(
  data,
  aes(GPRD, BTC_log_returns)
) +
  
  geom_point() +
  
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


# Bitcoin Volatility vs GPR
ggplot(
  data,
  aes(GPRD, BTC_Volatility)
) +
  
  geom_point() +
  
  geom_smooth(
    method = "lm",
    colour = "red",
    linewidth = 1,
    se = FALSE
  ) +
  
  geom_smooth(
    method = "loess",
    colour = "blue",
    linewidth = 1,
    se = TRUE
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Bitcoin Volatility vs Geopolitical Risk",
    x = "Geopolitical Risk Index",
    y = "Conditional Volatility"
  )


# 7. Correlation Analysis
returns.cor <- cor.test(
  
  data$BTC_log_returns,
  
  data$GPRD
  
)

volatility.cor <- cor.test(
  
  data$BTC_Volatility,
  
  data$GPRD
  
)

returns.cor
volatility.cor


# 8. Bitcoin Returns Regressions

# Returns Model 1: No Lag
returns.model1 <- lm(
  
  BTC_log_returns ~
    
    GPRD,
  
  data = data
  
)


# Returns Model 2: One Lag
returns.model2 <- lm(
  
  BTC_log_returns ~
    
    GPRD +
    
    GPR_Lag1,
  
  data = data
  
)


# Returns Model 3: Two Lags
returns.model3 <- lm(
  
  BTC_log_returns ~
    
    GPRD +
    
    GPR_Lag1 +
    
    GPR_Lag2,
  
  data = data
  
)


# 9. Bitcoin Volatility Regressions

# Volatility Model 1: No Lag
volatility.model1 <- lm(
  
  BTC_Volatility ~
    
    GPRD,
  
  data = data
  
)


# Volatility Model 2: One Lag
volatility.model2 <- lm(
  
  BTC_Volatility ~
    
    GPRD +
    
    GPR_Lag1,
  
  data = data
  
)


# Volatility Model 3: Two Lags
volatility.model3 <- lm(
  
  BTC_Volatility ~
    
    GPRD +
    
    GPR_Lag1 +
    
    GPR_Lag2,
  
  data = data
  
)


# 10. Regression Output
summary(returns.model1)      # Not significant
summary(returns.model2)      # Not significant
summary(returns.model3)      # Not significant

summary(volatility.model1)   # Significant negative relationship
summary(volatility.model2)   # Both coefficients significant
summary(volatility.model3)   # Lag 2 significant at the 5% level

# The negative correlation is likely due to the linearity of the test.


# 11. Model Comparison Tables
returns.comparison <- data.frame(
  
  Model = c(
    "Returns: No Lag",
    "Returns: 1 Lag",
    "Returns: 2 Lags"
  ),
  
  Adj_R2 = c(
    summary(returns.model1)$adj.r.squared,
    summary(returns.model2)$adj.r.squared,
    summary(returns.model3)$adj.r.squared
  ),
  
  AIC = c(
    AIC(returns.model1),
    AIC(returns.model2),
    AIC(returns.model3)
  ),
  
  BIC = c(
    BIC(returns.model1),
    BIC(returns.model2),
    BIC(returns.model3)
  ),
  
  F_Statistic = c(
    summary(returns.model1)$fstatistic[1],
    summary(returns.model2)$fstatistic[1],
    summary(returns.model3)$fstatistic[1]
  ),
  
  Model_pvalue = c(
    
    pf(
      summary(returns.model1)$fstatistic[1],
      summary(returns.model1)$fstatistic[2],
      summary(returns.model1)$fstatistic[3],
      lower.tail = FALSE
    ),
    
    pf(
      summary(returns.model2)$fstatistic[1],
      summary(returns.model2)$fstatistic[2],
      summary(returns.model2)$fstatistic[3],
      lower.tail = FALSE
    ),
    
    pf(
      summary(returns.model3)$fstatistic[1],
      summary(returns.model3)$fstatistic[2],
      summary(returns.model3)$fstatistic[3],
      lower.tail = FALSE
    )
    
  )
  
)

returns.comparison


volatility.comparison <- data.frame(
  
  Model = c(
    "Volatility: No Lag",
    "Volatility: 1 Lag",
    "Volatility: 2 Lags"
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

volatility.comparison

###############################################################
# Section B: Regression Diagnostics
###############################################################

# 12. Diagnostic Plots

# Volatility Model 2 (1 Lag) and Volatility Model 3 (2 Lags)
par(mfrow = c(2,2))

plot(volatility.model2)

plot(volatility.model3)

par(mfrow = c(1,1))


# 13. Normality Tests
jb.model2 <- jarque.bera.test(
  residuals(volatility.model2)
)

jb.model3 <- jarque.bera.test(
  residuals(volatility.model3)
)

jb.model2   # The residuals are not normally distributed.
jb.model3   # The residuals are not normally distributed.


# 14. Autocorrelation Tests

# Durbin-Watson
dw.model2 <- dwtest(volatility.model2)
dw.model3 <- dwtest(volatility.model3)

dw.model2   # Strong evidence of positive residual autocorrelation (DW << 2).
dw.model3   # Strong evidence of positive residual autocorrelation (DW << 2).

# Ljung-Box
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

lb.model2   # Residual autocorrelation remains across multiple (20) lags.
lb.model3   # Residual autocorrelation remains across multiple (20) lags.


# 15. Heteroskedasticity Tests
# Examines whether the variance of the regression residuals is constant.

bp.model2 <- bptest(volatility.model2)

bp.model3 <- bptest(volatility.model3)

bp.model2   # There is evidence of heteroskedasticity.
bp.model3   # There is evidence of heteroskedasticity.


# 16. Multicollinearity
vif.model2 <- vif(volatility.model2)

vif.model3 <- vif(volatility.model3)

vif.model2   # No evidence of problematic multicollinearity.
vif.model3   # No evidence of problematic multicollinearity.


# 17. Newey-West Robust Inference
# Diagnostic tests indicate significant residual
# autocorrelation and heteroskedasticity.
# Therefore, Newey-West HAC standard errors are used
# for statistical inference.

nw.model2 <- coeftest(
  
  volatility.model2,
  
  vcov = NeweyWest(
    
    volatility.model2,
    
    prewhite = FALSE
    
  )
  
)

nw.model2   # HAC-robust inference; GPR effects not significant at the 5% level.

nw.model3 <- coeftest(
  
  volatility.model3,
  
  vcov = NeweyWest(
    
    volatility.model3,
    
    prewhite = FALSE
    
  )
  
)

nw.model3   # HAC-robust inference; lagged GPR effects marginally significant (10%).


# 18. Model Comparison
model.comparison <- data.frame(
  
  Model = c(
    "Volatility: 1 Lag",
    "Volatility: 2 Lags"
  ),
  
  LogLikelihood = c(
    as.numeric(logLik(volatility.model2)),
    as.numeric(logLik(volatility.model3))
  ),
  
  Adj_R2 = c(
    summary(volatility.model2)$adj.r.squared,
    summary(volatility.model3)$adj.r.squared
  ),
  
  AIC = c(
    AIC(volatility.model2),
    AIC(volatility.model3)
  ),
  
  BIC = c(
    BIC(volatility.model2),
    BIC(volatility.model3)
  ),
  
  Residual_SE = c(
    summary(volatility.model2)$sigma,
    summary(volatility.model3)$sigma
  ),
  
  Jarque_Bera_p_value = c(
    jb.model2$p.value,
    jb.model3$p.value
  ),
  
  DW_Statistic = c(
    as.numeric(dw.model2$statistic),
    as.numeric(dw.model3$statistic)
  ),
  
  DW_p_value = c(
    dw.model2$p.value,
    dw.model3$p.value
  ),
  
  LjungBox_p_value = c(
    lb.model2$p.value,
    lb.model3$p.value
  ),
  
  BreuschPagan_p_value = c(
    bp.model2$p.value,
    bp.model3$p.value
  ),
  
  Max_VIF = c(
    max(vif.model2),
    max(vif.model3)
  )
  
)

model.comparison[-1] <- round(model.comparison[-1], 4)

model.comparison