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
spec.btc <- ugarchspec(
  
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

fit.btc <- ugarchfit(
  spec = spec.btc,
  data = data$BTC_log_returns
)


# 4. Extract Bitcoin Conditional Volatility
data$BTC_Volatility <- as.numeric(
  sigma(fit.btc)
)


# 5. Estimate J303 EGARCH model
spec.j303 <- ugarchspec(
  
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

fit.j303 <- ugarchfit(
  spec = spec.j303,
  data = data$Index_log_returns
)


# 6. Extract J303 Conditional Volatility
data$J303_Volatility <- as.numeric(
  sigma(fit.j303)
)


###############################################################
# Section A: Exploratory Analysis
###############################################################

# 7. Correlation matrix
correlation.matrix <- cor(
  
  data[, c(
    "BTC_Volatility",
    "J303_Volatility",
    "GPRD"
  )],
  
  method = "pearson"
  
)

round(
  correlation.matrix,
  4
)


# 8. Bitcoin Volatility vs J303 Volatility
ggplot(
  data,
  aes(
    x = BTC_Volatility,
    y = J303_Volatility
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
    title = "Bitcoin Volatility vs J303 Volatility",
    x = "Bitcoin Conditional Volatility",
    y = "J303 Conditional Volatility"
  )


# 9. Pearson correlation test
btc.j303.cor <- cor.test(
  
  data$BTC_Volatility,
  
  data$J303_Volatility,
  
  method = "pearson"
  
)

btc.j303.cor


# 10. Correlation summary table
correlation.summary <- data.frame(
  
  Relationship = "Bitcoin vs J303 Volatility",
  
  Correlation = unname(
    btc.j303.cor$estimate
  ),
  
  P_Value = btc.j303.cor$p.value
  
)

correlation.summary$Correlation <-
  round(correlation.summary$Correlation, 4)

correlation.summary$P_Value <-
  signif(correlation.summary$P_Value, 4)

correlation.summary


###############################################################
# Section B: Regression Analysis
###############################################################

# 11. Baseline volatility model
j303.btc <- lm(
  
  J303_Volatility ~
    
    BTC_Volatility,
  
  data = data
  
)


# 12. Extended volatility model including GPR
j303.btc.gpr <- lm(
  
  J303_Volatility ~
    
    BTC_Volatility +
    
    GPRD,
  
  data = data
  
)


# 13. Model summaries
summary(j303.btc)

summary(j303.btc.gpr)


# 14. 95% confidence intervals
confint(j303.btc)

confint(j303.btc.gpr)


###############################################################
# Section C: Model Comparison
###############################################################

# 15. Compare regression models
model.comparison <- data.frame(
  
  Model = c(
    
    "J303 Volatility ~ BTC Volatility",
    
    "J303 Volatility ~ BTC Volatility + GPR"
    
  ),
  
  LogLikelihood = c(
    as.numeric(logLik(j303.btc)),
    as.numeric(logLik(j303.btc.gpr))
  ),
  
  Adj_R2 = c(
    summary(j303.btc)$adj.r.squared,
    summary(j303.btc.gpr)$adj.r.squared
  ),
  
  AIC = c(
    AIC(j303.btc),
    AIC(j303.btc.gpr)
  ),
  
  BIC = c(
    BIC(j303.btc),
    BIC(j303.btc.gpr)
  ),
  
  Residual_SE = c(
    summary(j303.btc)$sigma,
    summary(j303.btc.gpr)$sigma
  ),
  
  F_Statistic = c(
    summary(j303.btc)$fstatistic[1],
    summary(j303.btc.gpr)$fstatistic[1]
  ),
  
  Model_pvalue = c(
    
    pf(
      summary(j303.btc)$fstatistic[1],
      summary(j303.btc)$fstatistic[2],
      summary(j303.btc)$fstatistic[3],
      lower.tail = FALSE
    ),
    
    pf(
      summary(j303.btc.gpr)$fstatistic[1],
      summary(j303.btc.gpr)$fstatistic[2],
      summary(j303.btc.gpr)$fstatistic[3],
      lower.tail = FALSE
    )
    
  )
  
)

model.comparison$Model_pvalue <-
  signif(model.comparison$Model_pvalue, 4)

model.comparison


# 16. Nested model comparison
model.comparison.test <- anova(
  
  j303.btc,
  
  j303.btc.gpr
  
)

model.comparison.test


###############################################################
# Section D: Model Diagnostics
###############################################################

# 17. Diagnostic plots
par(mfrow = c(2,2))

plot(j303.btc)

plot(j303.btc.gpr)

par(mfrow = c(1,1))


# 18. Jarque-Bera tests
jb.j303.btc <- jarque.bera.test(
  residuals(j303.btc)
)

jb.j303.btc.gpr <- jarque.bera.test(
  residuals(j303.btc.gpr)
)

jb.j303.btc
jb.j303.btc.gpr


# 19. Durbin-Watson tests
dw.j303.btc <- dwtest(j303.btc)

dw.j303.btc.gpr <- dwtest(j303.btc.gpr)

dw.j303.btc
dw.j303.btc.gpr


# 20. Ljung-Box tests
lb.j303.btc <- Box.test(
  residuals(j303.btc),
  lag = 20,
  type = "Ljung-Box"
)

lb.j303.btc.gpr <- Box.test(
  residuals(j303.btc.gpr),
  lag = 20,
  type = "Ljung-Box"
)

lb.j303.btc
lb.j303.btc.gpr


# 21. Breusch-Pagan tests
bp.j303.btc <- bptest(j303.btc)

bp.j303.btc.gpr <- bptest(j303.btc.gpr)

bp.j303.btc
bp.j303.btc.gpr


# 22. Variance inflation factors
vif.j303.btc.gpr <- vif(
  j303.btc.gpr
)

vif.j303.btc.gpr


###############################################################
# Section E: Robust Inference
###############################################################

# 23. Newey-West robust standard errors
nw.j303.btc <- coeftest(
  
  j303.btc,
  
  vcov = NeweyWest(
    
    j303.btc,
    
    prewhite = FALSE
    
  )
  
)


nw.j303.btc.gpr <- coeftest(
  
  j303.btc.gpr,
  
  vcov = NeweyWest(
    
    j303.btc.gpr,
    
    prewhite = FALSE
    
  )
  
)


# 24. Display Newey-West results
nw.j303.btc

nw.j303.btc.gpr


###############################################################
# Section F: Regression Summary
###############################################################

# 25. Regression summary table
regression.summary <- data.frame(
  
  Model = c(
    
    "J303 Volatility ~ BTC Volatility",
    
    "J303 Volatility ~ BTC Volatility + GPR"
    
  ),
  
  BTC_Coefficient = c(
    unname(coef(j303.btc)["BTC_Volatility"]),
    unname(coef(j303.btc.gpr)["BTC_Volatility"])
  ),
  
  GPR_Coefficient = c(
    NA,
    unname(coef(j303.btc.gpr)["GPRD"])
  ),
  
  Adj_R2 = c(
    summary(j303.btc)$adj.r.squared,
    summary(j303.btc.gpr)$adj.r.squared
  ),
  
  OLS_BTC_pvalue = c(
    summary(j303.btc)$coefficients["BTC_Volatility",4],
    summary(j303.btc.gpr)$coefficients["BTC_Volatility",4]
  ),
  
  Final_BTC_pvalue = c(
    nw.j303.btc["BTC_Volatility","Pr(>|t|)"],
    nw.j303.btc.gpr["BTC_Volatility","Pr(>|t|)"]
  )
  
)


regression.summary$Significant_5pct <- ifelse(
  
  ifelse(
    is.na(regression.summary$Final_BTC_pvalue),
    regression.summary$OLS_BTC_pvalue,
    regression.summary$Final_BTC_pvalue
  ) < 0.05,
  
  "Yes",
  
  "No"
  
)


regression.summary$BTC_Coefficient <-
  round(regression.summary$BTC_Coefficient, 4)

regression.summary$GPR_Coefficient <-
  signif(regression.summary$GPR_Coefficient, 4)

regression.summary$Adj_R2 <-
  round(regression.summary$Adj_R2, 4)

regression.summary$OLS_BTC_pvalue <-
  signif(regression.summary$OLS_BTC_pvalue, 4)

regression.summary$Final_BTC_pvalue <-
  signif(regression.summary$Final_BTC_pvalue, 4)

regression.summary


###############################################################
# Section G: Overall Summary
###############################################################

# 26. Overall findings table
overall.summary <- data.frame(
  
  Model = c(
    "J303 Volatility ~ BTC Volatility",
    "J303 Volatility ~ BTC Volatility + GPR"
  ),
  
  Correlation = c(
    unname(btc.j303.cor$estimate),
    unname(btc.j303.cor$estimate)
  ),
  
  Correlation_pvalue = c(
    btc.j303.cor$p.value,
    btc.j303.cor$p.value
  ),
  
  BTC_Coefficient = c(
    unname(coef(j303.btc)["BTC_Volatility"]),
    unname(coef(j303.btc.gpr)["BTC_Volatility"])
  ),
  
  GPR_Coefficient = c(
    NA,
    unname(coef(j303.btc.gpr)["GPRD"])
  ),
  
  Adj_R2 = c(
    summary(j303.btc)$adj.r.squared,
    summary(j303.btc.gpr)$adj.r.squared
  ),
  
  Final_pvalue = c(
    nw.j303.btc["BTC_Volatility","Pr(>|t|)"],
    nw.j303.btc.gpr["BTC_Volatility","Pr(>|t|)"]
  )
  
)

overall.summary$Significant <- ifelse(
  
  overall.summary$Final_pvalue < 0.05,
  
  "Yes",
  
  "No"
  
)

overall.summary$Correlation <-
  round(overall.summary$Correlation, 4)

overall.summary$Correlation_pvalue <-
  signif(overall.summary$Correlation_pvalue, 4)

overall.summary$BTC_Coefficient <-
  round(overall.summary$BTC_Coefficient, 4)

overall.summary$GPR_Coefficient <-
  signif(overall.summary$GPR_Coefficient, 4)

overall.summary$Adj_R2 <-
  round(overall.summary$Adj_R2, 4)

overall.summary$Final_pvalue <-
  signif(overall.summary$Final_pvalue, 4)

overall.summary


