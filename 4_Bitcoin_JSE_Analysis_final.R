#1 Load required packages
library(readxl)
library(dplyr)
library(ggplot2)
library(lmtest)
library(car)
library(sandwich)
library(tseries)


#2 Import data
data <- read_excel(
  "Thesis_Data.xlsx",
  sheet = "Data",
  na="NA"
)

data <- na.omit(data)


#3 Create variables
btc.returns <- data$BTC_log_returns

market.returns <- data$Index_log_returns

gpr.raw <- data$GPRD


###############################################################
# Section A: Exploratory Analysis
###############################################################

#4 Correlation matrix
correlation.matrix <- cor(
  
  data[, c(
    "BTC_log_returns",
    "Index_log_returns",
    "GPRD"
  )],
  
  method = "pearson"
  
)

round(
  correlation.matrix,
  4
)


#5 Scatterplots
ggplot(
  data,
  aes(
    x = BTC_log_returns,
    y = Index_log_returns
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
    title = "Bitcoin Returns vs JSE Returns",
    x = "Bitcoin Log Returns",
    y = "JSE Log Returns"
  )


#6 Pearson correlation test
btc.jse.cor <- cor.test(
  
  btc.returns,
  
  market.returns,
  
  method = "pearson"
  
)

btc.jse.cor


#7 Correlation summary table
correlation.summary <- data.frame(
  
  Relationship = "Bitcoin vs JSE",
  
  Correlation = unname(
    btc.jse.cor$estimate
  ),
  
  P_Value = btc.jse.cor$p.value
  
)

correlation.summary$Correlation <-
  round(correlation.summary$Correlation, 4)

correlation.summary$P_Value <-
  signif(correlation.summary$P_Value, 4)

correlation.summary


###############################################################
# Section B: Regression Analysis
###############################################################

#8 Estimate regression models
jse.btc <- lm(
  
  market.returns ~
    
    btc.returns,
  
  data = data
  
)


jse.btc.gpr <- lm(
  
  market.returns ~
    
    btc.returns +
    
    gpr.raw,
  
  data = data
  
)


#9 Model summaries
summary(jse.btc)

summary(jse.btc.gpr)


#10 95% confidence intervals
confint(jse.btc)

confint(jse.btc.gpr)


###############################################################
# Section C: Model Comparison
###############################################################

#11 Compare regression models
model.comparison <- data.frame(
  
  Model = c(
    
    "JSE ~ BTC",
    
    "JSE ~ BTC + GPR"
    
  ),
  
  LogLikelihood = c(
    as.numeric(logLik(jse.btc)),
    as.numeric(logLik(jse.btc.gpr))
  ),
  
  Adj_R2 = c(
    summary(jse.btc)$adj.r.squared,
    summary(jse.btc.gpr)$adj.r.squared
  ),
  
  AIC = c(
    AIC(jse.btc),
    AIC(jse.btc.gpr)
  ),
  
  BIC = c(
    BIC(jse.btc),
    BIC(jse.btc.gpr)
  ),
  
  Residual_SE = c(
    summary(jse.btc)$sigma,
    summary(jse.btc.gpr)$sigma
  ),
  
  F_Statistic = c(
    summary(jse.btc)$fstatistic[1],
    summary(jse.btc.gpr)$fstatistic[1]
  ),
  
  Model_pvalue = c(
    
    pf(
      summary(jse.btc)$fstatistic[1],
      summary(jse.btc)$fstatistic[2],
      summary(jse.btc)$fstatistic[3],
      lower.tail = FALSE
    ),
    
    pf(
      summary(jse.btc.gpr)$fstatistic[1],
      summary(jse.btc.gpr)$fstatistic[2],
      summary(jse.btc.gpr)$fstatistic[3],
      lower.tail = FALSE
    )
    
  )
  
)

model.comparison$Model_pvalue <-
  signif(model.comparison$Model_pvalue, 4)

model.comparison


#12 Nested model comparison
model.comparison.test <- anova(
  
  jse.btc,
  
  jse.btc.gpr
  
)

model.comparison.test


###############################################################
# Section D: Model Diagnostics
###############################################################

#13 Diagnostic plots
par(mfrow = c(2,2))

plot(jse.btc)

plot(jse.btc.gpr)

par(mfrow = c(1,1))


#14 Jarque-Bera tests
jb.jse.btc <- jarque.bera.test(
  residuals(jse.btc)
)

jb.jse.btc.gpr <- jarque.bera.test(
  residuals(jse.btc.gpr)
)

jb.jse.btc
jb.jse.btc.gpr


#15 Durbin-Watson tests
dw.jse.btc <- dwtest(jse.btc)

dw.jse.btc.gpr <- dwtest(jse.btc.gpr)

dw.jse.btc
dw.jse.btc.gpr


#16 Breusch-Pagan tests
bp.jse.btc <- bptest(jse.btc)

bp.jse.btc.gpr <- bptest(jse.btc.gpr)

bp.jse.btc
bp.jse.btc.gpr


#17 Variance inflation factors
vif.jse.btc.gpr <- vif(
  jse.btc.gpr
)

vif.jse.btc.gpr


###############################################################
# Section E: Robust Inference
###############################################################

#18 Newey-West robust standard errors
nw.jse.btc <- coeftest(
  
  jse.btc,
  
  vcov = NeweyWest(
    
    jse.btc,
    
    prewhite = FALSE
    
  )
  
)


nw.jse.btc.gpr <- coeftest(
  
  jse.btc.gpr,
  
  vcov = NeweyWest(
    
    jse.btc.gpr,
    
    prewhite = FALSE
    
  )
  
)


#19 Display Newey-West results
nw.jse.btc

nw.jse.btc.gpr


###############################################################
# Section F: Regression Summary
###############################################################

#20 Regression summary table
regression.summary <- data.frame(
  
  Model = c(
    
    "JSE ~ BTC",
    
    "JSE ~ BTC + GPR"
    
  ),
  
  BTC_Coefficient = c(
    unname(coef(jse.btc)["btc.returns"]),
    unname(coef(jse.btc.gpr)["btc.returns"])
  ),
  
  GPR_Coefficient = c(
    NA,
    unname(coef(jse.btc.gpr)["gpr.raw"])
  ),
  
  Adj_R2 = c(
    summary(jse.btc)$adj.r.squared,
    summary(jse.btc.gpr)$adj.r.squared
  ),
  
  OLS_BTC_pvalue = c(
    summary(jse.btc)$coefficients["btc.returns",4],
    summary(jse.btc.gpr)$coefficients["btc.returns",4]
  ),
  
  Final_BTC_pvalue = c(
    nw.jse.btc["btc.returns","Pr(>|t|)"],
    nw.jse.btc.gpr["btc.returns","Pr(>|t|)"]
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

#21 Overall findings table
overall.summary <- data.frame(
  
  Model = c(
    "JSE ~ BTC",
    "JSE ~ BTC + GPR"
  ),
  
  Correlation = c(
    unname(btc.jse.cor$estimate),
    unname(btc.jse.cor$estimate)
  ),
  
  Correlation_pvalue = c(
    btc.jse.cor$p.value,
    btc.jse.cor$p.value
  ),
  
  BTC_Coefficient = c(
    unname(coef(jse.btc)["btc.returns"]),
    unname(coef(jse.btc.gpr)["btc.returns"])
  ),
  
  GPR_Coefficient = c(
    NA,
    unname(coef(jse.btc.gpr)["gpr.raw"])
  ),
  
  Adj_R2 = c(
    summary(jse.btc)$adj.r.squared,
    summary(jse.btc.gpr)$adj.r.squared
  ),
  
  Final_pvalue = c(
    nw.jse.btc["btc.returns","Pr(>|t|)"],
    nw.jse.btc.gpr["btc.returns","Pr(>|t|)"]
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
