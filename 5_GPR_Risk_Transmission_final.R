#1 Load required packages
library(readxl)
library(dplyr)
library(ggplot2)
library(rugarch)

library(FinTS)
library(tseries)
library(moments)

library(lmtest)
library(sandwich)
library(car)

library(pracma)
library(strucchange)
library(zoo)


#2 Import and prepare data
data <- read_excel(
  "Thesis_Data.xlsx",
  sheet = "Data",
  na="NA"
)

data <- na.omit(data)

btc.returns <- data$BTC_log_returns
market.returns <- data$Index_log_returns
gpr.raw <- data$GPRD


#3 Estimate Bitcoin conditional volatility
btc.spec <- ugarchspec(
  
  variance.model = list(
    model = "eGARCH",
    garchOrder = c(1, 1)
  ),
  
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  
  distribution.model = "std"
  
)

btc.fit <- ugarchfit(
  spec = btc.spec,
  data = btc.returns
)

data$BTC_Volatility <- as.numeric(
  sigma(btc.fit)
)


###############################################################
# Section A: Exploratory Data Analysis
###############################################################

#4 Descriptive statistics
summary(gpr.raw)

mean(gpr.raw)

median(gpr.raw)

sd(gpr.raw)

IQR(gpr.raw)

quantile(
  gpr.raw,
  probs = c(
    0.10,
    0.25,
    0.50,
    0.75,
    0.90,
    0.95,
    0.99
  )
)


#5 K-means clustering
set.seed(123)

kmeans.2 <- kmeans(
  x = matrix(gpr.raw, ncol = 1),
  centers = 2,
  nstart = 100
)

data$GPR_Cluster <- kmeans.2$cluster

sort(kmeans.2$centers[,1])

table(data$GPR_Cluster)

aggregate(
  gpr.raw,
  by = list(
    Cluster = data$GPR_Cluster
  ),
  summary
)


###############################################################
# Section B: Regime Analysis
###############################################################

#6 Define geopolitical risk regimes
data$GPR_Regime <- ifelse(
  data$GPRD < 170,
  "Lower GPR",
  "Elevated GPR"
)

data$GPR_Regime <- factor(
  data$GPR_Regime,
  levels = c(
    "Lower GPR",
    "Elevated GPR"
  )
)

table(data$GPR_Regime)


#7 Split the dataset
low.gpr <- subset(
  data,
  GPR_Regime == "Lower GPR"
)

high.gpr <- subset(
  data,
  GPR_Regime == "Elevated GPR"
)


#8 Descriptive statistics by regime
regime.summary <- data.frame(
  
  Regime = c(
    "Lower GPR",
    "Elevated GPR"
  ),
  
  Mean_BTC_Return = c(
    mean(low.gpr$BTC_log_returns),
    mean(high.gpr$BTC_log_returns)
  ),
  
  SD_BTC_Return = c(
    sd(low.gpr$BTC_log_returns),
    sd(high.gpr$BTC_log_returns)
  ),
  
  Mean_BTC_Volatility = c(
    mean(low.gpr$BTC_Volatility),
    mean(high.gpr$BTC_Volatility)
  ),
  
  SD_BTC_Volatility = c(
    sd(low.gpr$BTC_Volatility),
    sd(high.gpr$BTC_Volatility)
  ),
  
  Mean_GPR = c(
    mean(low.gpr$GPRD),
    mean(high.gpr$GPRD)
  ),
  
  SD_GPR = c(
    sd(low.gpr$GPRD),
    sd(high.gpr$GPRD)
  ),
  
  Observations = c(
    nrow(low.gpr),
    nrow(high.gpr)
  )
  
)

regime.summary[-1] <- round(
  regime.summary[-1],
  4
)

regime.summary


#9 Scatterplots
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
    colour = "red",
    se = TRUE
  ) +
  geom_smooth(
    method = "loess",
    colour = "blue",
    se = TRUE
  ) +
  theme_minimal() +
  labs(
    title = "Bitcoin Volatility vs Geopolitical Risk (Lower GPR)",
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
    colour = "red",
    se = TRUE
  ) +
  geom_smooth(
    method = "loess",
    colour = "blue",
    se = TRUE
  ) +
  theme_minimal() +
  labs(
    title = "Bitcoin Volatility vs Geopolitical Risk (Elevated GPR)",
    x = "Geopolitical Risk Index",
    y = "Conditional Volatility"
  )


#10 Correlation analysis
cor.low <- cor.test(
  low.gpr$BTC_Volatility,
  low.gpr$GPRD
)

cor.high <- cor.test(
  high.gpr$BTC_Volatility,
  high.gpr$GPRD
)

cor.low

cor.high


#11 Fisher's r-to-z test
r.low <- unname(cor.low$estimate)
r.high <- unname(cor.high$estimate)

n.low <- nrow(low.gpr)
n.high <- nrow(high.gpr)

z.low <- 0.5 * log(
  (1 + r.low) /
    (1 - r.low)
)

z.high <- 0.5 * log(
  (1 + r.high) /
    (1 - r.high)
)

SE <- sqrt(
  (1 / (n.low - 3)) +
    (1 / (n.high - 3))
)

z.stat <- (z.low - z.high) / SE

p.value <- 2 * (
  1 - pnorm(abs(z.stat))
)

cat(
  "Fisher z statistic:",
  round(z.stat, 4),
  "\n"
)

if (p.value < 0.0001) {
  
  cat(
    "P-value: < 0.0001\n"
  )
  
} else {
  
  cat(
    "P-value:",
    round(p.value, 4),
    "\n"
  )
  
}


###############################################################
# Section C: Regime Regression Analysis
###############################################################

#12 Regime-specific regression models

# Lower GPR
model.low <- lm(
  BTC_Volatility ~ GPRD,
  data = low.gpr
)

summary(model.low)

# Elevated GPR
model.high <- lm(
  BTC_Volatility ~ GPRD,
  data = high.gpr
)

summary(model.high)


#13 Regression model comparison
regime.comparison <- data.frame(
  
  Regime = c(
    "Lower GPR",
    "Elevated GPR"
  ),
  
  Correlation = c(
    unname(cor.low$estimate),
    unname(cor.high$estimate)
  ),
  
  Intercept = c(
    coef(model.low)[1],
    coef(model.high)[1]
  ),
  
  GPR_Coefficient = c(
    coef(model.low)[2],
    coef(model.high)[2]
  ),
  
  Adj_R2 = c(
    summary(model.low)$adj.r.squared,
    summary(model.high)$adj.r.squared
  ),
  
  Residual_SE = c(
    summary(model.low)$sigma,
    summary(model.high)$sigma
  ),
  
  Model_p_value = c(
    pf(
      summary(model.low)$fstatistic[1],
      summary(model.low)$fstatistic[2],
      summary(model.low)$fstatistic[3],
      lower.tail = FALSE
    ),
    pf(
      summary(model.high)$fstatistic[1],
      summary(model.high)$fstatistic[2],
      summary(model.high)$fstatistic[3],
      lower.tail = FALSE
    )
  )
  
)


#14 Format regression comparison table
regime.comparison$Correlation <-
  round(regime.comparison$Correlation, 4)

regime.comparison$Intercept <-
  round(regime.comparison$Intercept, 4)

regime.comparison$GPR_Coefficient <-
  signif(regime.comparison$GPR_Coefficient, 4)

regime.comparison$Adj_R2 <-
  round(regime.comparison$Adj_R2, 4)

regime.comparison$Residual_SE <-
  round(regime.comparison$Residual_SE, 5)

regime.comparison$Model_p_value <-
  signif(regime.comparison$Model_p_value, 4)

regime.comparison


#15 Regression diagnostic tests

# Diagnostic plots
par(mfrow = c(2,2))

plot(model.low)

plot(model.high)

par(mfrow = c(1,1))


# Jarque-Bera tests
jb.low <- jarque.bera.test(
  residuals(model.low)
)

jb.high <- jarque.bera.test(
  residuals(model.high)
)

jb.low
jb.high


# Durbin-Watson tests
dw.low <- dwtest(model.low)

dw.high <- dwtest(model.high)

dw.low
dw.high


# Breusch-Pagan tests
bp.low <- bptest(model.low)

bp.high <- bptest(model.high)

bp.low
bp.high


#16 Newey-West robust inference for regime models
nw.low <- coeftest(
  
  model.low,
  
  vcov = NeweyWest(
    
    model.low,
    
    prewhite = FALSE
    
  )
  
)

nw.high <- coeftest(
  
  model.high,
  
  vcov = NeweyWest(
    
    model.high,
    
    prewhite = FALSE
    
  )
  
)

nw.low
nw.high


#17 Robustness check: Interaction regression
interaction.model <- lm(
  
  BTC_Volatility ~
    
    GPRD *
    GPR_Regime,
  
  data = data
  
)

summary(interaction.model)


#18 Interaction model comparison
interaction.comparison <- data.frame(
  
  Model = "Interaction Regression",
  
  LogLikelihood = as.numeric(
    logLik(interaction.model)
  ),
  
  Adj_R2 = summary(
    interaction.model
  )$adj.r.squared,
  
  AIC = AIC(
    interaction.model
  ),
  
  BIC = BIC(
    interaction.model
  ),
  
  Residual_SE = summary(
    interaction.model
  )$sigma,
  
  F_Statistic = summary(
    interaction.model
  )$fstatistic[1],
  
  Model_p_value = pf(
    
    summary(interaction.model)$fstatistic[1],
    
    summary(interaction.model)$fstatistic[2],
    
    summary(interaction.model)$fstatistic[3],
    
    lower.tail = FALSE
    
  )
  
)

interaction.comparison$LogLikelihood <-
  round(interaction.comparison$LogLikelihood,2)

interaction.comparison$Adj_R2 <-
  round(interaction.comparison$Adj_R2,4)

interaction.comparison$AIC <-
  round(interaction.comparison$AIC,2)

interaction.comparison$BIC <-
  round(interaction.comparison$BIC,2)

interaction.comparison$Residual_SE <-
  round(interaction.comparison$Residual_SE,4)

interaction.comparison$F_Statistic <-
  round(interaction.comparison$F_Statistic,2)

interaction.comparison$Model_p_value <-
  signif(interaction.comparison$Model_p_value,4)

interaction.comparison


#19 Newey-West robust interaction model summary
interaction.nw <- coeftest(
  
  interaction.model,
  
  vcov = NeweyWest(
    
    interaction.model,
    
    prewhite = FALSE
    
  )
  
)

interaction.nw


#20 Interaction model summary
interaction.summary <- data.frame(
  
  Variable = rownames(
    interaction.nw
  ),
  
  Estimate = interaction.nw[,1],
  
  Robust_SE = interaction.nw[,2],
  
  t_value = interaction.nw[,3],
  
  P_Value = interaction.nw[,4]
  
)

interaction.summary$Variable <- c(
  
  "Intercept",
  
  "GPR",
  
  "Elevated GPR",
  
  "GPR × Elevated GPR"
  
)

interaction.summary$Estimate <-
  signif(interaction.summary$Estimate,4)

interaction.summary$Robust_SE <-
  signif(interaction.summary$Robust_SE,4)

interaction.summary$t_value <-
  round(interaction.summary$t_value,3)

interaction.summary$P_Value <-
  signif(interaction.summary$P_Value,4)

interaction.summary$Significant <- ifelse(
  
  interaction.summary$P_Value < 0.05,
  
  "Yes",
  
  "No"
  
)

interaction.summary


###############################################################
# Section D: Summary of Regime Analysis
###############################################################

#21 Regime analysis summary
regime.results <- data.frame(
  
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
  
  BTC_Coefficient = c(
    coef(model.low)[2],
    coef(model.high)[2]
  ),
  
  Adj_R2 = c(
    summary(model.low)$adj.r.squared,
    summary(model.high)$adj.r.squared
  ),
  
  Residual_SE = c(
    summary(model.low)$sigma,
    summary(model.high)$sigma
  ),
  
  NeweyWest_P_Value = c(
    nw.low["GPRD","Pr(>|t|)"],
    nw.high["GPRD","Pr(>|t|)"]
  )
  
)


#22 Format final summary table
regime.results$Sample_Size <-
  round(regime.results$Sample_Size,0)

regime.results$Correlation <-
  round(regime.results$Correlation,4)

regime.results$BTC_Coefficient <-
  signif(regime.results$BTC_Coefficient,4)

regime.results$Adj_R2 <-
  round(regime.results$Adj_R2,4)

regime.results$Residual_SE <-
  round(regime.results$Residual_SE,4)

regime.results$NeweyWest_P_Value <-
  signif(regime.results$NeweyWest_P_Value,4)

regime.results$Significant <- ifelse(
  
  regime.results$NeweyWest_P_Value < 0.05,
  
  "Yes",
  
  "No"
  
)

print(regime.results)

