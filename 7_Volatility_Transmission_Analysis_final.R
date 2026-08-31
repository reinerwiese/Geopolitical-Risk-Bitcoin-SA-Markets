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


#2 Import and prepare data
data <- read_excel(
  "Thesis_Data.xlsx",
  sheet = "Data"
)

data <- na.omit(data)

data$Date <- as.Date(data$Date)

btc.returns <- data$BTC_log_returns
j303.returns <- data$Index_log_returns


###############################################################
# Section A: Analysis by Geopolitical Risk Regime
###############################################################

#3 Estimate Bitcoin conditional volatility
btc.spec <- ugarchspec(
  
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

btc.fit <- ugarchfit(
  
  spec = btc.spec,
  
  data = btc.returns
  
)

data$BTC_Volatility <- as.numeric(
  sigma(btc.fit)
)


#4 Estimate J303 conditional volatility
j303.spec <- ugarchspec(
  
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

j303.fit <- ugarchfit(
  
  spec = j303.spec,
  
  data = j303.returns
  
)

data$J303_Volatility <- as.numeric(
  sigma(j303.fit)
)


#5 Baseline relationship between Bitcoin and J303 volatility
overall.cor <- cor.test(
  
  data$BTC_Volatility,
  
  data$J303_Volatility
  
)

overall.cor

overall.model <- lm(
  
  J303_Volatility ~
    
    BTC_Volatility,
  
  data = data
  
)

summary(overall.model)


#6 Baseline regression diagnostics

# Diagnostic plots
par(mfrow = c(2,2))

plot(overall.model)

par(mfrow = c(1,1))


# Jarque-Bera test
jb.overall <- jarque.bera.test(
  residuals(overall.model)
)

jb.overall


# Durbin-Watson test
dw.overall <- dwtest(
  overall.model
)

dw.overall


# Breusch-Pagan test
bp.overall <- bptest(
  overall.model
)

bp.overall


# Newey-West robust inference
nw.overall <- coeftest(
  
  overall.model,
  
  vcov = NeweyWest(
    overall.model,
    prewhite = FALSE
  )
  
)

nw.overall


#7 Baseline summary table
baseline.summary <- data.frame(
  
  Correlation =
    unname(overall.cor$estimate),
  
  BTC_Coefficient =
    coef(overall.model)[2],
  
  Adj_R2 =
    summary(overall.model)$adj.r.squared,
  
  Residual_SE =
    summary(overall.model)$sigma,
  
  NeweyWest_P_Value =
    nw.overall[
      "BTC_Volatility",
      "Pr(>|t|)"
    ]
  
)

baseline.summary$Correlation <-
  round(
    baseline.summary$Correlation,
    4
  )

baseline.summary$BTC_Coefficient <-
  signif(
    baseline.summary$BTC_Coefficient,
    4
  )

baseline.summary$Adj_R2 <-
  round(
    baseline.summary$Adj_R2,
    4
  )

baseline.summary$Residual_SE <-
  round(
    baseline.summary$Residual_SE,
    5
  )

baseline.summary$NeweyWest_P_Value <-
  signif(
    baseline.summary$NeweyWest_P_Value,
    4
  )

baseline.summary$Significant <- ifelse(
  
  baseline.summary$NeweyWest_P_Value < 0.05,
  
  "Yes",
  
  "No"
  
)

print(baseline.summary)


#8 Define geopolitical risk regimes
data$GPR_Regime <- ifelse(
  
  data$GPRD < 170,
  
  "Lower GPR",
  
  "Elevated GPR"
  
)

table(data$GPR_Regime)


#9 Split data by regime
lower.gpr <- subset(
  
  data,
  
  GPR_Regime == "Lower GPR"
  
)

elevated.gpr <- subset(
  
  data,
  
  GPR_Regime == "Elevated GPR"
  
)


#10 Summary statistics by regime
regime.statistics <- data.frame(
  
  Regime = c(
    "Lower GPR",
    "Elevated GPR"
  ),
  
  Sample_Size = c(
    nrow(lower.gpr),
    nrow(elevated.gpr)
  ),
  
  Mean_GPR = c(
    mean(lower.gpr$GPRD),
    mean(elevated.gpr$GPRD)
  ),
  
  SD_GPR = c(
    sd(lower.gpr$GPRD),
    sd(elevated.gpr$GPRD)
  ),
  
  Mean_BTC_Volatility = c(
    mean(lower.gpr$BTC_Volatility),
    mean(elevated.gpr$BTC_Volatility)
  ),
  
  SD_BTC_Volatility = c(
    sd(lower.gpr$BTC_Volatility),
    sd(elevated.gpr$BTC_Volatility)
  ),
  
  Mean_J303_Volatility = c(
    mean(lower.gpr$J303_Volatility),
    mean(elevated.gpr$J303_Volatility)
  ),
  
  SD_J303_Volatility = c(
    sd(lower.gpr$J303_Volatility),
    sd(elevated.gpr$J303_Volatility)
  )
  
)

regime.statistics[-1] <- round(
  regime.statistics[-1],
  4
)

print(regime.statistics)


#11 Correlation analysis
cor.lower <- cor.test(
  
  lower.gpr$BTC_Volatility,
  
  lower.gpr$J303_Volatility
  
)

cor.elevated <- cor.test(
  
  elevated.gpr$BTC_Volatility,
  
  elevated.gpr$J303_Volatility
  
)

cor.lower

cor.elevated


#12 Correlation summary table
correlation.summary <- data.frame(
  
  Regime = c(
    "Lower GPR",
    "Elevated GPR"
  ),
  
  Sample_Size = c(
    nrow(lower.gpr),
    nrow(elevated.gpr)
  ),
  
  Correlation = c(
    unname(cor.lower$estimate),
    unname(cor.elevated$estimate)
  ),
  
  Correlation_P_Value = c(
    cor.lower$p.value,
    cor.elevated$p.value
  )
  
)

correlation.summary$Correlation <-
  round(correlation.summary$Correlation, 4)

correlation.summary$Correlation_P_Value <-
  signif(correlation.summary$Correlation_P_Value, 4)

correlation.summary$Significant <- ifelse(
  
  correlation.summary$Correlation_P_Value < 0.05,
  
  "Yes",
  
  "No"
  
)

print(correlation.summary)


#13 Regime-specific regression models

# Lower GPR
model.lower <- lm(
  
  J303_Volatility ~ BTC_Volatility,
  
  data = lower.gpr
  
)

summary(model.lower)


# Elevated GPR
model.elevated <- lm(
  
  J303_Volatility ~ BTC_Volatility,
  
  data = elevated.gpr
  
)

summary(model.elevated)


#14 Regression diagnostic tests

# Diagnostic plots
par(mfrow = c(2,2))

plot(model.lower)

plot(model.elevated)

par(mfrow = c(1,1))


# Jarque-Bera tests
jb.lower <- jarque.bera.test(
  residuals(model.lower)
)

jb.elevated <- jarque.bera.test(
  residuals(model.elevated)
)

jb.lower

jb.elevated


# Durbin-Watson tests
dw.lower <- dwtest(model.lower)

dw.elevated <- dwtest(model.elevated)

dw.lower

dw.elevated


# Breusch-Pagan tests
bp.lower <- bptest(model.lower)

bp.elevated <- bptest(model.elevated)

bp.lower

bp.elevated


# Newey-West robust inference
nw.lower <- coeftest(
  
  model.lower,
  
  vcov = NeweyWest(
    
    model.lower,
    
    prewhite = FALSE
    
  )
  
)

nw.elevated <- coeftest(
  
  model.elevated,
  
  vcov = NeweyWest(
    
    model.elevated,
    
    prewhite = FALSE
    
  )
  
)

nw.lower

nw.elevated


#15 Regression summary table
regression.summary <- data.frame(
  
  Regime = c(
    "Lower GPR",
    "Elevated GPR"
  ),
  
  Sample_Size = c(
    nrow(lower.gpr),
    nrow(elevated.gpr)
  ),
  
  Correlation = c(
    unname(cor.lower$estimate),
    unname(cor.elevated$estimate)
  ),
  
  BTC_Coefficient = c(
    coef(model.lower)[2],
    coef(model.elevated)[2]
  ),
  
  Adj_R2 = c(
    summary(model.lower)$adj.r.squared,
    summary(model.elevated)$adj.r.squared
  ),
  
  Residual_SE = c(
    summary(model.lower)$sigma,
    summary(model.elevated)$sigma
  ),
  
  NeweyWest_P_Value = c(
    nw.lower["BTC_Volatility","Pr(>|t|)"],
    nw.elevated["BTC_Volatility","Pr(>|t|)"]
  )
  
)

regression.summary$Correlation <-
  round(regression.summary$Correlation, 4)

regression.summary$BTC_Coefficient <-
  signif(regression.summary$BTC_Coefficient, 4)

regression.summary$Adj_R2 <-
  round(regression.summary$Adj_R2, 4)

regression.summary$Residual_SE <-
  round(regression.summary$Residual_SE, 5)

regression.summary$NeweyWest_P_Value <-
  signif(regression.summary$NeweyWest_P_Value, 4)

regression.summary$Significant <- ifelse(
  
  regression.summary$NeweyWest_P_Value < 0.05,
  
  "Yes",
  
  "No"
  
)

print(regression.summary)


#16 Fisher r-to-z test
z.lower <- atanh(
  cor.lower$estimate
)

z.elevated <- atanh(
  cor.elevated$estimate
)

z.statistic <- (
  z.lower - z.elevated
) /
  sqrt(
    1 / (nrow(lower.gpr) - 3) +
      1 / (nrow(elevated.gpr) - 3)
  )

p.value <- 2 * (
  1 -
    pnorm(abs(z.statistic))
)

cat(
  "Fisher z statistic =",
  round(z.statistic, 4),
  "\n"
)

cat(
  "p-value =",
  signif(p.value, 4),
  "\n"
)

if(p.value < 0.05){
  
  cat(
    "Conclusion: The correlations differ significantly between geopolitical risk regimes.\n"
  )
  
} else {
  
  cat(
    "Conclusion: No statistically significant difference exists between the correlations across geopolitical risk regimes.\n"
  )
  
}


#17 Interaction model
data$GPR_Regime <- factor(
  
  data$GPR_Regime,
  
  levels = c(
    "Lower GPR",
    "Elevated GPR"
  )
  
)

interaction.model <- lm(
  
  J303_Volatility ~
    BTC_Volatility * GPR_Regime,
  
  data = data
  
)

summary(interaction.model)


#18 Interaction model diagnostics

# Diagnostic plots
par(mfrow = c(2,2))

plot(interaction.model)

par(mfrow = c(1,1))


# Jarque-Bera test
jb.interaction <- jarque.bera.test(
  residuals(interaction.model)
)

jb.interaction


# Durbin-Watson test
dw.interaction <- dwtest(
  interaction.model
)

dw.interaction


# Breusch-Pagan test
bp.interaction <- bptest(
  interaction.model
)

bp.interaction


# Newey-West robust inference
interaction.nw <- coeftest(
  
  interaction.model,
  
  vcov = NeweyWest(
    
    interaction.model,
    
    prewhite = FALSE
    
  )
  
)

interaction.nw


#19 Overall regime summary
overall.summary <- data.frame(
  
  Regime = c(
    "Lower GPR",
    "Elevated GPR"
  ),
  
  Correlation = c(
    unname(cor.lower$estimate),
    unname(cor.elevated$estimate)
  ),
  
  NeweyWest_P_Value = c(
    nw.lower["BTC_Volatility","Pr(>|t|)"],
    nw.elevated["BTC_Volatility","Pr(>|t|)"]
  ),
  
  Significant = c(
    ifelse(
      nw.lower["BTC_Volatility","Pr(>|t|)"] < 0.05,
      "Yes",
      "No"
    ),
    ifelse(
      nw.elevated["BTC_Volatility","Pr(>|t|)"] < 0.05,
      "Yes",
      "No"
    )
  )
  
)

overall.summary$Correlation <-
  round(overall.summary$Correlation, 4)

overall.summary$NeweyWest_P_Value <-
  signif(overall.summary$NeweyWest_P_Value, 4)

print(overall.summary)


###############################################################
# Section B: Analysis During Major Geopolitical Events
###############################################################

#20 Define event windows
events <- data.frame(
  
  Event = c(
    "Russia-Ukraine Conflict",
    "Israel-Hamas Conflict",
    "Iran-Israel Conflict"
  ),
  
  Start_Date = as.Date(c(
    "2022-02-07",
    "2023-09-20",
    "2025-02-17"
  )),
  
  End_Date = as.Date(c(
    "2022-04-05",
    "2023-11-17",
    "2026-04-19"
  ))
  
)

print(events)


#21 Split data into event windows
ukraine <- subset(
  
  data,
  
  Date >= events$Start_Date[1] &
    Date <= events$End_Date[1]
  
)

hamas <- subset(
  
  data,
  
  Date >= events$Start_Date[2] &
    Date <= events$End_Date[2]
  
)

iran <- subset(
  
  data,
  
  Date >= events$Start_Date[3] &
    Date <= events$End_Date[3]
  
)


#22 Summary statistics by event
event.statistics <- data.frame(
  
  Event = c(
    "Russia-Ukraine Conflict",
    "Israel-Hamas Conflict",
    "Iran-Israel Conflict"
  ),
  
  Sample_Size = c(
    nrow(ukraine),
    nrow(hamas),
    nrow(iran)
  ),
  
  Mean_GPR = c(
    mean(ukraine$GPRD),
    mean(hamas$GPRD),
    mean(iran$GPRD)
  ),
  
  SD_GPR = c(
    sd(ukraine$GPRD),
    sd(hamas$GPRD),
    sd(iran$GPRD)
  ),
  
  Mean_BTC_Volatility = c(
    mean(ukraine$BTC_Volatility),
    mean(hamas$BTC_Volatility),
    mean(iran$BTC_Volatility)
  ),
  
  SD_BTC_Volatility = c(
    sd(ukraine$BTC_Volatility),
    sd(hamas$BTC_Volatility),
    sd(iran$BTC_Volatility)
  ),
  
  Mean_J303_Volatility = c(
    mean(ukraine$J303_Volatility),
    mean(hamas$J303_Volatility),
    mean(iran$J303_Volatility)
  ),
  
  SD_J303_Volatility = c(
    sd(ukraine$J303_Volatility),
    sd(hamas$J303_Volatility),
    sd(iran$J303_Volatility)
  )
  
)

event.statistics[-1] <- round(
  event.statistics[-1],
  4
)

print(event.statistics)


#23 Correlation analysis
cor.ukraine <- cor.test(
  
  ukraine$BTC_Volatility,
  
  ukraine$J303_Volatility
  
)

cor.hamas <- cor.test(
  
  hamas$BTC_Volatility,
  
  hamas$J303_Volatility
  
)

cor.iran <- cor.test(
  
  iran$BTC_Volatility,
  
  iran$J303_Volatility
  
)

cor.ukraine

cor.hamas

cor.iran


#24 Correlation summary table
event.correlation <- data.frame(
  
  Event = c(
    "Russia-Ukraine Conflict",
    "Israel-Hamas Conflict",
    "Iran-Israel Conflict"
  ),
  
  Sample_Size = c(
    nrow(ukraine),
    nrow(hamas),
    nrow(iran)
  ),
  
  Correlation = c(
    unname(cor.ukraine$estimate),
    unname(cor.hamas$estimate),
    unname(cor.iran$estimate)
  ),
  
  Correlation_P_Value = c(
    cor.ukraine$p.value,
    cor.hamas$p.value,
    cor.iran$p.value
  )
  
)

event.correlation$Correlation <-
  round(event.correlation$Correlation, 4)

event.correlation$Correlation_P_Value <-
  signif(event.correlation$Correlation_P_Value, 4)

event.correlation$Significant <- ifelse(
  
  event.correlation$Correlation_P_Value < 0.05,
  
  "Yes",
  
  "No"
  
)

print(event.correlation)


#25 Event-specific regression models

# Russia-Ukraine Conflict
model.ukraine <- lm(
  
  J303_Volatility ~ BTC_Volatility,
  
  data = ukraine
  
)

summary(model.ukraine)


# Israel-Hamas Conflict
model.hamas <- lm(
  
  J303_Volatility ~ BTC_Volatility,
  
  data = hamas
  
)

summary(model.hamas)


# Iran-Israel Conflict
model.iran <- lm(
  
  J303_Volatility ~ BTC_Volatility,
  
  data = iran
  
)

summary(model.iran)


#26 Regression diagnostic tests

# Diagnostic plots
par(mfrow = c(2,2))

plot(model.ukraine)

plot(model.hamas)

plot(model.iran)

par(mfrow = c(1,1))


# Jarque-Bera tests
jb.ukraine <- jarque.bera.test(
  
  residuals(model.ukraine)
  
)

jb.hamas <- jarque.bera.test(
  
  residuals(model.hamas)
  
)

jb.iran <- jarque.bera.test(
  
  residuals(model.iran)
  
)

jb.ukraine

jb.hamas

jb.iran


# Durbin-Watson tests
dw.ukraine <- dwtest(
  model.ukraine
)

dw.hamas <- dwtest(
  model.hamas
)

dw.iran <- dwtest(
  model.iran
)

dw.ukraine

dw.hamas

dw.iran


# Breusch-Pagan tests
bp.ukraine <- bptest(
  model.ukraine
)

bp.hamas <- bptest(
  model.hamas
)

bp.iran <- bptest(
  model.iran
)

bp.ukraine

bp.hamas

bp.iran


# Newey-West robust inference
nw.ukraine <- coeftest(
  
  model.ukraine,
  
  vcov = NeweyWest(
    model.ukraine,
    prewhite = FALSE
  )
  
)

nw.hamas <- coeftest(
  
  model.hamas,
  
  vcov = NeweyWest(
    model.hamas,
    prewhite = FALSE
  )
  
)

nw.iran <- coeftest(
  
  model.iran,
  
  vcov = NeweyWest(
    model.iran,
    prewhite = FALSE
  )
  
)

nw.ukraine

nw.hamas

nw.iran


#27 Regression summary table
event.regression <- data.frame(
  
  Event = c(
    "Russia-Ukraine Conflict",
    "Israel-Hamas Conflict",
    "Iran-Israel Conflict"
  ),
  
  Sample_Size = c(
    nrow(ukraine),
    nrow(hamas),
    nrow(iran)
  ),
  
  Correlation = c(
    unname(cor.ukraine$estimate),
    unname(cor.hamas$estimate),
    unname(cor.iran$estimate)
  ),
  
  BTC_Coefficient = c(
    coef(model.ukraine)[2],
    coef(model.hamas)[2],
    coef(model.iran)[2]
  ),
  
  Adj_R2 = c(
    summary(model.ukraine)$adj.r.squared,
    summary(model.hamas)$adj.r.squared,
    summary(model.iran)$adj.r.squared
  ),
  
  Residual_SE = c(
    summary(model.ukraine)$sigma,
    summary(model.hamas)$sigma,
    summary(model.iran)$sigma
  ),
  
  NeweyWest_P_Value = c(
    nw.ukraine["BTC_Volatility","Pr(>|t|)"],
    nw.hamas["BTC_Volatility","Pr(>|t|)"],
    nw.iran["BTC_Volatility","Pr(>|t|)"]
  )
  
)

event.regression$Correlation <-
  round(event.regression$Correlation, 4)

event.regression$BTC_Coefficient <-
  signif(event.regression$BTC_Coefficient, 4)

event.regression$Adj_R2 <-
  round(event.regression$Adj_R2, 4)

event.regression$Residual_SE <-
  round(event.regression$Residual_SE, 5)

event.regression$NeweyWest_P_Value <-
  signif(event.regression$NeweyWest_P_Value, 4)

event.regression$Significant <- ifelse(
  
  event.regression$NeweyWest_P_Value < 0.05,
  
  "Yes",
  
  "No"
  
)

print(event.regression)


#28 Fisher r-to-z comparison tests

compare.correlations <- function(
    
  r1,
  n1,
  r2,
  n2
  
){
  
  z1 <- atanh(r1)
  
  z2 <- atanh(r2)
  
  se <- sqrt(
    1 / (n1 - 3) +
      1 / (n2 - 3)
  )
  
  z <- (z1 - z2) / se
  
  p <- 2 * (
    1 -
      pnorm(abs(z))
  )
  
  c(
    Z_Statistic = z,
    P_Value = p
  )
  
}


ukraine.hamas <- compare.correlations(
  
  cor.ukraine$estimate,
  nrow(ukraine),
  
  cor.hamas$estimate,
  nrow(hamas)
  
)

ukraine.iran <- compare.correlations(
  
  cor.ukraine$estimate,
  nrow(ukraine),
  
  cor.iran$estimate,
  nrow(iran)
  
)

hamas.iran <- compare.correlations(
  
  cor.hamas$estimate,
  nrow(hamas),
  
  cor.iran$estimate,
  nrow(iran)
  
)

comparison.summary <- data.frame(
  
  Comparison = c(
    
    "Russia-Ukraine Conflict vs Israel-Hamas Conflict",
    
    "Russia-Ukraine Conflict vs Iran-Israel Conflict",
    
    "Israel-Hamas Conflict vs Iran-Israel Conflict"
    
  ),
  
  Z_Statistic = c(
    
    ukraine.hamas[1],
    
    ukraine.iran[1],
    
    hamas.iran[1]
    
  ),
  
  P_Value = c(
    
    ukraine.hamas[2],
    
    ukraine.iran[2],
    
    hamas.iran[2]
    
  )
  
)

comparison.summary$Z_Statistic <-
  round(
    comparison.summary$Z_Statistic,
    4
  )

comparison.summary$P_Value <-
  signif(
    comparison.summary$P_Value,
    4
  )

comparison.summary$Significant <- ifelse(
  
  comparison.summary$P_Value < 0.05,
  
  "Yes",
  
  "No"
  
)

print(comparison.summary)


#29 Overall event summary
event.overall <- data.frame(
  
  Event = c(
    "Russia-Ukraine Conflict",
    "Israel-Hamas Conflict",
    "Iran-Israel Conflict"
  ),
  
  Correlation = c(
    unname(cor.ukraine$estimate),
    unname(cor.hamas$estimate),
    unname(cor.iran$estimate)
  ),
  
  NeweyWest_P_Value = c(
    nw.ukraine["BTC_Volatility","Pr(>|t|)"],
    nw.hamas["BTC_Volatility","Pr(>|t|)"],
    nw.iran["BTC_Volatility","Pr(>|t|)"]
  ),
  
  Significant = c(
    ifelse(
      nw.ukraine["BTC_Volatility","Pr(>|t|)"] < 0.05,
      "Yes",
      "No"
    ),
    ifelse(
      nw.hamas["BTC_Volatility","Pr(>|t|)"] < 0.05,
      "Yes",
      "No"
    ),
    ifelse(
      nw.iran["BTC_Volatility","Pr(>|t|)"] < 0.05,
      "Yes",
      "No"
    )
  )
  
)

event.overall$Correlation <-
  round(event.overall$Correlation,4)

event.overall$NeweyWest_P_Value <-
  signif(event.overall$NeweyWest_P_Value,4)

print(event.overall)


###############################################################
# Section C: Comparative Analysis
###############################################################

#30 Correlation comparison plot
comparison.plot <- data.frame(
  
  Analysis = factor(
    
    c(
      "Overall",
      "Lower GPR",
      "Elevated GPR",
      "Russia-\nUkraine",
      "Israel-\nHamas",
      "Iran-\nIsrael"
    ),
    
    levels = c(
      "Overall",
      "Lower GPR",
      "Elevated GPR",
      "Russia-\nUkraine",
      "Israel-\nHamas",
      "Iran-\nIsrael"
    )
    
  ),
  
  Correlation = c(
    
    overall.cor$estimate,
    
    cor.lower$estimate,
    
    cor.elevated$estimate,
    
    cor.ukraine$estimate,
    
    cor.hamas$estimate,
    
    cor.iran$estimate
    
  ),
  
  Group = c(
    
    "Overall",
    
    "Regime",
    
    "Regime",
    
    "Conflict",
    
    "Conflict",
    
    "Conflict"
    
  )
  
)


ggplot(
  comparison.plot,
  aes(
    x = Analysis,
    y = Correlation,
    fill = Group
  )
) +
  
  geom_col(
    width = 0.6
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  coord_cartesian(
    ylim = c(-0.10, 1.00)
  ) +
  
  theme_minimal() +
  
  theme(
    
    plot.title = element_text(
      size = 14,
      face = "bold",
      hjust = 0.5
    ),
    
    axis.title = element_text(
      size = 12
    ),
    
    axis.text = element_text(
      size = 11
    ),
    
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    ),
    
    legend.title = element_text(
      size = 11
    ),
    
    legend.text = element_text(
      size = 10
    )
    
  ) +
  
  labs(
    title = "Correlation Between Bitcoin and J303 Volatility",
    x = "",
    y = "Pearson Correlation",
    fill = "Analysis"
  )


#31 Regression coefficient comparison plot
coefficient.plot <- data.frame(
  
  Analysis = factor(
    
    c(
      "Overall",
      "Lower GPR",
      "Elevated GPR",
      "Russia-\nUkraine",
      "Israel-\nHamas",
      "Iran-\nIsrael"
    ),
    
    levels = c(
      "Overall",
      "Lower GPR",
      "Elevated GPR",
      "Russia-\nUkraine",
      "Israel-\nHamas",
      "Iran-\nIsrael"
    )
    
  ),
  
  BTC_Coefficient = c(
    
    coef(overall.model)[2],
    
    coef(model.lower)[2],
    
    coef(model.elevated)[2],
    
    coef(model.ukraine)[2],
    
    coef(model.hamas)[2],
    
    coef(model.iran)[2]
    
  ),
  
  Group = c(
    
    "Overall",
    
    "Regime",
    
    "Regime",
    
    "Conflict",
    
    "Conflict",
    
    "Conflict"
    
  )
  
)


ggplot(
  
  coefficient.plot,
  
  aes(
    
    x = Analysis,
    
    y = BTC_Coefficient,
    
    fill = Group
    
  )
  
) +
  
  geom_col(
    width = 0.6
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  theme_minimal() +
  
  theme(
    
    plot.title = element_text(
      size = 14,
      face = "bold",
      hjust = 0.5
    ),
    
    axis.title = element_text(
      size = 12
    ),
    
    axis.text = element_text(
      size = 11
    ),
    
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    ),
    
    legend.title = element_text(
      size = 11
    ),
    
    legend.text = element_text(
      size = 10
    )
    
  ) +
  
  labs(
    
    title = "Estimated Effect of Bitcoin Volatility on J303 Volatility",
    
    x = "",
    
    y = "Regression Coefficient",
    
    fill = "Analysis"
    
  )


#32 Overall empirical summary
overall.results <- data.frame(
  
  Analysis = c(
    
    "Overall",
    
    "Lower GPR",
    
    "Elevated GPR",
    
    "Russia-Ukraine",
    
    "Israel-Hamas",
    
    "Iran-Israel"
    
  ),
  
  Correlation = c(
    
    unname(overall.cor$estimate),
    
    unname(cor.lower$estimate),
    
    unname(cor.elevated$estimate),
    
    unname(cor.ukraine$estimate),
    
    unname(cor.hamas$estimate),
    
    unname(cor.iran$estimate)
    
  ),
  
  BTC_Volatility_Coefficient = c(
    
    coef(overall.model)[2],
    
    coef(model.lower)[2],
    
    coef(model.elevated)[2],
    
    coef(model.ukraine)[2],
    
    coef(model.hamas)[2],
    
    coef(model.iran)[2]
    
  ),
  
  Adj_R2 = c(
    
    summary(overall.model)$adj.r.squared,
    
    summary(model.lower)$adj.r.squared,
    
    summary(model.elevated)$adj.r.squared,
    
    summary(model.ukraine)$adj.r.squared,
    
    summary(model.hamas)$adj.r.squared,
    
    summary(model.iran)$adj.r.squared
    
  ),
  
  NeweyWest_P_Value = c(
    
    nw.overall["BTC_Volatility","Pr(>|t|)"],
    
    nw.lower["BTC_Volatility","Pr(>|t|)"],
    
    nw.elevated["BTC_Volatility","Pr(>|t|)"],
    
    nw.ukraine["BTC_Volatility","Pr(>|t|)"],
    
    nw.hamas["BTC_Volatility","Pr(>|t|)"],
    
    nw.iran["BTC_Volatility","Pr(>|t|)"]
    
  )
  
)

overall.results$Correlation <-
  round(overall.results$Correlation,4)

overall.results$BTC_Volatility_Coefficient <-
  signif(overall.results$BTC_Volatility_Coefficient,4)

overall.results$Adj_R2 <-
  round(overall.results$Adj_R2,4)

overall.results$NeweyWest_P_Value <-
  signif(overall.results$NeweyWest_P_Value,4)

overall.results$Significant <- ifelse(
  
  overall.results$NeweyWest_P_Value < 0.05,
  
  "Yes",
  
  "No"
  
)

print(overall.results)


