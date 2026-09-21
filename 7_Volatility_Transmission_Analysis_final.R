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
  sheet = "Data",
  na = "NA"
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


#5 Baseline relationship between Bitcoin and J303 conditional volatility
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


#6 Baseline regression diagnostics and robust inference

# Diagnostic plots
par(mfrow = c(2,2))

plot(overall.model)

par(mfrow = c(1,1))


# Jarque-Bera test for residual normality
jb.overall <- jarque.bera.test(
  residuals(overall.model)
)

jb.overall


# Durbin-Watson test for residual autocorrelation
dw.overall <- dwtest(
  overall.model
)

dw.overall


# Breusch-Pagan test for heteroskedasticity
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


#7 Baseline regression summary table
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
  
  data$GPRD < 153,
  
  "Lower GPR",
  
  "Elevated GPR"
  
)

table(data$GPR_Regime)


#9 Split data by geopolitical risk regime
lower.gpr <- subset(
  
  data,
  
  GPR_Regime == "Lower GPR"
  
)

elevated.gpr <- subset(
  
  data,
  
  GPR_Regime == "Elevated GPR"
  
)


#10 Summary statistics by geopolitical risk regime
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


#11 Correlation analysis by geopolitical risk regime
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
  round(
    correlation.summary$Correlation,
    4
  )

correlation.summary$Correlation_P_Value <-
  signif(
    correlation.summary$Correlation_P_Value,
    4
  )

correlation.summary$Significant <- ifelse(
  
  correlation.summary$Correlation_P_Value < 0.05,
  
  "Yes",
  
  "No"
  
)

print(correlation.summary)


#13 Regime-specific regression models

# Lower GPR
model.lower <- lm(
  
  J303_Volatility ~
    BTC_Volatility,
  
  data = lower.gpr
  
)

summary(model.lower)


# Elevated GPR
model.elevated <- lm(
  
  J303_Volatility ~
    BTC_Volatility,
  
  data = elevated.gpr
  
)

summary(model.elevated)


#14 Regime-specific regression diagnostics and robust inference

# Diagnostic plots
par(mfrow = c(2,2))

plot(model.lower)

plot(model.elevated)

par(mfrow = c(1,1))


# Jarque-Bera tests for residual normality
jb.lower <- jarque.bera.test(
  residuals(model.lower)
)

jb.elevated <- jarque.bera.test(
  residuals(model.elevated)
)

jb.lower

jb.elevated


# Durbin-Watson tests for residual autocorrelation
dw.lower <- dwtest(
  model.lower
)

dw.elevated <- dwtest(
  model.elevated
)

dw.lower

dw.elevated


# Breusch-Pagan tests for heteroskedasticity
bp.lower <- bptest(
  model.lower
)

bp.elevated <- bptest(
  model.elevated
)

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


#15 Regime-specific regression summary table
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
    nw.lower[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    nw.elevated[
      "BTC_Volatility",
      "Pr(>|t|)"
    ]
  )
  
)

regression.summary$Correlation <-
  round(
    regression.summary$Correlation,
    4
  )

regression.summary$BTC_Coefficient <-
  signif(
    regression.summary$BTC_Coefficient,
    4
  )

regression.summary$Adj_R2 <-
  round(
    regression.summary$Adj_R2,
    4
  )

regression.summary$Residual_SE <-
  round(
    regression.summary$Residual_SE,
    5
  )

regression.summary$NeweyWest_P_Value <-
  signif(
    regression.summary$NeweyWest_P_Value,
    4
  )

regression.summary$Significant <- ifelse(
  
  regression.summary$NeweyWest_P_Value < 0.05,
  
  "Yes",
  
  "No"
  
)

print(regression.summary)


#16 Fisher r-to-z test comparing regime correlations
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

z.statistic

p.value <- 2 * (
  1 -
    pnorm(abs(z.statistic))
)

# Fisher z-test result
round(z.statistic, 4)

signif(p.value, 4)

# Fisher z-test: z = 1.0649, p = 0.2869
# No statistically significant difference exists between the correlations across geopolitical risk regimes


#17 Interaction model testing whether the volatility relationship differs by regime
data$GPR_Regime <- factor(
  
  data$GPR_Regime,
  
  levels = c(
    "Lower GPR",
    "Elevated GPR"
  )
  
)

interaction.model <- lm(
  
  J303_Volatility ~
    BTC_Volatility *
    GPR_Regime,
  
  data = data
  
)

summary(interaction.model)


#18 Interaction model diagnostics and robust inference

# Diagnostic plots
par(mfrow = c(2,2))

plot(interaction.model)

par(mfrow = c(1,1))


# Jarque-Bera test for residual normality
jb.interaction <- jarque.bera.test(
  residuals(interaction.model)
)

jb.interaction


# Durbin-Watson test for residual autocorrelation
dw.interaction <- dwtest(
  interaction.model
)

dw.interaction


# Breusch-Pagan test for heteroskedasticity
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


#19 Regime-level summary of correlations and robust regression significance
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
    nw.lower[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    nw.elevated[
      "BTC_Volatility",
      "Pr(>|t|)"
    ]
  ),
  
  Significant = c(
    ifelse(
      nw.lower[
        "BTC_Volatility",
        "Pr(>|t|)"
      ] < 0.05,
      "Yes",
      "No"
    ),
    ifelse(
      nw.elevated[
        "BTC_Volatility",
        "Pr(>|t|)"
      ] < 0.05,
      "Yes",
      "No"
    )
  )
  
)

overall.summary$Correlation <-
  round(
    overall.summary$Correlation,
    4
  )

overall.summary$NeweyWest_P_Value <-
  signif(
    overall.summary$NeweyWest_P_Value,
    4
  )

print(overall.summary)


###############################################################
# Section B: Analysis During Major Geopolitical Events
###############################################################

#20 Define event windows

events <- data.frame(
  
  Event = c(
    "Russia-Ukraine (Crimea Crisis)",
    "Paris Attacks",
    "Russia-Ukraine Invasion",
    "Israel-Hamas War",
    "US-Israel-Iran Conflict"
  ),
  
  Start_Date = as.Date(c(
    "2014-02-13",
    "2015-10-30",
    "2022-02-07",
    "2023-09-20",
    "2026-02-11"
  )),
  
  End_Date = as.Date(c(
    "2014-03-25",
    "2015-12-17",
    "2022-04-05",
    "2023-11-17",
    "2026-03-23"
  ))
  
)

print(events)

# Note: These are GPR-derived statistical event windows and do not represent
# the exact real-world start and end dates of each geopolitical event.


#21 Split data into event windows

crimea <- subset(
  
  data,
  
  Date >= events$Start_Date[1] &
    Date <= events$End_Date[1]
  
)

paris <- subset(
  
  data,
  
  Date >= events$Start_Date[2] &
    Date <= events$End_Date[2]
  
)

ukraine <- subset(
  
  data,
  
  Date >= events$Start_Date[3] &
    Date <= events$End_Date[3]
  
)

hamas <- subset(
  
  data,
  
  Date >= events$Start_Date[4] &
    Date <= events$End_Date[4]
  
)

iran <- subset(
  
  data,
  
  Date >= events$Start_Date[5] &
    Date <= events$End_Date[5]
  
)


#22 Summary statistics during major geopolitical events

event.statistics <- data.frame(
  
  Event = c(
    "Russia-Ukraine (Crimea Crisis)",
    "Paris Attacks",
    "Russia-Ukraine Invasion",
    "Israel-Hamas War",
    "US-Israel-Iran Conflict"
  ),
  
  Sample_Size = c(
    nrow(crimea),
    nrow(paris),
    nrow(ukraine),
    nrow(hamas),
    nrow(iran)
  ),
  
  Mean_GPR = c(
    mean(crimea$GPRD),
    mean(paris$GPRD),
    mean(ukraine$GPRD),
    mean(hamas$GPRD),
    mean(iran$GPRD)
  ),
  
  SD_GPR = c(
    sd(crimea$GPRD),
    sd(paris$GPRD),
    sd(ukraine$GPRD),
    sd(hamas$GPRD),
    sd(iran$GPRD)
  ),
  
  Mean_BTC_Volatility = c(
    mean(crimea$BTC_Volatility),
    mean(paris$BTC_Volatility),
    mean(ukraine$BTC_Volatility),
    mean(hamas$BTC_Volatility),
    mean(iran$BTC_Volatility)
  ),
  
  SD_BTC_Volatility = c(
    sd(crimea$BTC_Volatility),
    sd(paris$BTC_Volatility),
    sd(ukraine$BTC_Volatility),
    sd(hamas$BTC_Volatility),
    sd(iran$BTC_Volatility)
  )
  
)

event.statistics[-1] <- round(
  event.statistics[-1],
  4
)

print(event.statistics)


#23 Correlation analysis during major geopolitical events

cor.crimea <- cor.test(
  
  crimea$BTC_Volatility,
  
  crimea$J303_Volatility
  
)

cor.paris <- cor.test(
  
  paris$BTC_Volatility,
  
  paris$J303_Volatility
  
)

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

cor.crimea

cor.paris

cor.ukraine

cor.hamas

cor.iran


#24 Correlation summary table for major geopolitical events

event.correlation.summary <- data.frame(
  
  Event = c(
    "Russia-Ukraine (Crimea Crisis)",
    "Paris Attacks",
    "Russia-Ukraine Invasion",
    "Israel-Hamas War",
    "US-Israel-Iran Conflict"
  ),
  
  Sample_Size = c(
    nrow(crimea),
    nrow(paris),
    nrow(ukraine),
    nrow(hamas),
    nrow(iran)
  ),
  
  Correlation = c(
    unname(cor.crimea$estimate),
    unname(cor.paris$estimate),
    unname(cor.ukraine$estimate),
    unname(cor.hamas$estimate),
    unname(cor.iran$estimate)
  ),
  
  Correlation_P_Value = c(
    cor.crimea$p.value,
    cor.paris$p.value,
    cor.ukraine$p.value,
    cor.hamas$p.value,
    cor.iran$p.value
  )
  
)

event.correlation.summary$Correlation <-
  round(
    event.correlation.summary$Correlation,
    4
  )

event.correlation.summary$Correlation_P_Value <-
  signif(
    event.correlation.summary$Correlation_P_Value,
    4
  )

event.correlation.summary$Significant <- ifelse(
  
  event.correlation.summary$Correlation_P_Value < 0.05,
  
  "Yes",
  
  "No"
  
)

print(event.correlation.summary)


#25 Event-specific regression models

# Russia-Ukraine (Crimea Crisis)
model.crimea <- lm(
  
  J303_Volatility ~
    BTC_Volatility,
  
  data = crimea
  
)

summary(model.crimea)


# Paris Attacks
model.paris <- lm(
  
  J303_Volatility ~
    BTC_Volatility,
  
  data = paris
  
)

summary(model.paris)


# Russia-Ukraine Invasion
model.ukraine <- lm(
  
  J303_Volatility ~
    BTC_Volatility,
  
  data = ukraine
  
)

summary(model.ukraine)


# Israel-Hamas War
model.hamas <- lm(
  
  J303_Volatility ~
    BTC_Volatility,
  
  data = hamas
  
)

summary(model.hamas)


# US-Israel-Iran Conflict
model.iran <- lm(
  
  J303_Volatility ~
    BTC_Volatility,
  
  data = iran
  
)

summary(model.iran)


#26 Event-specific regression diagnostics

# Diagnostic plots
par(mfrow = c(2,2))

plot(model.crimea)

plot(model.paris)

plot(model.ukraine)

plot(model.hamas)

plot(model.iran)

par(mfrow = c(1,1))


# Jarque-Bera tests for residual normality
jb.crimea <- jarque.bera.test(
  residuals(model.crimea)
)

jb.paris <- jarque.bera.test(
  residuals(model.paris)
)

jb.ukraine <- jarque.bera.test(
  residuals(model.ukraine)
)

jb.hamas <- jarque.bera.test(
  residuals(model.hamas)
)

jb.iran <- jarque.bera.test(
  residuals(model.iran)
)

jb.crimea

jb.paris

jb.ukraine

jb.hamas

jb.iran


# Durbin-Watson tests for residual autocorrelation
dw.crimea <- dwtest(
  model.crimea
)

dw.paris <- dwtest(
  model.paris
)

dw.ukraine <- dwtest(
  model.ukraine
)

dw.hamas <- dwtest(
  model.hamas
)

dw.iran <- dwtest(
  model.iran
)

dw.crimea

dw.paris

dw.ukraine

dw.hamas

dw.iran


# Breusch-Pagan tests for heteroskedasticity
bp.crimea <- bptest(
  model.crimea
)

bp.paris <- bptest(
  model.paris
)

bp.ukraine <- bptest(
  model.ukraine
)

bp.hamas <- bptest(
  model.hamas
)

bp.iran <- bptest(
  model.iran
)

bp.crimea

bp.paris

bp.ukraine

bp.hamas

bp.iran


#27 Newey-West robust inference for event-specific regressions

nw.crimea <- coeftest(
  
  model.crimea,
  
  vcov = NeweyWest(
    model.crimea,
    prewhite = FALSE
  )
  
)

nw.paris <- coeftest(
  
  model.paris,
  
  vcov = NeweyWest(
    model.paris,
    prewhite = FALSE
  )
  
)

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

nw.crimea

nw.paris

nw.ukraine

nw.hamas

nw.iran


#28 Event-specific regression summary table

event.overall <- data.frame(
  
  Event = c(
    "Russia-Ukraine (Crimea Crisis)",
    "Paris Attacks",
    "Russia-Ukraine Invasion",
    "Israel-Hamas War",
    "US-Israel-Iran Conflict"
  ),
  
  Sample_Size = c(
    nrow(crimea),
    nrow(paris),
    nrow(ukraine),
    nrow(hamas),
    nrow(iran)
  ),
  
  Correlation = c(
    unname(cor.crimea$estimate),
    unname(cor.paris$estimate),
    unname(cor.ukraine$estimate),
    unname(cor.hamas$estimate),
    unname(cor.iran$estimate)
  ),
  
  BTC_Coefficient = c(
    coef(model.crimea)[2],
    coef(model.paris)[2],
    coef(model.ukraine)[2],
    coef(model.hamas)[2],
    coef(model.iran)[2]
  ),
  
  Adj_R2 = c(
    summary(model.crimea)$adj.r.squared,
    summary(model.paris)$adj.r.squared,
    summary(model.ukraine)$adj.r.squared,
    summary(model.hamas)$adj.r.squared,
    summary(model.iran)$adj.r.squared
  ),
  
  Residual_SE = c(
    summary(model.crimea)$sigma,
    summary(model.paris)$sigma,
    summary(model.ukraine)$sigma,
    summary(model.hamas)$sigma,
    summary(model.iran)$sigma
  ),
  
  NeweyWest_P_Value = c(
    nw.crimea[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    nw.paris[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    nw.ukraine[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    nw.hamas[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    nw.iran[
      "BTC_Volatility",
      "Pr(>|t|)"
    ]
  )
  
)

event.overall$Correlation <-
  round(
    event.overall$Correlation,
    4
  )

event.overall$BTC_Coefficient <-
  signif(
    event.overall$BTC_Coefficient,
    4
  )

event.overall$Adj_R2 <-
  round(
    event.overall$Adj_R2,
    4
  )

event.overall$Residual_SE <-
  signif(
    event.overall$Residual_SE,
    4
  )

event.overall$NeweyWest_P_Value <-
  signif(
    event.overall$NeweyWest_P_Value,
    4
  )

event.overall$Significant <- ifelse(
  
  event.overall$NeweyWest_P_Value < 0.05,
  
  "Yes",
  
  "No"
  
)

print(event.overall)


#29 Pairwise Fisher r-to-z tests comparing event correlations

r.crimea <- cor.crimea$estimate

r.paris <- cor.paris$estimate

r.ukraine <- cor.ukraine$estimate

r.hamas <- cor.hamas$estimate

r.iran <- cor.iran$estimate


z.crimea.paris <- (
  atanh(r.crimea) -
    atanh(r.paris)
) /
  sqrt(
    1 / (nrow(crimea) - 3) +
      1 / (nrow(paris) - 3)
  )

z.crimea.ukraine <- (
  atanh(r.crimea) -
    atanh(r.ukraine)
) /
  sqrt(
    1 / (nrow(crimea) - 3) +
      1 / (nrow(ukraine) - 3)
  )

z.crimea.hamas <- (
  atanh(r.crimea) -
    atanh(r.hamas)
) /
  sqrt(
    1 / (nrow(crimea) - 3) +
      1 / (nrow(hamas) - 3)
  )

z.crimea.iran <- (
  atanh(r.crimea) -
    atanh(r.iran)
) /
  sqrt(
    1 / (nrow(crimea) - 3) +
      1 / (nrow(iran) - 3)
  )

z.paris.ukraine <- (
  atanh(r.paris) -
    atanh(r.ukraine)
) /
  sqrt(
    1 / (nrow(paris) - 3) +
      1 / (nrow(ukraine) - 3)
  )

z.paris.hamas <- (
  atanh(r.paris) -
    atanh(r.hamas)
) /
  sqrt(
    1 / (nrow(paris) - 3) +
      1 / (nrow(hamas) - 3)
  )

z.paris.iran <- (
  atanh(r.paris) -
    atanh(r.iran)
) /
  sqrt(
    1 / (nrow(paris) - 3) +
      1 / (nrow(iran) - 3)
  )

z.ukraine.hamas <- (
  atanh(r.ukraine) -
    atanh(r.hamas)
) /
  sqrt(
    1 / (nrow(ukraine) - 3) +
      1 / (nrow(hamas) - 3)
  )

z.ukraine.iran <- (
  atanh(r.ukraine) -
    atanh(r.iran)
) /
  sqrt(
    1 / (nrow(ukraine) - 3) +
      1 / (nrow(iran) - 3)
  )

z.hamas.iran <- (
  atanh(r.hamas) -
    atanh(r.iran)
) /
  sqrt(
    1 / (nrow(hamas) - 3) +
      1 / (nrow(iran) - 3)
  )


pairwise.z <- data.frame(
  
  Comparison = c(
    "Crimea vs Paris",
    "Crimea vs Ukraine",
    "Crimea vs Hamas",
    "Crimea vs Iran",
    "Paris vs Ukraine",
    "Paris vs Hamas",
    "Paris vs Iran",
    "Ukraine vs Hamas",
    "Ukraine vs Iran",
    "Hamas vs Iran"
  ),
  
  Z_Statistic = c(
    z.crimea.paris,
    z.crimea.ukraine,
    z.crimea.hamas,
    z.crimea.iran,
    z.paris.ukraine,
    z.paris.hamas,
    z.paris.iran,
    z.ukraine.hamas,
    z.ukraine.iran,
    z.hamas.iran
  )
  
)

pairwise.z$P_Value <- 2 * (
  1 -
    pnorm(
      abs(pairwise.z$Z_Statistic)
    )
)

pairwise.z$Z_Statistic <-
  round(
    pairwise.z$Z_Statistic,
    4
  )

pairwise.z$P_Value <-
  signif(
    pairwise.z$P_Value,
    4
  )

pairwise.z$Significant <- ifelse(
  
  pairwise.z$P_Value < 0.05,
  
  "Yes",
  
  "No"
  
)

print(pairwise.z)

# Note: Pairwise Fisher r-to-z tests are treated as exploratory because
# event-window observations are time-series observations and may not be
# independent. The event regressions therefore use Newey-West robust inference
# as the primary basis for statistical significance.

# Note: Ten pairwise correlation comparisons are conducted, creating a
# multiple-comparison issue. The treatment of these tests should be discussed
# with the thesis partner and supervisor, including whether to retain the
# unadjusted results, apply a multiple-comparison adjustment, or present them
# as exploratory results.


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
      "Russia-\nUkraine\n(Crimea)",
      "Paris\nAttacks",
      "Russia-\nUkraine\nInvasion",
      "Israel-\nHamas",
      "US-Israel-\nIran"
    ),
    
    levels = c(
      "Overall",
      "Lower GPR",
      "Elevated GPR",
      "Russia-\nUkraine\n(Crimea)",
      "Paris\nAttacks",
      "Russia-\nUkraine\nInvasion",
      "Israel-\nHamas",
      "US-Israel-\nIran"
    )
    
  ),
  
  Correlation = c(
    
    overall.cor$estimate,
    
    cor.lower$estimate,
    
    cor.elevated$estimate,
    
    cor.crimea$estimate,
    
    cor.paris$estimate,
    
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
    ylim = c(-1, 1)
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
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 10
    ),
    
    legend.title = element_text(
      size = 11
    ),
    
    legend.text = element_text(
      size = 10
    ),
    
    plot.margin = margin(
      5.5,
      5.5,
      30,
      5.5
    )
    
  ) +
  
  labs(
    
    title = "Correlation Between Bitcoin and J303 Conditional Volatility",
    
    x = "",
    
    y = "Pearson Correlation",
    
    fill = "Analysis"
    
  ) +
  
  scale_x_discrete(
    labels = c(
      "Overall" = "Overall",
      "Lower GPR" = "Lower\nGPR",
      "Elevated GPR" = "Elevated\nGPR",
      "Russia-Ukraine (Crimea)" = "Russia-Ukraine\n(Crimea)",
      "Paris Attack" = "Paris\nAttack",
      "Russia-Ukraine Invasion" = "Russia-Ukraine\nInvasion",
      "Israel-Hamas" = "Israel-\nHamas",
      "US-Israel-Iran" = "US-Israel-\nIran"
    )
  )

#31 Regression coefficient comparison plot

coefficient.plot <- data.frame(
  
  Analysis = factor(
    
    c(
      "Overall",
      "Lower GPR",
      "Elevated GPR",
      "Russia-\nUkraine\n(Crimea)",
      "Paris\nAttacks",
      "Russia-\nUkraine\nInvasion",
      "Israel-\nHamas",
      "US-Israel-\nIran"
    ),
    
    levels = c(
      "Overall",
      "Lower GPR",
      "Elevated GPR",
      "Russia-\nUkraine\n(Crimea)",
      "Paris\nAttacks",
      "Russia-\nUkraine\nInvasion",
      "Israel-\nHamas",
      "US-Israel-\nIran"
    )
    
  ),
  
  BTC_Coefficient = c(
    
    coef(overall.model)[2],
    
    coef(model.lower)[2],
    
    coef(model.elevated)[2],
    
    coef(model.crimea)[2],
    
    coef(model.paris)[2],
    
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
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 10
    ),
    
    legend.title = element_text(
      size = 11
    ),
    
    legend.text = element_text(
      size = 10
    ),
    
    plot.margin = margin(
      5.5,
      5.5,
      30,
      5.5
    )
    
  ) +
  
  labs(
    
    title = "Estimated Relationship Between Bitcoin and J303 Conditional Volatility",
    
    x = "",
    
    y = "Regression Coefficient",
    
    fill = "Analysis"
    
  ) +
  
  scale_x_discrete(
    labels = c(
      "Overall" = "Overall",
      "Lower GPR" = "Lower\nGPR",
      "Elevated GPR" = "Elevated\nGPR",
      "Russia-Ukraine (Crimea)" = "Russia-Ukraine\n(Crimea)",
      "Paris Attack" = "Paris\nAttack",
      "Russia-Ukraine Invasion" = "Russia-Ukraine\nInvasion",
      "Israel-Hamas" = "Israel-\nHamas",
      "US-Israel-Iran" = "US-Israel-\nIran"
    )
  )


#32 Overall empirical summary

overall.results <- data.frame(
  
  Analysis = c(
    
    "Overall",
    
    "Lower GPR",
    
    "Elevated GPR",
    
    "Russia-Ukraine (Crimea Crisis)",
    
    "Paris Attacks",
    
    "Russia-Ukraine Invasion",
    
    "Israel-Hamas War",
    
    "US-Israel-Iran Conflict"
    
  ),
  
  Correlation = c(
    
    unname(overall.cor$estimate),
    
    unname(cor.lower$estimate),
    
    unname(cor.elevated$estimate),
    
    unname(cor.crimea$estimate),
    
    unname(cor.paris$estimate),
    
    unname(cor.ukraine$estimate),
    
    unname(cor.hamas$estimate),
    
    unname(cor.iran$estimate)
    
  ),
  
  BTC_Volatility_Coefficient = c(
    
    coef(overall.model)[2],
    
    coef(model.lower)[2],
    
    coef(model.elevated)[2],
    
    coef(model.crimea)[2],
    
    coef(model.paris)[2],
    
    coef(model.ukraine)[2],
    
    coef(model.hamas)[2],
    
    coef(model.iran)[2]
    
  ),
  
  Adj_R2 = c(
    
    summary(overall.model)$adj.r.squared,
    
    summary(model.lower)$adj.r.squared,
    
    summary(model.elevated)$adj.r.squared,
    
    summary(model.crimea)$adj.r.squared,
    
    summary(model.paris)$adj.r.squared,
    
    summary(model.ukraine)$adj.r.squared,
    
    summary(model.hamas)$adj.r.squared,
    
    summary(model.iran)$adj.r.squared
    
  ),
  
  NeweyWest_P_Value = c(
    
    nw.overall[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    
    nw.lower[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    
    nw.elevated[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    
    nw.crimea[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    
    nw.paris[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    
    nw.ukraine[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    
    nw.hamas[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    
    nw.iran[
      "BTC_Volatility",
      "Pr(>|t|)"
    ]
    
  )
  
)

overall.results$Correlation <-
  round(
    overall.results$Correlation,
    4
  )

overall.results$BTC_Volatility_Coefficient <-
  signif(
    overall.results$BTC_Volatility_Coefficient,
    4
  )

overall.results$Adj_R2 <-
  round(
    overall.results$Adj_R2,
    4
  )

overall.results$NeweyWest_P_Value <-
  signif(
    overall.results$NeweyWest_P_Value,
    4
  )

overall.results$Significant <- ifelse(
  
  overall.results$NeweyWest_P_Value < 0.05,
  
  "Yes",
  
  "No"
  
)

print(overall.results)

