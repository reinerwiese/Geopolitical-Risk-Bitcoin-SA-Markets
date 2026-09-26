#1 Load required packages
library(dplyr)
library(ggplot2)

library(FinTS)
library(tseries)
library(moments)

library(lmtest)
library(sandwich)
library(car)

source("0_Run_First.R")

data <- data0


###############################################################
# Section A: Analysis by Geopolitical Risk Regime
###############################################################

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


#8 Use geopolitical risk regimes from shared setup

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

#20 Use event windows from shared setup

print(events)

#21 Split data into event windows

paris <- subset(
  
  data,
  
  Date >= events$Start_Date[1] &
    Date <= events$End_Date[1]
  
)

qatar <- subset(
  
  data,
  
  Date >= events$Start_Date[2] &
    Date <= events$End_Date[2]
  
)

turkey.syria <- subset(
  
  data,
  
  Date >= events$Start_Date[3] &
    Date <= events$End_Date[3]
  
)

bakhmut <- subset(
  
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
  
  Event = events$Event,
  
  Sample_Size = c(
    nrow(paris),
    nrow(qatar),
    nrow(turkey.syria),
    nrow(bakhmut),
    nrow(iran)
  ),
  
  Mean_GPR = c(
    mean(paris$GPRD),
    mean(qatar$GPRD),
    mean(turkey.syria$GPRD),
    mean(bakhmut$GPRD),
    mean(iran$GPRD)
  ),
  
  SD_GPR = c(
    sd(paris$GPRD),
    sd(qatar$GPRD),
    sd(turkey.syria$GPRD),
    sd(bakhmut$GPRD),
    sd(iran$GPRD)
  ),
  
  Mean_BTC_Volatility = c(
    mean(paris$BTC_Volatility),
    mean(qatar$BTC_Volatility),
    mean(turkey.syria$BTC_Volatility),
    mean(bakhmut$BTC_Volatility),
    mean(iran$BTC_Volatility)
  ),
  
  SD_BTC_Volatility = c(
    sd(paris$BTC_Volatility),
    sd(qatar$BTC_Volatility),
    sd(turkey.syria$BTC_Volatility),
    sd(bakhmut$BTC_Volatility),
    sd(iran$BTC_Volatility)
  )
  
)

event.statistics[-1] <- round(
  event.statistics[-1],
  4
)

print(event.statistics)


#23 Correlation analysis during major geopolitical events

cor.paris <- cor.test(
  
  paris$BTC_Volatility,
  
  paris$J303_Volatility
  
)

cor.qatar <- cor.test(
  
  qatar$BTC_Volatility,
  
  qatar$J303_Volatility
  
)

cor.turkey.syria <- cor.test(
  
  turkey.syria$BTC_Volatility,
  
  turkey.syria$J303_Volatility
  
)

cor.bakhmut <- cor.test(
  
  bakhmut$BTC_Volatility,
  
  bakhmut$J303_Volatility
  
)

cor.iran <- cor.test(
  
  iran$BTC_Volatility,
  
  iran$J303_Volatility
  
)

cor.paris

cor.qatar

cor.turkey.syria

cor.bakhmut

cor.iran


#24 Correlation summary table for major geopolitical events

event.correlation.summary <- data.frame(
  
  Event = events$Event,
  
  Sample_Size = c(
    nrow(paris),
    nrow(qatar),
    nrow(turkey.syria),
    nrow(bakhmut),
    nrow(iran)
  ),
  
  Correlation = c(
    unname(cor.paris$estimate),
    unname(cor.qatar$estimate),
    unname(cor.turkey.syria$estimate),
    unname(cor.bakhmut$estimate),
    unname(cor.iran$estimate)
  ),
  
  Correlation_P_Value = c(
    cor.paris$p.value,
    cor.qatar$p.value,
    cor.turkey.syria$p.value,
    cor.bakhmut$p.value,
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

# Paris Attacks
model.paris <- lm(
  
  J303_Volatility ~
    BTC_Volatility,
  
  data = paris
  
)

summary(model.paris)


# Qatar Diplomatic Crisis
model.qatar <- lm(
  
  J303_Volatility ~
    BTC_Volatility,
  
  data = qatar
  
)

summary(model.qatar)


# Turkey-Syria Escalation
model.turkey.syria <- lm(
  
  J303_Volatility ~
    BTC_Volatility,
  
  data = turkey.syria
  
)

summary(model.turkey.syria)


# Russia-Ukraine / Bakhmut
model.bakhmut <- lm(
  
  J303_Volatility ~
    BTC_Volatility,
  
  data = bakhmut
  
)

summary(model.bakhmut)


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

plot(model.paris)

plot(model.qatar)

plot(model.turkey.syria)

plot(model.bakhmut)

plot(model.iran)

par(mfrow = c(1,1))


# Jarque-Bera tests for residual normality
jb.paris <- jarque.bera.test(
  residuals(model.paris)
)

jb.qatar <- jarque.bera.test(
  residuals(model.qatar)
)

jb.turkey.syria <- jarque.bera.test(
  residuals(model.turkey.syria)
)

jb.bakhmut <- jarque.bera.test(
  residuals(model.bakhmut)
)

jb.iran <- jarque.bera.test(
  residuals(model.iran)
)

jb.paris

jb.qatar

jb.turkey.syria

jb.bakhmut

jb.iran


# Durbin-Watson tests for residual autocorrelation
dw.paris <- dwtest(
  model.paris
)

dw.qatar <- dwtest(
  model.qatar
)

dw.turkey.syria <- dwtest(
  model.turkey.syria
)

dw.bakhmut <- dwtest(
  model.bakhmut
)

dw.iran <- dwtest(
  model.iran
)

dw.paris

dw.qatar

dw.turkey.syria

dw.bakhmut

dw.iran


# Breusch-Pagan tests for heteroskedasticity
bp.paris <- bptest(
  model.paris
)

bp.qatar <- bptest(
  model.qatar
)

bp.turkey.syria <- bptest(
  model.turkey.syria
)

bp.bakhmut <- bptest(
  model.bakhmut
)

bp.iran <- bptest(
  model.iran
)

bp.paris

bp.qatar

bp.turkey.syria

bp.bakhmut

bp.iran


#27 Newey-West robust inference for event-specific regressions

nw.paris <- coeftest(
  
  model.paris,
  
  vcov = NeweyWest(
    model.paris,
    prewhite = FALSE
  )
  
)

nw.qatar <- coeftest(
  
  model.qatar,
  
  vcov = NeweyWest(
    model.qatar,
    prewhite = FALSE
  )
  
)

nw.turkey.syria <- coeftest(
  
  model.turkey.syria,
  
  vcov = NeweyWest(
    model.turkey.syria,
    prewhite = FALSE
  )
  
)

nw.bakhmut <- coeftest(
  
  model.bakhmut,
  
  vcov = NeweyWest(
    model.bakhmut,
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

nw.paris

nw.qatar

nw.turkey.syria

nw.bakhmut

nw.iran


#28 Event-specific regression summary table

event.overall <- data.frame(
  
  Event = events$Event,
  
  Sample_Size = c(
    nrow(paris),
    nrow(qatar),
    nrow(turkey.syria),
    nrow(bakhmut),
    nrow(iran)
  ),
  
  Correlation = c(
    unname(cor.paris$estimate),
    unname(cor.qatar$estimate),
    unname(cor.turkey.syria$estimate),
    unname(cor.bakhmut$estimate),
    unname(cor.iran$estimate)
  ),
  
  BTC_Coefficient = c(
    coef(model.paris)[2],
    coef(model.qatar)[2],
    coef(model.turkey.syria)[2],
    coef(model.bakhmut)[2],
    coef(model.iran)[2]
  ),
  
  Adj_R2 = c(
    summary(model.paris)$adj.r.squared,
    summary(model.qatar)$adj.r.squared,
    summary(model.turkey.syria)$adj.r.squared,
    summary(model.bakhmut)$adj.r.squared,
    summary(model.iran)$adj.r.squared
  ),
  
  Residual_SE = c(
    summary(model.paris)$sigma,
    summary(model.qatar)$sigma,
    summary(model.turkey.syria)$sigma,
    summary(model.bakhmut)$sigma,
    summary(model.iran)$sigma
  ),
  
  NeweyWest_P_Value = c(
    nw.paris[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    nw.qatar[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    nw.turkey.syria[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    nw.bakhmut[
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

r.paris <- cor.paris$estimate

r.qatar <- cor.qatar$estimate

r.turkey.syria <- cor.turkey.syria$estimate

r.bakhmut <- cor.bakhmut$estimate

r.iran <- cor.iran$estimate


z.paris.qatar <- (
  atanh(r.paris) -
    atanh(r.qatar)
) /
  sqrt(
    1 / (nrow(paris) - 3) +
      1 / (nrow(qatar) - 3)
  )

z.paris.turkey.syria <- (
  atanh(r.paris) -
    atanh(r.turkey.syria)
) /
  sqrt(
    1 / (nrow(paris) - 3) +
      1 / (nrow(turkey.syria) - 3)
  )

z.paris.bakhmut <- (
  atanh(r.paris) -
    atanh(r.bakhmut)
) /
  sqrt(
    1 / (nrow(paris) - 3) +
      1 / (nrow(bakhmut) - 3)
  )

z.paris.iran <- (
  atanh(r.paris) -
    atanh(r.iran)
) /
  sqrt(
    1 / (nrow(paris) - 3) +
      1 / (nrow(iran) - 3)
  )

z.qatar.turkey.syria <- (
  atanh(r.qatar) -
    atanh(r.turkey.syria)
) /
  sqrt(
    1 / (nrow(qatar) - 3) +
      1 / (nrow(turkey.syria) - 3)
  )

z.qatar.bakhmut <- (
  atanh(r.qatar) -
    atanh(r.bakhmut)
) /
  sqrt(
    1 / (nrow(qatar) - 3) +
      1 / (nrow(bakhmut) - 3)
  )

z.qatar.iran <- (
  atanh(r.qatar) -
    atanh(r.iran)
) /
  sqrt(
    1 / (nrow(qatar) - 3) +
      1 / (nrow(iran) - 3)
  )

z.turkey.syria.bakhmut <- (
  atanh(r.turkey.syria) -
    atanh(r.bakhmut)
) /
  sqrt(
    1 / (nrow(turkey.syria) - 3) +
      1 / (nrow(bakhmut) - 3)
  )

z.turkey.syria.iran <- (
  atanh(r.turkey.syria) -
    atanh(r.iran)
) /
  sqrt(
    1 / (nrow(turkey.syria) - 3) +
      1 / (nrow(iran) - 3)
  )

z.bakhmut.iran <- (
  atanh(r.bakhmut) -
    atanh(r.iran)
) /
  sqrt(
    1 / (nrow(bakhmut) - 3) +
      1 / (nrow(iran) - 3)
  )


pairwise.z <- data.frame(
  
  Comparison = c(
    "Paris Attacks vs Qatar Diplomatic Crisis",
    "Paris Attacks vs Turkey-Syria Escalation",
    "Paris Attacks vs Russia-Ukraine / Bakhmut",
    "Paris Attacks vs US-Israel-Iran Conflict",
    "Qatar Diplomatic Crisis vs Turkey-Syria Escalation",
    "Qatar Diplomatic Crisis vs Russia-Ukraine / Bakhmut",
    "Qatar Diplomatic Crisis vs US-Israel-Iran Conflict",
    "Turkey-Syria Escalation vs Russia-Ukraine / Bakhmut",
    "Turkey-Syria Escalation vs US-Israel-Iran Conflict",
    "Russia-Ukraine / Bakhmut vs US-Israel-Iran Conflict"
  ),
  
  Z_Statistic = c(
    z.paris.qatar,
    z.paris.turkey.syria,
    z.paris.bakhmut,
    z.paris.iran,
    z.qatar.turkey.syria,
    z.qatar.bakhmut,
    z.qatar.iran,
    z.turkey.syria.bakhmut,
    z.turkey.syria.iran,
    z.bakhmut.iran
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
      "Paris\nAttacks",
      "Qatar\nDiplomatic\nCrisis",
      "Turkey-Syria\nEscalation",
      "Russia-Ukraine\n/ Bakhmut",
      "US-Israel-Iran\nConflict"
    ),
    
    levels = c(
      "Overall",
      "Lower GPR",
      "Elevated GPR",
      "Paris\nAttacks",
      "Qatar\nDiplomatic\nCrisis",
      "Turkey-Syria\nEscalation",
      "Russia-Ukraine\n/ Bakhmut",
      "US-Israel-Iran\nConflict"
    )
    
  ),
  
  Correlation = c(
    
    overall.cor$estimate,
    
    cor.lower$estimate,
    
    cor.elevated$estimate,
    
    cor.paris$estimate,
    
    cor.qatar$estimate,
    
    cor.turkey.syria$estimate,
    
    cor.bakhmut$estimate,
    
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
    width = 0.6,
    colour = "black"
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  coord_cartesian(
    ylim = c(-1, 1)
  ) +
  
  scale_fill_manual(
    values = c(
      "Overall" = "grey20",
      "Regime" = "grey55",
      "Conflict" = "grey80"
    )
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
    )
  ) +
  
  labs(
    title = "Bitcoin–J303 Conditional Volatility Correlations",
    x = "",
    y = "Pearson Correlation",
    fill = "Analysis"
  ) +
  
  scale_x_discrete(
    labels = c(
      "Overall" = "Overall",
      "Lower GPR" = "Lower\nGPR",
      "Elevated GPR" = "Elevated\nGPR",
      "Paris\nAttacks" = "Paris\nAttacks",
      "Qatar\nDiplomatic\nCrisis" = "Qatar\nDiplomatic\nCrisis",
      "Turkey-Syria\nEscalation" = "Turkey-Syria\nEscalation",
      "Russia-Ukraine\n/ Bakhmut" = "Russia-Ukraine\n/ Bakhmut",
      "US-Israel-Iran\nConflict" = "US-Israel-Iran\nConflict"
    )
  )

#31 Regression coefficient comparison plot

coefficient.plot <- data.frame(
  
  Analysis = factor(
    
    c(
      "Overall",
      "Lower GPR",
      "Elevated GPR",
      "Paris\nAttacks",
      "Qatar\nDiplomatic\nCrisis",
      "Turkey-Syria\nEscalation",
      "Russia-Ukraine\n/ Bakhmut",
      "US-Israel-Iran\nConflict"
    ),
    levels = c(
      "Overall",
      "Lower GPR",
      "Elevated GPR",
      "Paris\nAttacks",
      "Qatar\nDiplomatic\nCrisis",
      "Turkey-Syria\nEscalation",
      "Russia-Ukraine\n/ Bakhmut",
      "US-Israel-Iran\nConflict"
    )
    
  ),
  
  BTC_Coefficient = c(
    
    coef(overall.model)[2],
    
    coef(model.lower)[2],
    
    coef(model.elevated)[2],
    
    coef(model.paris)[2],
    
    coef(model.qatar)[2],
    
    coef(model.turkey.syria)[2],
    
    coef(model.bakhmut)[2],
    
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
    width = 0.6,
    colour = "black"
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  scale_fill_manual(
    values = c(
      "Overall" = "grey20",
      "Regime" = "grey55",
      "Conflict" = "grey80"
    )
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
    )
  ) +
  
  labs(
    title = "Bitcoin–J303 Volatility Regression Coefficients",
    x = "",
    y = "Regression Coefficient",
    fill = "Analysis"
  ) +
  
  scale_x_discrete(
    labels = c(
      "Overall" = "Overall",
      "Lower GPR" = "Lower\nGPR",
      "Elevated GPR" = "Elevated\nGPR",
      "Paris\nAttacks" = "Paris\nAttacks",
      "Qatar\nDiplomatic\nCrisis" = "Qatar\nDiplomatic\nCrisis",
      "Turkey-Syria\nEscalation" = "Turkey-Syria\nEscalation",
      "Russia-Ukraine\n/ Bakhmut" = "Russia-Ukraine\n/ Bakhmut",
      "US-Israel-Iran\nConflict" = "US-Israel-Iran\nConflict"
    )
  )


#32 Overall empirical summary

overall.results <- data.frame(
  
  Analysis = c(
    "Overall",
    "Lower GPR",
    "Elevated GPR",
    "Paris Attacks",
    "Qatar Diplomatic Crisis",
    "Turkey-Syria Escalation",
    "Russia-Ukraine / Bakhmut",
    "US-Israel-Iran Conflict"
  ),
  
  Correlation = c(
    
    unname(overall.cor$estimate),
    
    unname(cor.lower$estimate),
    
    unname(cor.elevated$estimate),
    
    unname(cor.paris$estimate),
    
    unname(cor.qatar$estimate),
    
    unname(cor.turkey.syria$estimate),
    
    unname(cor.bakhmut$estimate),
    
    unname(cor.iran$estimate)
    
  ),
  
  BTC_Volatility_Coefficient = c(
    
    coef(overall.model)[2],
    
    coef(model.lower)[2],
    
    coef(model.elevated)[2],
    
    coef(model.paris)[2],
    
    coef(model.qatar)[2],
    
    coef(model.turkey.syria)[2],
    
    coef(model.bakhmut)[2],
    
    coef(model.iran)[2]
    
  ),
  
  Adj_R2 = c(
    
    summary(overall.model)$adj.r.squared,
    
    summary(model.lower)$adj.r.squared,
    
    summary(model.elevated)$adj.r.squared,
    
    summary(model.paris)$adj.r.squared,
    
    summary(model.qatar)$adj.r.squared,
    
    summary(model.turkey.syria)$adj.r.squared,
    
    summary(model.bakhmut)$adj.r.squared,
    
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
    
    nw.paris[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    
    nw.qatar[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    
    nw.turkey.syria[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    
    nw.bakhmut[
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

