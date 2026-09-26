#1 Load required packages
library(dplyr)
library(ggplot2)

library(FinTS)
library(tseries)
library(moments)

library(lmtest)
library(sandwich)
library(car)

library(pracma)
library(strucchange)
library(zoo)

source("0_Run_First.R")

data <- data0

###############################################################
# Section B: Event Study Analysis
###############################################################

#10 Create event datasets from shared setup

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

#11 Event summary statistics

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

event.statistics


#12 Correlation analysis

cor.paris <- cor.test(
  paris$BTC_Volatility,
  paris$GPRD
)

cor.qatar <- cor.test(
  qatar$BTC_Volatility,
  qatar$GPRD
)

cor.turkey.syria <- cor.test(
  turkey.syria$BTC_Volatility,
  turkey.syria$GPRD
)

cor.bakhmut <- cor.test(
  bakhmut$BTC_Volatility,
  bakhmut$GPRD
)

cor.iran <- cor.test(
  iran$BTC_Volatility,
  iran$GPRD
)

cor.paris

cor.qatar

cor.turkey.syria

cor.bakhmut

cor.iran


#13 Event comparison

event.summary <- data.frame(
  
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
  
  Mean_BTC_Volatility = c(
    mean(paris$BTC_Volatility),
    mean(qatar$BTC_Volatility),
    mean(turkey.syria$BTC_Volatility),
    mean(bakhmut$BTC_Volatility),
    mean(iran$BTC_Volatility)
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


#14 Format event comparison table

event.summary$Mean_GPR <-
  round(event.summary$Mean_GPR,2)

event.summary$Mean_BTC_Volatility <-
  round(event.summary$Mean_BTC_Volatility,5)

event.summary$Correlation <-
  round(event.summary$Correlation,4)

event.summary$Correlation_P_Value <-
  signif(event.summary$Correlation_P_Value,4)

event.summary$Significant <- ifelse(
  event.summary$Correlation_P_Value < 0.05,
  "Yes",
  "No"
)

print(event.summary)


###############################################################
# Section C: Event Regression Analysis
###############################################################

#15 Event-specific regression models

# Paris Attacks
model.paris <- lm(
  BTC_Volatility ~ GPRD,
  data = paris
)

summary(model.paris)

# Qatar Diplomatic Crisis
model.qatar <- lm(
  BTC_Volatility ~ GPRD,
  data = qatar
)

summary(model.qatar)

# Turkey-Syria Escalation
model.turkey.syria <- lm(
  BTC_Volatility ~ GPRD,
  data = turkey.syria
)

summary(model.turkey.syria)

# Russia-Ukraine / Bakhmut
model.bakhmut <- lm(
  BTC_Volatility ~ GPRD,
  data = bakhmut
)

summary(model.bakhmut)

# US-Israel-Iran Conflict
model.iran <- lm(
  BTC_Volatility ~ GPRD,
  data = iran
)

summary(model.iran)

#16 Regression diagnostic tests

# Diagnostic plots
par(mfrow = c(2,2))

plot(model.paris)

plot(model.qatar)

plot(model.turkey.syria)

plot(model.bakhmut)

plot(model.iran)

par(mfrow = c(1,1))


# Jarque-Bera tests
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


# Durbin-Watson tests
dw.paris <- dwtest(model.paris)

dw.qatar <- dwtest(model.qatar)

dw.turkey.syria <- dwtest(model.turkey.syria)

dw.bakhmut <- dwtest(model.bakhmut)

dw.iran <- dwtest(model.iran)

dw.paris
dw.qatar
dw.turkey.syria
dw.bakhmut
dw.iran


# Breusch-Pagan tests
bp.paris <- bptest(model.paris)

bp.qatar <- bptest(model.qatar)

bp.turkey.syria <- bptest(model.turkey.syria)

bp.bakhmut <- bptest(model.bakhmut)

bp.iran <- bptest(model.iran)

bp.paris
bp.qatar
bp.turkey.syria
bp.bakhmut
bp.iran


#17 Newey-West robust inference

# Paris Attacks
nw.paris <- coeftest(
  
  model.paris,
  
  vcov = NeweyWest(
    
    model.paris,
    
    prewhite = FALSE
    
  )
  
)

# Qatar Diplomatic Crisis
nw.qatar <- coeftest(
  
  model.qatar,
  
  vcov = NeweyWest(
    
    model.qatar,
    
    prewhite = FALSE
    
  )
  
)

# Turkey-Syria Escalation
nw.turkey.syria <- coeftest(
  
  model.turkey.syria,
  
  vcov = NeweyWest(
    
    model.turkey.syria,
    
    prewhite = FALSE
    
  )
  
)

# Russia-Ukraine / Bakhmut
nw.bakhmut <- coeftest(
  
  model.bakhmut,
  
  vcov = NeweyWest(
    
    model.bakhmut,
    
    prewhite = FALSE
    
  )
  
)

# US-Israel-Iran Conflict
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


#18 Event regression comparison
event.regression <- data.frame(
  
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
  
  Intercept = c(
    coef(model.paris)[1],
    coef(model.qatar)[1],
    coef(model.turkey.syria)[1],
    coef(model.bakhmut)[1],
    coef(model.iran)[1]
  ),
  
  GPR_Coefficient = c(
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
    nw.paris["GPRD","Pr(>|t|)"],
    nw.qatar["GPRD","Pr(>|t|)"],
    nw.turkey.syria["GPRD","Pr(>|t|)"],
    nw.bakhmut["GPRD","Pr(>|t|)"],
    nw.iran["GPRD","Pr(>|t|)"]
  )
  
)


#19 Format event regression table
event.regression$Sample_Size <-
  round(event.regression$Sample_Size,0)

event.regression$Correlation <-
  round(event.regression$Correlation,4)

event.regression$Intercept <-
  round(event.regression$Intercept,4)

event.regression$GPR_Coefficient <-
  signif(event.regression$GPR_Coefficient,4)

event.regression$Adj_R2 <-
  round(event.regression$Adj_R2,4)

event.regression$Residual_SE <-
  round(event.regression$Residual_SE,5)

event.regression$NeweyWest_P_Value <-
  signif(event.regression$NeweyWest_P_Value,4)

event.regression$Significant <- ifelse(
  
  event.regression$NeweyWest_P_Value < 0.05,
  
  "Yes",
  
  "No"
  
)

print(event.regression)
