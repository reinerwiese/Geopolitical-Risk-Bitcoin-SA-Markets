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


###############################################################
# Section A: Identification of Major Geopolitical Events
###############################################################

#4 Plot the geopolitical risk index
ggplot(
  data,
  aes(
    x = Date,
    y = GPRD
  )
) +
  geom_line(linewidth = 0.8) +
  theme_minimal() +
  labs(
    title = "Geopolitical Risk Index (2020-2026)",
    x = "Date",
    y = "GPR Index"
  )


#5 Structural break analysis
bp <- breakpoints(
  GPRD ~ 1,
  data = data
)

summary(bp)

plot(bp)

break.dates <- data.frame(
  
  Observation = bp$breakpoints,
  
  Date = data$Date[bp$breakpoints]
  
)

break.dates


#6 Smoothed geopolitical risk index
data$GPR_MA <- rollmean(
  data$GPRD,
  k = 21,
  fill = NA,
  align = "center"
)

ggplot(
  data,
  aes(
    x = Date
  )
) +
  geom_line(
    aes(y = GPRD),
    colour = "grey75",
    linewidth = 0.4
  ) +
  geom_line(
    aes(y = GPR_MA),
    colour = "blue",
    linewidth = 1
  ) +
  theme_minimal() +
  labs(
    title = "21-Day Moving Average of the Geopolitical Risk Index",
    x = "Date",
    y = "GPR Index"
  )


#7 Identification of local turning points
gpr.ma <- data$GPR_MA

maxima <- which(
  diff(sign(diff(gpr.ma))) == -2
) + 1

minima <- which(
  diff(sign(diff(gpr.ma))) == 2
) + 1

maxima <- maxima[
  !is.na(gpr.ma[maxima])
]

minima <- minima[
  !is.na(gpr.ma[minima])
]


#8 Peak prominence analysis
episode.table <- data.frame()

last.obs <- nrow(data)

for(i in seq_along(maxima)){
  
  peak <- maxima[i]
  
  left.min <- tail(
    minima[minima < peak],
    1
  )
  
  if(length(left.min) == 0)
    next
  
  right.min <- head(
    minima[minima > peak],
    1
  )
  
  if(length(right.min) == 0){
    
    right.min <- last.obs
    
  }
  
  left.value <- gpr.ma[left.min]
  
  right.value <- gpr.ma[right.min]
  
  prominence <- gpr.ma[peak] -
    max(left.value, right.value)
  
  episode.table <- rbind(
    
    episode.table,
    
    data.frame(
      
      Peak_Date = data$Date[peak],
      
      Peak_GPR = round(
        gpr.ma[peak],
        2
      ),
      
      Start_Date = data$Date[left.min],
      
      End_Date = data$Date[right.min],
      
      Start_GPR = round(
        left.value,
        2
      ),
      
      End_GPR = round(
        right.value,
        2
      ),
      
      Prominence = round(
        prominence,
        2
      )
      
    )
    
  )
  
}

episode.table <- episode.table[
  order(-episode.table$Prominence),
]

row.names(episode.table) <- NULL

episode.table


#9 Final geopolitical case studies
top.events <- episode.table[
  order(-episode.table$Prominence),
][1:2,]

events <- data.frame(
  
  Event = c(
    "Russia-Ukraine",
    "Israel-Hamas",
    "Iran-Israel"
  ),
  
  Start_Date = c(
    top.events$Start_Date[1],
    top.events$Start_Date[2],
    tail(break.dates$Date,1)
  ),
  
  End_Date = c(
    top.events$End_Date[1],
    top.events$End_Date[2],
    as.Date("2026-04-19")
  )
  
)

events


###############################################################
# Section B: Event Study Analysis
###############################################################

#10 Create event datasets
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


#11 Event summary statistics
event.statistics <- data.frame(
  
  Event = c(
    "Russia-Ukraine",
    "Israel-Hamas",
    "Iran-Israel"
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
  )
  
)

event.statistics[-1] <- round(
  event.statistics[-1],
  4
)

event.statistics


#12 Correlation analysis
cor.ukraine <- cor.test(
  ukraine$BTC_Volatility,
  ukraine$GPRD
)

cor.hamas <- cor.test(
  hamas$BTC_Volatility,
  hamas$GPRD
)

cor.iran <- cor.test(
  iran$BTC_Volatility,
  iran$GPRD
)

cor.ukraine

cor.hamas

cor.iran


#13 Event comparison
event.summary <- data.frame(
  
  Event = c(
    "Russia-Ukraine",
    "Israel-Hamas",
    "Iran-Israel"
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
  
  Mean_BTC_Volatility = c(
    mean(ukraine$BTC_Volatility),
    mean(hamas$BTC_Volatility),
    mean(iran$BTC_Volatility)
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

# Russia-Ukraine
model.ukraine <- lm(
  BTC_Volatility ~ GPRD,
  data = ukraine
)

summary(model.ukraine)

# Israel-Hamas
model.hamas <- lm(
  BTC_Volatility ~ GPRD,
  data = hamas
)

summary(model.hamas)

# Iran-Israel
model.iran <- lm(
  BTC_Volatility ~ GPRD,
  data = iran
)

summary(model.iran)


#16 Regression diagnostic tests

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
dw.ukraine <- dwtest(model.ukraine)

dw.hamas <- dwtest(model.hamas)

dw.iran <- dwtest(model.iran)

dw.ukraine
dw.hamas
dw.iran


# Breusch-Pagan tests
bp.ukraine <- bptest(model.ukraine)

bp.hamas <- bptest(model.hamas)

bp.iran <- bptest(model.iran)

bp.ukraine
bp.hamas
bp.iran


#17 Newey-West robust inference

# Russia-Ukraine
nw.ukraine <- coeftest(
  
  model.ukraine,
  
  vcov = NeweyWest(
    
    model.ukraine,
    
    prewhite = FALSE
    
  )
  
)

# Israel-Hamas
nw.hamas <- coeftest(
  
  model.hamas,
  
  vcov = NeweyWest(
    
    model.hamas,
    
    prewhite = FALSE
    
  )
  
)

# Iran-Israel
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


#18 Event regression comparison
event.regression <- data.frame(
  
  Event = c(
    "Russia-Ukraine",
    "Israel-Hamas",
    "Iran-Israel"
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
  
  Intercept = c(
    coef(model.ukraine)[1],
    coef(model.hamas)[1],
    coef(model.iran)[1]
  ),
  
  GPR_Coefficient = c(
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
    nw.ukraine["GPRD","Pr(>|t|)"],
    nw.hamas["GPRD","Pr(>|t|)"],
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


###############################################################
# Section C: Event Regression Analysis
###############################################################

#15 Event-specific regression models

# Russia-Ukraine
model.ukraine <- lm(
  BTC_Volatility ~ GPRD,
  data = ukraine
)

summary(model.ukraine)

# Israel-Hamas
model.hamas <- lm(
  BTC_Volatility ~ GPRD,
  data = hamas
)

summary(model.hamas)

# Iran-Israel
model.iran <- lm(
  BTC_Volatility ~ GPRD,
  data = iran
)

summary(model.iran)


#16 Regression diagnostic tests

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
dw.ukraine <- dwtest(model.ukraine)

dw.hamas <- dwtest(model.hamas)

dw.iran <- dwtest(model.iran)

dw.ukraine
dw.hamas
dw.iran


# Breusch-Pagan tests
bp.ukraine <- bptest(model.ukraine)

bp.hamas <- bptest(model.hamas)

bp.iran <- bptest(model.iran)

bp.ukraine
bp.hamas
bp.iran


#17 Newey-West robust inference

# Russia-Ukraine
nw.ukraine <- coeftest(
  
  model.ukraine,
  
  vcov = NeweyWest(
    
    model.ukraine,
    
    prewhite = FALSE
    
  )
  
)

# Israel-Hamas
nw.hamas <- coeftest(
  
  model.hamas,
  
  vcov = NeweyWest(
    
    model.hamas,
    
    prewhite = FALSE
    
  )
  
)

# Iran-Israel
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


#18 Event regression comparison
event.regression <- data.frame(
  
  Event = c(
    "Russia-Ukraine",
    "Israel-Hamas",
    "Iran-Israel"
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
  
  Intercept = c(
    coef(model.ukraine)[1],
    coef(model.hamas)[1],
    coef(model.iran)[1]
  ),
  
  GPR_Coefficient = c(
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
    nw.ukraine["GPRD","Pr(>|t|)"],
    nw.hamas["GPRD","Pr(>|t|)"],
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
