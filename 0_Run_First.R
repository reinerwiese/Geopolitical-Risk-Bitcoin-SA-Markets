# Run this script before any other script


# 1. LOAD PACKAGES

library(readxl)
library(dplyr)
library(rugarch)
library(zoo)


# 2. LOAD AND PREPARE DATA

data0 <- read_excel(
 "Thesis_Data.xlsx",
  sheet = "Data",
  na = "NA"
)

data0 <- na.omit(data0)

data0$Date <- as.Date(data0$Date)


# 3. EGARCH FUNCTION

fit_egarch <- function(returns) {
  
  spec <- ugarchspec(
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
  
  fit <- ugarchfit(
    spec = spec,
    data = returns
  )
  
  return(fit)
}


# 4. FINAL EGARCH MODELS

btc.fit <- fit_egarch(
  data0$BTC_log_returns
)

j303.fit <- fit_egarch(
  data0$Index_log_returns
)


# Canonical conditional volatility series

data0$BTC_Volatility <- as.numeric(
  sigma(btc.fit)
)

data0$J303_Volatility <- as.numeric(
  sigma(j303.fit)
)


# 5. GPR K-MEANS THRESHOLD FUNCTION

get_gpr_threshold <- function(gpr) {
  
  set.seed(123)
  
  kmeans.result <- kmeans(
    x = matrix(gpr, ncol = 1),
    centers = 2,
    nstart = 100
  )
  
  cluster.centres <- sort(
    as.numeric(kmeans.result$centers[, 1])
  )
  
  threshold <- mean(cluster.centres)
  
  return(threshold)
}


# 6. GPR THRESHOLDS

GPRD_threshold <- get_gpr_threshold(
  data0$GPRD
)

GPRD_ACT_threshold <- get_gpr_threshold(
  data0$GPRD_ACT
)

GPRD_THREAT_threshold <- get_gpr_threshold(
  data0$GPRD_THREAT
)


# 7. GPR REGIME FUNCTION

get_gpr_regime <- function(gpr, threshold) {
  
  regime <- ifelse(
    gpr < threshold,
    "Lower GPR",
    "Elevated GPR"
  )
  
  regime <- factor(
    regime,
    levels = c(
      "Lower GPR",
      "Elevated GPR"
    )
  )
  
  return(regime)
}


# 8. GPR REGIMES

data0$GPR_Regime <- get_gpr_regime(
  data0$GPRD,
  GPRD_threshold
)

data0$GPRD_ACT_Regime <- get_gpr_regime(
  data0$GPRD_ACT,
  GPRD_ACT_threshold
)

data0$GPRD_THREAT_Regime <- get_gpr_regime(
  data0$GPRD_THREAT,
  GPRD_THREAT_threshold
)


# 9. GPR EVENT IDENTIFICATION FUNCTION

identify_gpr_events <- function(data, gpr_column, ma_days = 21) {
  
  gpr <- data[[gpr_column]]
  
  gpr_ma <- rollmean(
    gpr,
    k = ma_days,
    fill = NA,
    align = "center"
  )
  
  maxima <- which(
    diff(sign(diff(gpr_ma))) == -2
  ) + 1
  
  minima <- which(
    diff(sign(diff(gpr_ma))) == 2
  ) + 1
  
  maxima <- maxima[
    !is.na(gpr_ma[maxima])
  ]
  
  minima <- minima[
    !is.na(gpr_ma[minima])
  ]
  
  last.obs <- max(
    which(!is.na(gpr_ma))
  )
  
  episode.table <- data.frame()
  
  for (i in maxima) {
    
    left.min <- minima[
      minima < i
    ]
    
    if (length(left.min) == 0) {
      next
    }
    
    left.min <- max(left.min)
    
    right.min <- minima[
      minima > i
    ]
    
    if (length(right.min) == 0) {
      right.min <- last.obs
    } else {
      right.min <- min(right.min)
    }
    
    peak.gpr <- gpr_ma[i]
    start.gpr <- gpr_ma[left.min]
    end.gpr <- gpr_ma[right.min]
    
    prominence <- peak.gpr -
      max(start.gpr, end.gpr)
    
    episode.table <- rbind(
      episode.table,
      data.frame(
        Peak_Date = data$Date[i],
        Peak_GPR = peak.gpr,
        Start_Date = data$Date[left.min],
        End_Date = data$Date[right.min],
        Start_GPR = start.gpr,
        End_GPR = end.gpr,
        Prominence = prominence
      )
    )
  }
  
  episode.table <- episode.table[
    order(-episode.table$Prominence),
  ]
  
  episode.table <- episode.table[
    seq_len(min(5, nrow(episode.table))),
  ]
  
  episode.table <- episode.table[
    order(episode.table$Start_Date),
  ]
  
  return(episode.table)
}


# 10. FINAL GPR EVENT WINDOWS

top.events <- identify_gpr_events(
  data0,
  "GPRD",
  ma_days = 21
)

events <- data.frame(
  Event = c(
    "Paris Attacks",
    "Qatar Diplomatic Crisis",
    "Turkey-Syria Escalation",
    "Russia-Ukraine / Bakhmut",
    "US-Israel-Iran Conflict"
  ),
  Peak_Date = top.events$Peak_Date,
  Peak_GPR = top.events$Peak_GPR,
  Start_Date = top.events$Start_Date,
  End_Date = top.events$End_Date,
  Start_GPR = top.events$Start_GPR,
  End_GPR = top.events$End_GPR,
  Prominence = top.events$Prominence
)

