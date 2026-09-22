# ============================================================
# 1. LOAD PACKAGES
# ============================================================

library(readxl)
library(dplyr)
library(rugarch)
library(zoo)


# ============================================================
# 2. LOAD AND PREPARE DATA
# ============================================================

data <- read_excel(
  "Thesis_Data.xlsx",
  sheet = "Data",
  na = "NA"
)

# Remove observations containing missing values
data <- na.omit(data)

# Ensure Date is stored as Date
data$Date <- as.Date(data$Date)

# Ensure observations are in chronological order
data <- data %>%
  arrange(Date)


# Common vectors used by downstream scripts
btc.returns  <- data$BTC_log_returns
j303.returns <- data$Index_log_returns
gpr.raw      <- data$GPRD


# ============================================================
# 3. FINAL BITCOIN EGARCH MODEL
# ============================================================

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

# Canonical Bitcoin conditional volatility series
data$BTC_Volatility <- as.numeric(
  sigma(btc.fit)
)


# ============================================================
# 4. FINAL J303 EGARCH MODEL
# ============================================================

j303.spec <- ugarchspec(
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

j303.fit <- ugarchfit(
  spec = j303.spec,
  data = j303.returns
)

# Canonical J303 conditional volatility series
data$J303_Volatility <- as.numeric(
  sigma(j303.fit)
)

# ============================================================
# 5. BASELINE J303-BITCOIN VOLATILITY REGRESSION
# ============================================================

j303.btc <- lm(
  J303_Volatility ~ BTC_Volatility,
  data = data
)

# ============================================================
# 6. GPR K-MEANS CLUSTERING
# ============================================================

set.seed(123)

kmeans.2 <- kmeans(
  x = matrix(gpr.raw, ncol = 1),
  centers = 2,
  nstart = 100
)

# Store the cluster assignment
data$GPR_Cluster <- kmeans.2$cluster

# Sort the two cluster centres from low to high
GPR_cluster_centres <- sort(
  as.numeric(kmeans.2$centers[, 1])
)


# ============================================================
# 7. DATA-DRIVEN GPR THRESHOLD
# ============================================================

# The threshold is the midpoint between the two
# k-means cluster centres.
GPR_threshold <- mean(GPR_cluster_centres)

# Classify observations using the calculated threshold
data$GPR_Regime <- ifelse(
  data$GPRD < GPR_threshold,
  "Lower GPR",
  "Elevated GPR"
)

# Make the ordering explicit
data$GPR_Regime <- factor(
  data$GPR_Regime,
  levels = c("Lower GPR", "Elevated GPR")
)


# ============================================================
# 8. DYNAMIC IDENTIFICATION OF GPR EVENT WINDOWS
# ============================================================

# 21-day centred moving average
data$GPR_MA <- rollmean(
  data$GPRD,
  k = 21,
  fill = NA,
  align = "center"
)


# ------------------------------------------------------------
# 8.1 Identify local maxima and minima
# ------------------------------------------------------------

gpr.ma <- data$GPR_MA

maxima <- which(
  diff(sign(diff(gpr.ma))) == -2
) + 1

minima <- which(
  diff(sign(diff(gpr.ma))) == 2
) + 1

# Remove observations where the moving average is NA
maxima <- maxima[
  !is.na(gpr.ma[maxima])
]

minima <- minima[
  !is.na(gpr.ma[minima])
]


# ------------------------------------------------------------
# 8.2 Construct GPR episodes
# ------------------------------------------------------------

episode.table <- data.frame()

last.obs <- max(
  which(!is.na(gpr.ma))
)

for (i in maxima) {
  
  # Find the nearest local minimum before the peak
  left.min <- minima[
    minima < i
  ]
  
  if (length(left.min) == 0) {
    next
  }
  
  left.min <- max(left.min)
  
  # Find the nearest local minimum after the peak
  right.min <- minima[
    minima > i
  ]
  
  if (length(right.min) == 0) {
    right.min <- last.obs
  } else {
    right.min <- min(right.min)
  }
  
  # GPR values
  peak.gpr <- gpr.ma[i]
  start.gpr <- gpr.ma[left.min]
  end.gpr <- gpr.ma[right.min]
  
  # Prominence of the episode
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


# ------------------------------------------------------------
# 8.3 Select the five most prominent events
# ------------------------------------------------------------

top.events <- episode.table[
  order(-episode.table$Prominence),
]

# Keep the five most prominent events
top.events <- top.events[
  seq_len(min(5, nrow(top.events))),
]

# Put selected events into chronological order
top.events <- top.events[
  order(top.events$Start_Date),
]


# ============================================================
# 9. CREATE FINAL EVENT WINDOW OBJECT
# ============================================================

event.names <- c(
  "Russia-Ukraine (Crimea Crisis)",
  "Paris Attacks",
  "Russia-Ukraine Invasion",
  "Israel-Hamas War",
  "US-Israel-Iran Conflict"
)

if (nrow(top.events) != length(event.names)) {
  stop(
    "The dynamic GPR procedure did not identify exactly five events."
  )
}

events <- data.frame(
  Event = event.names,
  Start_Date = top.events$Start_Date,
  End_Date = top.events$End_Date
)
