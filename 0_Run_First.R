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


# 10. GPR EVENT WINDOWS

GPRD_events_11 <- identify_gpr_events(
  data0,
  "GPRD",
  ma_days = 11
)

GPRD_events_21 <- identify_gpr_events(
  data0,
  "GPRD",
  ma_days = 21
)

GPRD_events_31 <- identify_gpr_events(
  data0,
  "GPRD",
  ma_days = 31
)


GPRD_ACT_events_11 <- identify_gpr_events(
  data0,
  "GPRD_ACT",
  ma_days = 11
)

GPRD_ACT_events_21 <- identify_gpr_events(
  data0,
  "GPRD_ACT",
  ma_days = 21
)

GPRD_ACT_events_31 <- identify_gpr_events(
  data0,
  "GPRD_ACT",
  ma_days = 31
)


GPRD_THREAT_events_11 <- identify_gpr_events(
  data0,
  "GPRD_THREAT",
  ma_days = 11
)

GPRD_THREAT_events_21 <- identify_gpr_events(
  data0,
  "GPRD_THREAT",
  ma_days = 21
)

GPRD_THREAT_events_31 <- identify_gpr_events(
  data0,
  "GPRD_THREAT",
  ma_days = 31
)


# 11. EVENT RESULTS

format_event_results <- function(events, measure, smoothing) {
  
  events$Measure <- measure
  events$Smoothing <- smoothing
  
  events <- events[
    ,
    c(
      "Measure",
      "Smoothing",
      "Peak_Date",
      "Peak_GPR",
      "Start_Date",
      "End_Date",
      "Start_GPR",
      "End_GPR",
      "Prominence"
    )
  ]
  
  return(events)
}


event_results <- bind_rows(
  
  format_event_results(
    GPRD_events_11,
    "GPRD",
    11
  ),
  
  format_event_results(
    GPRD_events_21,
    "GPRD",
    21
  ),
  
  format_event_results(
    GPRD_events_31,
    "GPRD",
    31
  ),
  
  format_event_results(
    GPRD_ACT_events_11,
    "GPRD_ACT",
    11
  ),
  
  format_event_results(
    GPRD_ACT_events_21,
    "GPRD_ACT",
    21
  ),
  
  format_event_results(
    GPRD_ACT_events_31,
    "GPRD_ACT",
    31
  ),
  
  format_event_results(
    GPRD_THREAT_events_11,
    "GPRD_THREAT",
    11
  ),
  
  format_event_results(
    GPRD_THREAT_events_21,
    "GPRD_THREAT",
    21
  ),
  
  format_event_results(
    GPRD_THREAT_events_31,
    "GPRD_THREAT",
    31
  )
) %>%
  arrange(
    Measure,
    Smoothing,
    Peak_Date
  )


# 12. HISTORICAL GPR EVENTS

historical_events <- data.frame(
  
  Event = c(
    "Crimea",
    "Israel-Gaza",
    "Russia-Ukraine",
    "Suruc",
    "Paris",
    "US-Iran",
    "Aleppo-Syria",
    "Israel-Syria",
    "Qatar",
    "North Korea",
    "India-Pakistan",
    "Saudi-US-Iran",
    "US-Iran",
    "Israel-Gaza",
    "Russia-Ukraine",
    "Russia-Ukraine",
    "Bakhmut",
    "Israel-Hamas",
    "Russia-Ukraine",
    "Israel-Iran",
    "Iran"
  ),
  
  Start_Date = as.Date(c(
    "2014-03-07",
    "2014-08-19",
    "2015-01-20",
    "2015-07-20",
    "2015-11-13",
    "2016-01-12",
    "2016-12-01",
    "2017-01-13",
    "2017-06-05",
    "2017-07-04",
    "2019-02-14",
    "2019-09-14",
    "2020-01-03",
    "2021-05-10",
    "2022-02-24",
    "2022-07-14",
    "2023-05-01",
    "2023-10-07",
    "2025-03-03",
    "2025-06-13",
    "2026-02-28"
  )),
  
  End_Date = as.Date(c(
    "2014-03-25",
    "2014-08-26",
    "2015-02-18",
    "2015-07-27",
    "2015-11-30",
    "2016-01-13",
    "2016-12-19",
    "2017-01-14",
    "2017-06-29",
    "2017-07-28",
    "2019-03-15",
    "2019-10-08",
    "2020-01-13",
    "2021-05-21",
    "2022-03-16",
    "2022-07-17",
    "2023-05-31",
    "2023-11-21",
    "2025-03-17",
    "2025-06-30",
    "2026-05-13"
  ))
)


# 13. MATCH GPR EVENTS TO HISTORICAL EVENTS

match_historical_events <- function(event_results, historical_events) {
  
  matched_events <- lapply(seq_len(nrow(event_results)), function(i) {
    
    event <- event_results[i, ]
    
    # Allow a buffer around the historical event
    # corresponding to the centred smoothing window.
    
    buffer_days <- floor(
      event$Smoothing / 2
    )
    
    historical_events_buffered <- historical_events
    
    historical_events_buffered$Start_Date <- 
      historical_events_buffered$Start_Date -
      buffer_days
    
    historical_events_buffered$End_Date <- 
      historical_events_buffered$End_Date +
      buffer_days
    
    historical_events_buffered$Overlap_Days <- pmax(
      0,
      as.numeric(
        pmin(
          event$End_Date,
          historical_events_buffered$End_Date
        ) -
          pmax(
            event$Start_Date,
            historical_events_buffered$Start_Date
          )
      ) + 1
    )
    
    overlap <- historical_events_buffered[
      historical_events_buffered$Overlap_Days > 0,
    ]
    
    measure_code <- case_when(
      event$Measure == "GPRD" ~ "G",
      event$Measure == "GPRD_ACT" ~ "A",
      event$Measure == "GPRD_THREAT" ~ "T",
      TRUE ~ NA_character_
    )
    
    event_code <- paste0(
      measure_code,
      event$Smoothing
    )
    
    if (nrow(overlap) == 0) {
      
      data.frame(
        Event = paste0("Unknown ", event_code),
        Start_Date = event$Start_Date,
        End_Date = event$End_Date,
        Peak_Date = event$Peak_Date,
        Measure = event$Measure,
        Smoothing = event$Smoothing,
        Peak_GPR = event$Peak_GPR,
        Prominence = event$Prominence
      )
      
    } else {
      
      max_overlap <- max(
        overlap$Overlap_Days
      )
      
      selected_event <- overlap[
        overlap$Overlap_Days == max_overlap,
      ][1, ]
      
      data.frame(
        Event = paste0(
          selected_event$Event,
          " ",
          event_code
        ),
        Start_Date = event$Start_Date,
        End_Date = event$End_Date,
        Peak_Date = event$Peak_Date,
        Measure = event$Measure,
        Smoothing = event$Smoothing,
        Peak_GPR = event$Peak_GPR,
        Prominence = event$Prominence
      )
    }
  })
  
  matched_events <- as.data.frame(
    do.call(rbind, matched_events)
  )
  
  event_counts <- table(
    matched_events$Event
  )
  
  repeated_events <- names(
    event_counts[
      event_counts > 1
    ]
  )
  
  for (event_name in repeated_events) {
    
    rows <- which(
      matched_events$Event == event_name
    )
    
    matched_events$Event[rows] <- paste0(
      event_name,
      "-",
      seq_along(rows)
    )
  }
  
  return(matched_events)
}


event_matches <- match_historical_events(
  event_results,
  historical_events
)


