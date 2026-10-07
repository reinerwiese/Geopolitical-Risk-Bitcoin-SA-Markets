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
  
  event.table <- data.frame()
  
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
    
    event.table <- rbind(
      event.table,
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
  
  event.table <- event.table[
    order(-event.table$Prominence),
  ]
  
  event.table <- event.table[
    seq_len(min(5, nrow(event.table))),
  ]
  
  event.table <- event.table[
    order(event.table$Start_Date),
  ]
  
  return(event.table)
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


# 11. COMBINE EVENT RESULTS

format_event_results <- function(events, measure, smoothing) {
  
  events %>%
    mutate(
      Measure = measure,
      Smoothing = smoothing
    )
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
)


# 12. EVENT ANALYSIS WINDOWS

create_event_windows <- function(event_results, data) {
  
  event_results$Start_Row <- match(
    event_results$Start_Date,
    data$Date
  )
  
  event_results$End_Row <- match(
    event_results$End_Date,
    data$Date
  )
  
  
  event_results$Peak_Row <- match(
    event_results$Peak_Date,
    data$Date
  )
  
  event_results$Half_Window <- floor(
    event_results$Smoothing / 2
  )
  
  event_results$News_Start_Row <- pmax(
    1,
    event_results$Peak_Row -
      event_results$Half_Window
  )
  
  event_results$News_End_Row <- pmin(
    nrow(data),
    event_results$Peak_Row +
      event_results$Half_Window
  )
  
  event_results$News_Start_Date <- data$Date[
    event_results$News_Start_Row
  ]
  
  event_results$News_End_Date <- data$Date[
    event_results$News_End_Row
  ]
  
  return(event_results)
}

event_results <- create_event_windows(
  event_results,
  data0
)

event_results$News_Observations <-
  event_results$News_End_Row -
  event_results$News_Start_Row +
  1

if (
  any(
    event_results$News_Observations !=
    event_results$Smoothing
  )
) {
  
  stop(
    "One or more fixed event-analysis windows do not contain the required number of trading observations."
  )
}


# 13. TOP THREE RAW GPR PEAKS WITHIN EACH FULL SMOOTHED EVENT PERIOD

get_top_raw_peaks <- function(event_results, data) {
  
  top_raw_results <- lapply(
    seq_len(nrow(event_results)),
    function(i) {
      
      gpr_column <- event_results$Measure[i]
      
      start_row <- event_results$Start_Row[i]
      end_row <- event_results$End_Row[i]
      
      raw_window <- data.frame(
        Date = data$Date[
          start_row:end_row
        ],
        Raw_GPR = data[[gpr_column]][
          start_row:end_row
        ]
      )
      
      raw_window <- raw_window %>%
        filter(
          !is.na(Raw_GPR)
        ) %>%
        arrange(
          desc(Raw_GPR),
          Date
        )
      
      if (nrow(raw_window) < 3) {
        
        stop(
          "One or more smoothed event periods contain fewer than three raw GPR observations."
        )
      }
      
      data.frame(
        Top_Raw_Date_1 = raw_window$Date[1],
        Top_Raw_GPR_1 = raw_window$Raw_GPR[1],
        
        Top_Raw_Date_2 = raw_window$Date[2],
        Top_Raw_GPR_2 = raw_window$Raw_GPR[2],
        
        Top_Raw_Date_3 = raw_window$Date[3],
        Top_Raw_GPR_3 = raw_window$Raw_GPR[3]
      )
      
    }
  )
  
  return(
    bind_rows(top_raw_results)
  )
}


top_raw_peaks <- get_top_raw_peaks(
  event_results,
  data0
)


event_results <- bind_cols(
  event_results,
  top_raw_peaks
)

if (
  any(
    event_results$Top_Raw_Date_1 <
    event_results$Start_Date |
    event_results$Top_Raw_Date_1 >
    event_results$End_Date |
    event_results$Top_Raw_Date_2 <
    event_results$Start_Date |
    event_results$Top_Raw_Date_2 >
    event_results$End_Date |
    event_results$Top_Raw_Date_3 <
    event_results$Start_Date |
    event_results$Top_Raw_Date_3 >
    event_results$End_Date
  )
) {
  
  stop(
    "One or more top raw GPR peaks fall outside the full smoothed event period."
  )
}


# 14. EVENT IDENTIFICATION TABLE

event_results <- event_results %>%
  arrange(
    Measure,
    Smoothing,
    Peak_Date
  ) %>%
  group_by(
    Measure,
    Smoothing
  ) %>%
  mutate(
    Event_ID = paste0(
      Measure,
      "_",
      Smoothing,
      "_",
      row_number()
    )
  ) %>%
  ungroup()


# 15. IMPORT EXCEL EVENT AUDIT AND NAME EVENTS

event_audit <- read_excel(
  "Thesis_GPR_Event_Audit_FINAL_VERIFIED.xlsx",
  sheet = "45 Event Audit"
) %>%
  select(
    Event_ID,
    Other_Event_IDs_Within_Window,
    Shared_Highest_Raw_Day_Event_IDs,
    Episode_Link_Status,
    Other_Selected_Raw_Peak_IDs_Within_Window,
    Additional_Event_Codes,
    Additional_Event_Names,
    Combined_Event_Codes,
    Combined_Event_Names
  )


raw_peak_audit <- read_excel(
  "Thesis_GPR_Event_Audit_FINAL_VERIFIED.xlsx",
  sheet = "135 Raw Peak Audit"
) %>%
  select(
    Event_ID,
    Raw_Rank,
    Underlying_Event_Code,
    Underlying_Event
  ) %>%
  arrange(
    Event_ID,
    Raw_Rank
  )


raw_peak_counts <- raw_peak_audit %>%
  count(Event_ID)

if (
  any(
    raw_peak_counts$n != 3
  )
) {
  
  stop(
    "One or more Event_IDs do not have exactly three audited raw peaks."
  )
}

raw_peak_events <- raw_peak_audit %>%
  group_by(
    Event_ID
  ) %>%
  summarise(
    
    Top3_Event_Codes = paste(
      Underlying_Event_Code,
      collapse = "; "
    ),
    
    Top3_Event_Name = paste(
      unique(Underlying_Event),
      collapse = "; "
    ),
    
    Event_Code_1 = Underlying_Event_Code[Raw_Rank == 1][1],
    Event_Code_2 = Underlying_Event_Code[Raw_Rank == 2][1],
    Event_Code_3 = Underlying_Event_Code[Raw_Rank == 3][1],
    
    Event_Name_1 = Underlying_Event[Raw_Rank == 1][1],
    Event_Name_2 = Underlying_Event[Raw_Rank == 2][1],
    Event_Name_3 = Underlying_Event[Raw_Rank == 3][1],
    
    .groups = "drop"
  )


event_results <- event_results %>%
  left_join(
    raw_peak_events,
    by = "Event_ID"
  ) %>%
  left_join(
    event_audit,
    by = "Event_ID"
  )


event_results <- event_results %>%
  mutate(
    Event_Codes = Combined_Event_Codes,
    Event_Name = Combined_Event_Names
  )


if (
  any(
    is.na(event_results$Event_Name) |
    event_results$Event_Name == ""
  )
) {
  
  stop(
    "One or more R-generated events do not have a verified event name in the Excel audit."
  )
}

if (
  any(
    is.na(event_results$Event_Codes) |
    event_results$Event_Codes == ""
  )
) {
  
  stop(
    "One or more R-generated events do not have verified E01-E36 event codes."
  )
}


get_event_codes <- function(x) {
  
  if (
    length(x) == 0 |
    is.na(x) |
    trimws(x) == "" |
    trimws(x) == "None"
  ) {
    
    return(
      character(0)
    )
  }
  
  x <- unlist(
    strsplit(
      x,
      ";"
    )
  )
  
  x <- trimws(x)
  
  x <- x[
    !is.na(x) &
      x != "" &
      x != "None"
  ]
  
  return(
    unique(x)
  )
}


combined_event_check <- mapply(
  function(
    top3,
    combined
  ) {
    
    all(
      get_event_codes(top3) %in%
        get_event_codes(combined)
    )
    
  },
  event_results$Top3_Event_Codes,
  event_results$Event_Codes
)

if (
  any(
    !combined_event_check
  )
) {
  
  stop(
    "One or more original top-three E-codes are missing from the combined event list."
  )
}


additional_event_check <- mapply(
  function(
    additional,
    combined
  ) {
    
    additional_codes <- get_event_codes(
      additional
    )
    
    combined_codes <- get_event_codes(
      combined
    )
    
    all(
      additional_codes %in%
        combined_codes
    )
    
  },
  event_results$Additional_Event_Codes,
  event_results$Event_Codes
)

if (
  any(
    !additional_event_check
  )
) {
  
  stop(
    "One or more additional E-codes are missing from the combined event list."
  )
}


ecode_name_check <- raw_peak_audit %>%
  distinct(
    Underlying_Event_Code,
    Underlying_Event
  ) %>%
  count(
    Underlying_Event_Code
  )

if (
  any(
    ecode_name_check$n != 1
  )
) {
  
  stop(
    "One or more E-codes are linked to multiple underlying event names."
  )
}


# 16. E-CODE ROBUSTNESS OVERLAP

event_code_data <- event_results %>%
  mutate(
    Event_Group = paste(
      Measure,
      Smoothing,
      sep = "_"
    )
  ) %>%
  select(
    Event_Group,
    Event_Codes
  )

get_event_codes <- function(x) {
  
  if (
    length(x) == 0 |
    is.na(x) |
    x == "" |
    trimws(x) == "None"
  ) {
    return(character(0))
  }
  
  x <- unlist(
    strsplit(
      x,
      ";"
    )
  )
  
  x <- trimws(x)
  
  x <- x[
    !is.na(x) &
      x != "" &
      x != "None"
  ]
  
  return(
    unique(x)
  )
}

event_code_sets <- split(
  event_code_data$Event_Codes,
  event_code_data$Event_Group
)

event_code_sets <- lapply(
  event_code_sets,
  function(x) {
    
    codes <- unlist(
      lapply(
        x,
        get_event_codes
      )
    )
    
    sort(
      unique(codes)
    )
  }
)

event_groups <- sort(
  names(event_code_sets)
)

event_overlap <- do.call(
  rbind,
  lapply(
    seq_len(
      length(event_groups) - 1
    ),
    function(i) {
      
      do.call(
        rbind,
        lapply(
          (i + 1):length(event_groups),
          function(j) {
            
            group_1 <- event_groups[i]
            group_2 <- event_groups[j]
            
            shared_codes <- sort(
              intersect(
                event_code_sets[[group_1]],
                event_code_sets[[group_2]]
              )
            )
            
            data.frame(
              Group_1 = group_1,
              Group_2 = group_2,
              Shared_Event_Codes = if (
                length(shared_codes) == 0
              ) {
                "None"
              } else {
                paste(
                  shared_codes,
                  collapse = "; "
                )
              }
            )
          }
        )
      )
    }
  )
)


event_overlap


# 17. FINAL EVENT TABLE

event_dates <- event_results %>%
  arrange(
    Measure,
    Smoothing,
    Peak_Date
  ) %>%
  select(
    Event_ID,
    Event_Name,
    Event_Codes,
    Measure,
    Smoothing,
    Peak_Date,
    Start_Date,
    End_Date,
    Top_Raw_Date_1,
    Top_Raw_GPR_1,
    Top_Raw_Date_2,
    Top_Raw_GPR_2,
    Top_Raw_Date_3,
    Top_Raw_GPR_3,
    News_Start_Date,
    News_End_Date,
    everything()
  )




