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


# 11. COMBINE EVENT RESULTS

format_event_results <- function(events, measure, smoothing) {
  
  events %>%
    mutate(
      Measure = measure,
      Smoothing = smoothing
    )
}

event_results <- bind_rows(
  format_event_results(GPRD_events_11, "GPRD", 11),
  format_event_results(GPRD_events_21, "GPRD", 21),
  format_event_results(GPRD_events_31, "GPRD", 31),
  
  format_event_results(GPRD_ACT_events_11, "GPRD_ACT", 11),
  format_event_results(GPRD_ACT_events_21, "GPRD_ACT", 21),
  format_event_results(GPRD_ACT_events_31, "GPRD_ACT", 31),
  
  format_event_results(GPRD_THREAT_events_11, "GPRD_THREAT", 11),
  format_event_results(GPRD_THREAT_events_21, "GPRD_THREAT", 21),
  format_event_results(GPRD_THREAT_events_31, "GPRD_THREAT", 31)
)


# 12. NEWS SEARCH WINDOWS

create_news_windows <- function(event_results, data) {
  
  event_results$Peak_Row <- match(
    event_results$Peak_Date,
    data$Date
  )
  
  event_results$Half_Window <- floor(
    event_results$Smoothing / 2
  )
  
  event_results$News_Start_Row <- pmax(
    1,
    event_results$Peak_Row - event_results$Half_Window
  )
  
  event_results$News_End_Row <- pmin(
    nrow(data),
    event_results$Peak_Row + event_results$Half_Window
  )
  
  event_results$News_Start_Date <- data$Date[
    event_results$News_Start_Row
  ]
  
  event_results$News_End_Date <- data$Date[
    event_results$News_End_Row
  ]
  
  return(event_results)
}

event_results <- create_news_windows(
  event_results,
  data0
)


# 13. EVENT IDENTIFICATION TABLE

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


# EVENT NAMES

event_names <- c(
  
  "2014-03-17" =
    "2014 - Crimea referendum / Russia-Ukraine confrontation",
  
  "2014-08-21" =
    "2014 - Israel-Gaza conflict / Russia-Ukraine tensions",
  
  "2015-01-29" =
    "2015 - Russia-Ukraine conflict / Debaltseve fighting",
  
  "2015-07-22" =
    "2015 - Suruç bombing / Turkey-PKK escalation",
  
  "2015-11-23" =
    "2015 - Paris attacks / European anti-terror response",
  
  "2015-11-30" =
    "2015 - Russia-Turkey confrontation after Russian jet downing",
  
  "2015-12-07" =
    "2015 - Russia-Turkey tensions over Syria",
  
  "2016-01-12" =
    "2016 - Istanbul bombing / Islamic State",
  
  "2016-12-13" =
    "2016 - Battle of Aleppo / evacuation crisis",
  
  "2017-01-13" =
    "2017 - Israel-Syria military confrontation near Damascus",
  
  "2017-06-07" =
    "2017 - Qatar diplomatic crisis / Tehran terrorist attacks",
  
  "2017-07-25" =
    "2017 - North Korea missile/nuclear threat",
  
  "2019-03-08" =
    "2019 - India-Pakistan tensions / Jammu attack / North Korea-US tensions",
  
  "2019-10-02" =
    "2019 - Turkey-Syria cross-border offensive threat / Saudi-Iran tensions",
  
  "2020-01-06" =
    "2020 - US-Iran confrontation after Soleimani killing",
  
  "2020-01-09" =
    "2020 - Iranian missile strikes on US forces / Ukrainian airliner downing",
  
  "2021-05-06" =
    "2021 - Sheikh Jarrah eviction dispute / Jerusalem clashes",
  
  "2021-05-20" =
    "2021 - Israel-Gaza fighting / ceasefire negotiations",
  
  "2021-05-21" =
    "2021 - Israel-Gaza ceasefire",
  
  "2021-06-01" =
    "2021 - Gaza ceasefire and reconstruction aftermath",
  
  "2022-03-01" =
    "2022 - Russia's invasion of Ukraine",
  
  "2022-03-08" =
    "2022 - Russia's invasion of Ukraine / humanitarian corridors",
  
  "2022-07-15" =
    "2022 - Vinnytsia missile strike / Russia-Ukraine war",
  
  "2023-05-24" =
    "2023 - Belgorod cross-border incursion / Russia-Ukraine war",
  
  "2023-10-27" =
    "2023 - Israel-Hamas war / Israeli ground operations in Gaza",
  
  "2025-03-06" =
    "2025 - US suspension of military and intelligence support to Ukraine",
  
  "2025-03-13" =
    "2025 - Russia-Ukraine ceasefire negotiations / Putin response",
  
  "2025-06-05" =
    "2025 - Gaza humanitarian crisis / aid disruption",
  
  "2025-06-20" =
    "2025 - Israel-Iran war / US decision on intervention",
  
  "2025-06-24" =
    "2025 - Israel-Iran ceasefire",
  
  "2026-03-10" =
    "2026 - Iran war / Strait of Hormuz and energy disruption",
  
  "2026-03-17" =
    "2026 - Iran war / Strait of Hormuz and allied response",
  
  "2026-03-24" =
    "2026 - US-Iran negotiations / Strait of Hormuz",
  
  "2026-03-31" =
    "2026 - Iran war / Gulf shipping and Strait of Hormuz"
)


event_results <- event_results %>%
  mutate(
    Event_Name = unname(
      event_names[as.character(Peak_Date)]
    )
  )


# 14. OVERLAPPING EVENT WINDOWS

# Overlap is used to identify candidate matches across smoothing specifications.
# The final Episode assignment is manually validated below.

match_event_candidates <- function(x) {
  
  x <- x[
    order(
      x$Peak_Row
    ),
  ]
  
  x$Candidate_Episode <- NA_integer_
  
  episode <- 0
  
  for (i in seq_len(nrow(x))) {
    
    if (!is.na(x$Candidate_Episode[i])) {
      next
    }
    
    episode <- episode + 1
    
    x$Candidate_Episode[i] <- episode
    
    repeat {
      
      current_rows <- which(
        x$Candidate_Episode == episode
      )
      
      matched_rows <- which(
        sapply(
          seq_len(nrow(x)),
          function(j) {
            
            if (!is.na(x$Candidate_Episode[j])) {
              return(FALSE)
            }
            
            any(
              sapply(
                current_rows,
                function(k) {
                  
                  x$Smoothing[j] !=
                    x$Smoothing[k] &&
                    x$News_Start_Row[j] <=
                    x$News_End_Row[k] &&
                    x$News_Start_Row[k] <=
                    x$News_End_Row[j]
                  
                }
              )
            )
            
          }
        )
      )
      
      if (length(matched_rows) == 0) {
        break
      }
      
      x$Candidate_Episode[matched_rows] <-
        episode
      
    }
    
  }
  
  x$Candidate_Episode <- LETTERS[
    x$Candidate_Episode
  ]
  
  return(x)
}


candidate_event_results <- do.call(
  rbind,
  lapply(
    split(
      event_results,
      event_results$Measure
    ),
    match_event_candidates
  )
)

rownames(candidate_event_results) <- NULL


# 15. MANUALLY VALIDATED EVENT EPISODES

manual_episode_map <- bind_rows(
  
  data.frame(
    Measure = "GPRD",
    Peak_Date = as.Date(c(
      "2015-07-22",
      "2015-11-23",
      "2015-11-30",
      "2017-06-07",
      "2019-10-02",
      "2021-05-06",
      "2021-05-20",
      "2021-06-01",
      "2023-05-24",
      "2023-10-27",
      "2025-03-13",
      "2025-06-05",
      "2026-03-10",
      "2026-03-17",
      "2026-03-24"
    )),
    Episode = c(
      "A",
      "B",
      "C",
      "D",
      "E",
      "F",
      "G",
      "H",
      "I",
      "J",
      "K",
      "L",
      "M",
      "M",
      "M"
    )
  ),
  
  data.frame(
    Measure = "GPRD_ACT",
    Peak_Date = as.Date(c(
      "2015-01-29",
      "2015-11-23",
      "2015-11-30",
      "2015-12-07",
      "2016-12-13",
      "2017-01-13",
      "2017-06-07",
      "2019-03-08",
      "2020-01-06",
      "2021-05-20",
      "2025-03-06",
      "2025-03-13",
      "2025-06-24",
      "2026-03-17",
      "2026-03-24"
    )),
    Episode = c(
      "A",
      "B",
      "C",
      "C",
      "D",
      "E",
      "F",
      "G",
      "H",
      "I",
      "J",
      "J",
      "K",
      "L",
      "L"
    )
  ),
  
  data.frame(
    Measure = "GPRD_THREAT",
    Peak_Date = as.Date(c(
      "2014-03-17",
      "2014-08-21",
      "2015-11-30",
      "2016-01-12",
      "2017-07-25",
      "2019-10-02",
      "2020-01-09",
      "2021-05-21",
      "2022-03-01",
      "2022-03-08",
      "2022-07-15",
      "2025-06-05",
      "2025-06-20",
      "2026-03-24",
      "2026-03-31"
    )),
    Episode = c(
      "A",
      "B",
      "C",
      "D",
      "E",
      "F",
      "G",
      "H",
      "I",
      "I",
      "J",
      "K",
      "L",
      "M",
      "M"
    )
  )
)


event_results <- event_results %>%
  left_join(
    manual_episode_map,
    by = c(
      "Measure",
      "Peak_Date"
    )
  )


if (any(is.na(event_results$Episode))) {
  
  stop(
    "One or more events do not have a manually validated Episode assignment."
  )
}


# 16. FINAL EVENT TABLE

event_dates <- event_results %>%
  arrange(
    Measure,
    Episode,
    Smoothing,
    Peak_Date
  ) %>%
  select(
    Event_ID,
    Episode,
    Event_Name,
    Measure,
    Smoothing,
    Peak_Date,
    News_Start_Date,
    News_End_Date,
    everything()
  )

event_dates$Event_Name



