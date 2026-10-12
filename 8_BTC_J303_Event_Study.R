# 1. LOAD PACKAGES

library(dplyr)
library(lmtest)
library(sandwich)
library(tseries)


# 2. PREPARE DATA

data <- data0


###############################################################
# Section A: Event Study Analysis
###############################################################


# 3. EVENT STUDY FUNCTION

run_event_study <- function(smoothing_window) {
  
  events <- subset(
    event_dates,
    Smoothing == smoothing_window
  )
  
  
  # Use the fixed event-analysis windows already
  # defined in Script 0.
  
  event_data <- lapply(
    seq_len(nrow(events)),
    function(i) {
      
      data[
        events$News_Start_Row[i]:
          events$News_End_Row[i],
      ]
      
    }
  )
  
  names(event_data) <- events$Event_ID
  
  
  # Event summary statistics
  
  event.statistics <- do.call(
    rbind,
    lapply(
      seq_len(nrow(events)),
      function(i) {
        
        data.frame(
          Event_ID = events$Event_ID[i],
          Event = events$Event_Name[i],
          Event_Codes = events$Event_Codes[i],
          Measure = events$Measure[i],
          Smoothing = events$Smoothing[i],
          Peak_Date = events$Peak_Date[i],
          Sample_Size = nrow(event_data[[i]]),
          Mean_BTC_Volatility =
            mean(
              event_data[[i]]$BTC_Volatility
            ),
          SD_BTC_Volatility =
            sd(
              event_data[[i]]$BTC_Volatility
            ),
          Mean_J303_Volatility =
            mean(
              event_data[[i]]$J303_Volatility
            ),
          SD_J303_Volatility =
            sd(
              event_data[[i]]$J303_Volatility
            )
        )
        
      }
    )
  )
  
  
  event.statistics[-c(1, 2, 3, 4, 5, 6)] <-
    round(
      event.statistics[-c(1, 2, 3, 4, 5, 6)],
      4
    )
  
  
  # Correlation analysis
  
  event.correlations <- do.call(
    rbind,
    lapply(
      seq_len(nrow(events)),
      function(i) {
        
        correlation <- cor.test(
          event_data[[i]]$BTC_Volatility,
          event_data[[i]]$J303_Volatility
        )
        
        data.frame(
          Event_ID = events$Event_ID[i],
          Event = events$Event_Name[i],
          Event_Codes = events$Event_Codes[i],
          Measure = events$Measure[i],
          Smoothing = events$Smoothing[i],
          Peak_Date = events$Peak_Date[i],
          Sample_Size = nrow(event_data[[i]]),
          Correlation =
            unname(correlation$estimate),
          Correlation_P_Value =
            correlation$p.value
        )
        
      }
    )
  )
  
  
  event.correlations$Correlation <-
    round(
      event.correlations$Correlation,
      4
    )
  
  event.correlations$Correlation_P_Value <-
    signif(
      event.correlations$Correlation_P_Value,
      4
    )
  
  event.correlations$Significant <- ifelse(
    event.correlations$Correlation_P_Value < 0.05,
    "Yes",
    "No"
  )
  
  
  # Event-specific regression models
  
  event.models <- lapply(
    seq_len(nrow(events)),
    function(i) {
      
      lm(
        J303_Volatility ~
          BTC_Volatility,
        data = event_data[[i]]
      )
      
    }
  )
  
  names(event.models) <- events$Event_ID
  
  
  # Regression diagnostics
  
  event.diagnostics <- do.call(
    rbind,
    lapply(
      seq_along(event.models),
      function(i) {
        
        model <- event.models[[i]]
        
        data.frame(
          Event_ID = names(event.models)[i],
          Event = events$Event_Name[i],
          Event_Codes = events$Event_Codes[i],
          Measure = events$Measure[i],
          Smoothing = events$Smoothing[i],
          Peak_Date = events$Peak_Date[i],
          Jarque_Bera_P_Value =
            jarque.bera.test(
              residuals(model)
            )$p.value,
          Durbin_Watson_P_Value =
            dwtest(model)$p.value,
          Breusch_Pagan_P_Value =
            bptest(model)$p.value
        )
        
      }
    )
  )
  
  rownames(event.diagnostics) <- NULL
  
  
  # Newey-West robust inference
  
  event.nw <- lapply(
    event.models,
    function(model) {
      
      n_obs <- nobs(model)
      
      # Newey-West lag length
      
      nw_lag <- max(
        0,
        min(
          floor(
            4 * (n_obs / 100)^(2 / 9)
          ),
          n_obs - 1
        )
      )
      
      coeftest(
        model,
        vcov = NeweyWest(
          model,
          lag = nw_lag,
          prewhite = FALSE
        )
      )
      
    }
  )
  
  names(event.nw) <- names(event.models)
  
  
  # Event regression summary
  
  event.regression <- do.call(
    rbind,
    lapply(
      seq_along(event.models),
      function(i) {
        
        model <- event.models[[i]]
        
        data.frame(
          Event_ID = events$Event_ID[i],
          Event = events$Event_Name[i],
          Event_Codes = events$Event_Codes[i],
          Measure = events$Measure[i],
          Smoothing = events$Smoothing[i],
          Peak_Date = events$Peak_Date[i],
          News_Start_Date = events$News_Start_Date[i],
          News_End_Date = events$News_End_Date[i],
          News_Start_Row = events$News_Start_Row[i],
          News_End_Row = events$News_End_Row[i],
          Sample_Size = nrow(event_data[[i]]),
          Correlation =
            event.correlations$Correlation[i],
          BTC_Coefficient =
            unname(coef(model)["BTC_Volatility"]),
          NeweyWest_SE =
            unname(event.nw[[i]][
              "BTC_Volatility",
              "Std. Error"
            ]),
          CI_Lower =
            unname(coef(model)["BTC_Volatility"] -
                     qt(0.975, df = df.residual(model)) *
                     event.nw[[i]][
                       "BTC_Volatility",
                       "Std. Error"
                     ]),
          CI_Upper =
            unname(coef(model)["BTC_Volatility"] +
                     qt(0.975, df = df.residual(model)) *
                     event.nw[[i]][
                       "BTC_Volatility",
                       "Std. Error"
                     ]),
          Adj_R2 =
            summary(model)$adj.r.squared,
          Residual_SE =
            summary(model)$sigma,
          NeweyWest_P_Value =
            event.nw[[i]][
              "BTC_Volatility",
              "Pr(>|t|)"
            ]
        )
        
      }
    )
  )
  
  
  event.regression$Correlation <-
    round(
      event.regression$Correlation,
      4
    )
  
  event.regression$BTC_Coefficient <-
    signif(
      event.regression$BTC_Coefficient,
      4
    )
  
  event.regression$NeweyWest_SE <-
    signif(
      event.regression$NeweyWest_SE,
      4
    )
  
  event.regression$CI_Lower <-
    signif(
      event.regression$CI_Lower,
      4
    )
  
  event.regression$CI_Upper <-
    signif(
      event.regression$CI_Upper,
      4
    )
  
  event.regression$Adj_R2 <-
    round(
      event.regression$Adj_R2,
      4
    )
  
  event.regression$Residual_SE <-
    round(
      event.regression$Residual_SE,
      5
    )
  
  event.regression$NeweyWest_P_Value <-
    signif(
      event.regression$NeweyWest_P_Value,
      4
    )
  
  event.regression$Significant <- ifelse(
    event.regression$NeweyWest_P_Value < 0.05,
    "Yes",
    "No"
  )
  
  rownames(event.regression) <- NULL
  
  
  list(
    events = events,
    event_data = event_data,
    statistics = event.statistics,
    correlations = event.correlations,
    models = event.models,
    diagnostics = event.diagnostics,
    nw = event.nw,
    regression = event.regression
  )
}


# 4. RUN EVENT STUDY FOR ALL SMOOTHING WINDOWS

event.results_11 <- run_event_study(11)

event.results_21 <- run_event_study(21)

event.results_31 <- run_event_study(31)


# 5. 11-DAY RESULTS

event.results_11$events

event.results_11$statistics

event.results_11$correlations

event.results_11$diagnostics

event.results_11$regression


# 6. 21-DAY RESULTS

event.results_21$events

event.results_21$statistics

event.results_21$correlations

event.results_21$diagnostics

event.results_21$regression


# 6a. DISTINCT 21-DAY WINDOWS
# The same exact window can be selected by more than one GPR measure.
# These rows are collapsed by the actual data-row bounds used in the regression.

event.results_21$regression.distinct <-
  event.results_21$regression %>%
  group_by(News_Start_Row, News_End_Row) %>%
  summarise(
    Event_Window = paste(
      as.character(first(News_Start_Date)),
      "to",
      as.character(first(News_End_Date))
    ),
    Event_ID = paste(unique(Event_ID), collapse = "; "),
    Event = paste(unique(Event), collapse = " / "),
    Event_Codes = paste(unique(Event_Codes), collapse = "; "),
    Measure = paste(unique(Measure), collapse = "; "),
    Smoothing = first(Smoothing),
    Peak_Date = paste(unique(as.character(Peak_Date)), collapse = "; "),
    Sample_Size = first(Sample_Size),
    Correlation = first(Correlation),
    BTC_Coefficient = first(BTC_Coefficient),
    NeweyWest_SE = first(NeweyWest_SE),
    CI_Lower = first(CI_Lower),
    CI_Upper = first(CI_Upper),
    Adj_R2 = first(Adj_R2),
    Residual_SE = first(Residual_SE),
    NeweyWest_P_Value = first(NeweyWest_P_Value),
    Significant = first(Significant),
    Number_of_Specifications = n(),
    .groups = "drop"
  ) %>%
  arrange(News_Start_Row, News_End_Row)

event.results_21$regression.distinct


# Count significant distinct 21-day windows by coefficient sign.
# A window is significant only once, even if multiple measures selected it.

distinct.window.sign.summary.21 <- data.frame(
  Specifications = nrow(event.results_21$regression),
  Distinct_Windows = nrow(event.results_21$regression.distinct),
  Significant_Distinct_Windows = sum(
    event.results_21$regression.distinct$NeweyWest_P_Value < 0.05,
    na.rm = TRUE
  ),
  Significant_Negative_Windows = sum(
    event.results_21$regression.distinct$NeweyWest_P_Value < 0.05 &
      event.results_21$regression.distinct$BTC_Coefficient < 0,
    na.rm = TRUE
  ),
  Significant_Positive_Windows = sum(
    event.results_21$regression.distinct$NeweyWest_P_Value < 0.05 &
      event.results_21$regression.distinct$BTC_Coefficient > 0,
    na.rm = TRUE
  )
)

distinct.window.sign.summary.21


# 7. 31-DAY RESULTS

event.results_31$events

event.results_31$statistics

event.results_31$correlations

event.results_31$diagnostics

event.results_31$regression


# 8. EVENT WINDOW COMPARISON

event.window.comparison <- data.frame(
  
  Smoothing = c(11, 21, 31),
  
  Number_of_Events = c(
    nrow(event.results_11$events),
    nrow(event.results_21$events),
    nrow(event.results_31$events)
  ),
  
  Number_of_Distinct_Windows = c(
    nrow(dplyr::distinct(
      event.results_11$events,
      News_Start_Row,
      News_End_Row
    )),
    nrow(dplyr::distinct(
      event.results_21$events,
      News_Start_Row,
      News_End_Row
    )),
    nrow(dplyr::distinct(
      event.results_31$events,
      News_Start_Row,
      News_End_Row
    ))
  ),
  
  Significant_Correlations = c(
    sum(
      event.results_11$correlations$Correlation_P_Value < 0.05
    ),
    sum(
      event.results_21$correlations$Correlation_P_Value < 0.05
    ),
    sum(
      event.results_31$correlations$Correlation_P_Value < 0.05
    )
  ),
  
  Significant_NW_Regressions = c(
    sum(
      event.results_11$regression$NeweyWest_P_Value < 0.05
    ),
    sum(
      event.results_21$regression$NeweyWest_P_Value < 0.05
    ),
    sum(
      event.results_31$regression$NeweyWest_P_Value < 0.05
    )
  )
)

event.window.comparison


# 9. SIGNIFICANCE GRID BY MEASURE AND WINDOW
# These grids count measure/window specifications. They are not counts of
# distinct event windows; use regression.distinct for the 21-day unique-window results.


# Significant correlations

significant.correlation.grid <- data.frame(
  
  Smoothing_11 = c(
    sum(
      event.results_11$correlations$Measure == "GPRD" &
        event.results_11$correlations$Correlation_P_Value < 0.05
    ),
    sum(
      event.results_11$correlations$Measure == "GPRD_ACT" &
        event.results_11$correlations$Correlation_P_Value < 0.05
    ),
    sum(
      event.results_11$correlations$Measure == "GPRD_THREAT" &
        event.results_11$correlations$Correlation_P_Value < 0.05
    )
  ),
  
  Smoothing_21 = c(
    sum(
      event.results_21$correlations$Measure == "GPRD" &
        event.results_21$correlations$Correlation_P_Value < 0.05
    ),
    sum(
      event.results_21$correlations$Measure == "GPRD_ACT" &
        event.results_21$correlations$Correlation_P_Value < 0.05
    ),
    sum(
      event.results_21$correlations$Measure == "GPRD_THREAT" &
        event.results_21$correlations$Correlation_P_Value < 0.05
    )
  ),
  
  Smoothing_31 = c(
    sum(
      event.results_31$correlations$Measure == "GPRD" &
        event.results_31$correlations$Correlation_P_Value < 0.05
    ),
    sum(
      event.results_31$correlations$Measure == "GPRD_ACT" &
        event.results_31$correlations$Correlation_P_Value < 0.05
    ),
    sum(
      event.results_31$correlations$Measure == "GPRD_THREAT" &
        event.results_31$correlations$Correlation_P_Value < 0.05
    )
  )
)

rownames(significant.correlation.grid) <- c(
  "GPRD",
  "GPRD_ACT",
  "GPRD_THREAT"
)

significant.correlation.grid


# Significant Newey-West regressions

significant.regression.grid <- data.frame(
  
  Smoothing_11 = c(
    sum(
      event.results_11$regression$Measure == "GPRD" &
        event.results_11$regression$NeweyWest_P_Value < 0.05
    ),
    sum(
      event.results_11$regression$Measure == "GPRD_ACT" &
        event.results_11$regression$NeweyWest_P_Value < 0.05
    ),
    sum(
      event.results_11$regression$Measure == "GPRD_THREAT" &
        event.results_11$regression$NeweyWest_P_Value < 0.05
    )
  ),
  
  Smoothing_21 = c(
    sum(
      event.results_21$regression$Measure == "GPRD" &
        event.results_21$regression$NeweyWest_P_Value < 0.05
    ),
    sum(
      event.results_21$regression$Measure == "GPRD_ACT" &
        event.results_21$regression$NeweyWest_P_Value < 0.05
    ),
    sum(
      event.results_21$regression$Measure == "GPRD_THREAT" &
        event.results_21$regression$NeweyWest_P_Value < 0.05
    )
  ),
  
  Smoothing_31 = c(
    sum(
      event.results_31$regression$Measure == "GPRD" &
        event.results_31$regression$NeweyWest_P_Value < 0.05
    ),
    sum(
      event.results_31$regression$Measure == "GPRD_ACT" &
        event.results_31$regression$NeweyWest_P_Value < 0.05
    ),
    sum(
      event.results_31$regression$Measure == "GPRD_THREAT" &
        event.results_31$regression$NeweyWest_P_Value < 0.05
    )
  )
)

rownames(significant.regression.grid) <- c(
  "GPRD",
  "GPRD_ACT",
  "GPRD_THREAT"
)

significant.regression.grid


# 10. EVENT IDENTIFICATION ROBUSTNESS

event.identification.robustness <- event_overlap

event.identification.robustness

event.identification.robustness.shared <- event_overlap %>%
  filter(
    Shared_Event_Codes != "None"
  )

event.identification.robustness.shared



