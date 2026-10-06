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
          Sample_Size = nrow(event_data[[i]]),
          Correlation =
            event.correlations$Correlation[i],
          BTC_Coefficient =
            coef(model)["BTC_Volatility"],
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


# 9. EVENT IDENTIFICATION ROBUSTNESS
# E-CODE OVERLAP ACROSS MEASURE-SMOOTHING COMBINATIONS

event.identification.robustness <- event_overlap

event.identification.robustness


# Optional compact view showing only comparisons
# where at least one underlying event is shared.

event.identification.robustness.shared <- event_overlap %>%
  filter(
    Shared_Event_Codes != "None"
  )

event.identification.robustness.shared
