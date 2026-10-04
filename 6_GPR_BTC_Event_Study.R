#1 Load required packages

library(dplyr)
library(ggplot2)

library(FinTS)
library(tseries)
library(moments)

library(lmtest)
library(sandwich)
library(car)

library(zoo)


#2 Load data

data <- data0


###############################################################
# Section A: Event Study Analysis
###############################################################

#3 Event study function

run_event_study <- function(smoothing_window) {
  
  events <- subset(
    event_dates,
    Smoothing == smoothing_window
  )
  
  events$Peak_Row <- match(
    events$Peak_Date,
    data$Date
  )
  
  events$Window_Start_Row <- pmax(
    1,
    events$Peak_Row - floor(smoothing_window / 2)
  )
  
  events$Window_End_Row <- pmin(
    nrow(data),
    events$Peak_Row + floor(smoothing_window / 2)
  )
  
  events$Window_Start_Date <- data$Date[
    events$Window_Start_Row
  ]
  
  events$Window_End_Date <- data$Date[
    events$Window_End_Row
  ]
  
  
  # Create event datasets
  
  event_data <- lapply(
    seq_len(nrow(events)),
    function(i) {
      
      data[
        events$Window_Start_Row[i]:
          events$Window_End_Row[i],
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
          Measure = events$Measure[i],
          Peak_Date = events$Peak_Date[i],
          Sample_Size = nrow(event_data[[i]]),
          Mean_GPR =
            mean(event_data[[i]][[events$Measure[i]]]),
          SD_GPR =
            sd(event_data[[i]][[events$Measure[i]]]),
          Mean_BTC_Volatility =
            mean(event_data[[i]]$BTC_Volatility),
          SD_BTC_Volatility =
            sd(event_data[[i]]$BTC_Volatility)
        )
        
      }
    )
  )
  
  event.statistics[-c(1, 2, 3, 4)] <-
    round(event.statistics[-c(1, 2, 3, 4)], 4)
  
  
  # Correlation analysis
  
  event.correlations <- do.call(
    rbind,
    lapply(
      seq_len(nrow(events)),
      function(i) {
        
        correlation <- cor.test(
          event_data[[i]]$BTC_Volatility,
          event_data[[i]][[events$Measure[i]]]
        )
        
        data.frame(
          Event_ID = events$Event_ID[i],
          Event = events$Event_Name[i],
          Measure = events$Measure[i],
          Peak_Date = events$Peak_Date[i],
          Correlation = unname(correlation$estimate),
          Correlation_P_Value = correlation$p.value
        )
        
      }
    )
  )
  
  event.correlations$Correlation <-
    round(event.correlations$Correlation, 4)
  
  event.correlations$Correlation_P_Value <-
    signif(event.correlations$Correlation_P_Value, 4)
  
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
        as.formula(
          paste(
            "BTC_Volatility ~",
            events$Measure[i]
          )
        ),
        data = event_data[[i]]
      )
      
    }
  )
  
  names(event.models) <- events$Event_ID
  
  
  # Regression diagnostic tests
  
  event.diagnostics <- do.call(
    rbind,
    lapply(
      seq_along(event.models),
      function(i) {
        
        model <- event.models[[i]]
        
        data.frame(
          Event_ID = names(event.models)[i],
          Event = events$Event_Name[i],
          Measure = events$Measure[i],
          Peak_Date = events$Peak_Date[i],
          Jarque_Bera_P_Value =
            jarque.bera.test(
              residuals(model)
            )$p.value,
          Breusch_Pagan_P_Value =
            bptest(model)$p.value,
          Durbin_Watson_P_Value =
            dwtest(model)$p.value
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
          floor(4 * (n_obs / 100)^(2/9)),
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
  
  
  # Event regression comparison
  
  event.regression <- do.call(
    rbind,
    lapply(
      seq_along(event.models),
      function(i) {
        
        model <- event.models[[i]]
        
        data.frame(
          Event_ID = events$Event_ID[i],
          Event = events$Event_Name[i],
          Measure = events$Measure[i],
          Peak_Date = events$Peak_Date[i],
          Sample_Size = nrow(event_data[[i]]),
          Correlation = event.correlations$Correlation[i],
          GPR_Coefficient = coef(model)[2],
          Adj_R2 = summary(model)$adj.r.squared,
          Residual_SE = summary(model)$sigma,
          NeweyWest_P_Value =
            event.nw[[i]][2, "Pr(>|t|)"]
        )
        
      }
    )
  )
  
  event.regression$Correlation <-
    round(event.regression$Correlation, 4)
  
  event.regression$GPR_Coefficient <-
    signif(event.regression$GPR_Coefficient, 4)
  
  event.regression$Adj_R2 <-
    round(event.regression$Adj_R2, 4)
  
  event.regression$Residual_SE <-
    round(event.regression$Residual_SE, 5)
  
  event.regression$NeweyWest_P_Value <-
    signif(event.regression$NeweyWest_P_Value, 4)
  
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


#4 Run event study for all smoothing windows

event.results_11 <- run_event_study(11)

event.results_21 <- run_event_study(21)

event.results_31 <- run_event_study(31)


#5 11-day results

event.results_11$events

event.results_11$statistics

event.results_11$correlations

event.results_11$diagnostics

event.results_11$regression


#6 21-day results

event.results_21$events

event.results_21$statistics

event.results_21$correlations

event.results_21$diagnostics

event.results_21$regression


#7 31-day results

event.results_31$events

event.results_31$statistics

event.results_31$correlations

event.results_31$diagnostics

event.results_31$regression


#8 Event window comparison

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


#9 Event identification stability

event.stability.data <- rbind(
  
  data.frame(
    Event_ID = event.results_11$events$Event_ID,
    Event = event.results_11$events$Event_Name,
    Measure = event.results_11$events$Measure,
    Smoothing = 11,
    Peak_Date = event.results_11$events$Peak_Date,
    Peak_Row = event.results_11$events$Peak_Row,
    Significant_NW =
      event.results_11$regression$Significant
  ),
  
  data.frame(
    Event_ID = event.results_21$events$Event_ID,
    Event = event.results_21$events$Event_Name,
    Measure = event.results_21$events$Measure,
    Smoothing = 21,
    Peak_Date = event.results_21$events$Peak_Date,
    Peak_Row = event.results_21$events$Peak_Row,
    Significant_NW =
      event.results_21$regression$Significant
  ),
  
  data.frame(
    Event_ID = event.results_31$events$Event_ID,
    Event = event.results_31$events$Event_Name,
    Measure = event.results_31$events$Measure,
    Smoothing = 31,
    Peak_Date = event.results_31$events$Peak_Date,
    Peak_Row = event.results_31$events$Peak_Row,
    Significant_NW =
      event.results_31$regression$Significant
  )
)


match_event_peaks <- function(x) {
  
  x <- x[order(x$Peak_Row), ]
  
  x$Episode_ID <- NA_integer_
  
  episode_id <- 0
  
  for (i in seq_len(nrow(x))) {
    
    if (!is.na(x$Episode_ID[i])) next
    
    episode_id <- episode_id + 1
    
    x$Episode_ID[i] <- episode_id
    
    repeat {
      
      current_rows <- which(
        x$Episode_ID == episode_id
      )
      
      matched_rows <- which(
        sapply(
          seq_len(nrow(x)),
          function(j) {
            
            if (!is.na(x$Episode_ID[j])) {
              return(FALSE)
            }
            
            any(
              sapply(
                current_rows,
                function(k) {
                  
                  x$Smoothing[j] != x$Smoothing[k] &&
                    abs(
                      x$Peak_Row[j] -
                        x$Peak_Row[k]
                    ) <=
                    floor(
                      max(
                        x$Smoothing[j],
                        x$Smoothing[k]
                      ) / 2
                    )
                  
                }
              )
            )
            
          }
        )
      )
      
      if (length(matched_rows) == 0) break
      
      x$Episode_ID[matched_rows] <- episode_id
      
    }
    
  }
  
  return(x)
}


event.stability.data <- do.call(
  rbind,
  lapply(
    split(
      event.stability.data,
      event.stability.data$Measure
    ),
    match_event_peaks
  )
)

rownames(event.stability.data) <- NULL


event.stability <- do.call(
  rbind,
  lapply(
    split(
      event.stability.data,
      list(
        event.stability.data$Measure,
        event.stability.data$Episode_ID
      ),
      drop = TRUE
    ),
    function(x) {
      
      representative <- if (
        any(x$Smoothing == 21)
      ) {
        x[x$Smoothing == 21, ][1, ]
      } else if (
        any(x$Smoothing == 11)
      ) {
        x[x$Smoothing == 11, ][1, ]
      } else {
        x[x$Smoothing == 31, ][1, ]
      }
      
      data.frame(
        Event = representative$Event,
        Measure = representative$Measure,
        
        Peak_Date_11 =
          ifelse(
            any(x$Smoothing == 11),
            as.character(
              x$Peak_Date[x$Smoothing == 11][1]
            ),
            NA
          ),
        
        Peak_Date_21 =
          ifelse(
            any(x$Smoothing == 21),
            as.character(
              x$Peak_Date[x$Smoothing == 21][1]
            ),
            NA
          ),
        
        Peak_Date_31 =
          ifelse(
            any(x$Smoothing == 31),
            as.character(
              x$Peak_Date[x$Smoothing == 31][1]
            ),
            NA
          ),
        
        Window_11 =
          ifelse(
            any(x$Smoothing == 11),
            "Yes",
            "No"
          ),
        
        Window_21 =
          ifelse(
            any(x$Smoothing == 21),
            "Yes",
            "No"
          ),
        
        Window_31 =
          ifelse(
            any(x$Smoothing == 31),
            "Yes",
            "No"
          ),
        
        NW_Significant_11 =
          ifelse(
            any(
              x$Smoothing == 11 &
                x$Significant_NW == "Yes"
            ),
            "Yes",
            "No"
          ),
        
        NW_Significant_21 =
          ifelse(
            any(
              x$Smoothing == 21 &
                x$Significant_NW == "Yes"
            ),
            "Yes",
            "No"
          ),
        
        NW_Significant_31 =
          ifelse(
            any(
              x$Smoothing == 31 &
                x$Significant_NW == "Yes"
            ),
            "Yes",
            "No"
          )
      )
      
    }
  )
)

rownames(event.stability) <- NULL

event.stability



