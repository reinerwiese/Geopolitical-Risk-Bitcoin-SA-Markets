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
    event_matches,
    Smoothing == smoothing_window
  )
  
  
  # Create event datasets
  
  event_data <- lapply(
    seq_len(nrow(events)),
    function(i) {
      
      event <- events[i, ]
      
      subset(
        data,
        Date >= event$Start_Date &
          Date <= event$End_Date
      )
      
    }
  )
  
  names(event_data) <- events$Event
  
  
  # Event summary statistics
  
  event.statistics <- do.call(
    rbind,
    lapply(
      seq_len(nrow(events)),
      function(i) {
        
        data.frame(
          Event = events$Event[i],
          Measure = events$Measure[i],
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
  
  event.statistics[-c(1, 2)] <-
    round(event.statistics[-c(1, 2)], 4)
  
  
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
          Event = events$Event[i],
          Measure = events$Measure[i],
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
  
  names(event.models) <- events$Event
  
  
  # Regression diagnostic tests
  
  event.diagnostics <- do.call(
    rbind,
    lapply(
      seq_along(event.models),
      function(i) {
        
        model <- event.models[[i]]
        
        data.frame(
          Event = names(event.models)[i],
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
      
      coeftest(
        model,
        vcov = NeweyWest(
          model,
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
          Event = names(event.models)[i],
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
    Event = event.results_11$events$Event,
    Base_Event = sub(
      " [GAT][0-9]+(-[0-9]+)?$",
      "",
      event.results_11$events$Event
    ),
    Measure = event.results_11$events$Measure,
    Smoothing = 11,
    Significant_NW =
      event.results_11$regression$Significant
  ),
  
  data.frame(
    Event = event.results_21$events$Event,
    Base_Event = sub(
      " [GAT][0-9]+(-[0-9]+)?$",
      "",
      event.results_21$events$Event
    ),
    Measure = event.results_21$events$Measure,
    Smoothing = 21,
    Significant_NW =
      event.results_21$regression$Significant
  ),
  
  data.frame(
    Event = event.results_31$events$Event,
    Base_Event = sub(
      " [GAT][0-9]+(-[0-9]+)?$",
      "",
      event.results_31$events$Event
    ),
    Measure = event.results_31$events$Measure,
    Smoothing = 31,
    Significant_NW =
      event.results_31$regression$Significant
  )
)


event.stability <- do.call(
  rbind,
  lapply(
    split(
      event.stability.data,
      list(
        event.stability.data$Base_Event,
        event.stability.data$Measure
      ),
      drop = TRUE
    ),
    function(x) {
      
      data.frame(
        Event = x$Base_Event[1],
        Measure = x$Measure[1],
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

