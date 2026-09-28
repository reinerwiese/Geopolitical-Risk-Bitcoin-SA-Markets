# 1. LOAD PACKAGES

library(dplyr)
library(lmtest)
library(sandwich)
library(tseries)
library(ggplot2)


# 2. PREPARE DATA

data <- data0


###############################################################
# Section A: Event Study Analysis
###############################################################

# 3. EVENT STUDY FUNCTION

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
          Mean_BTC_Volatility =
            mean(event_data[[i]]$BTC_Volatility),
          SD_BTC_Volatility =
            sd(event_data[[i]]$BTC_Volatility),
          Mean_J303_Volatility =
            mean(event_data[[i]]$J303_Volatility),
          SD_J303_Volatility =
            sd(event_data[[i]]$J303_Volatility)
        )
        
      }
    )
  )
  
  event.statistics[-c(1, 2)] <-
    round(
      event.statistics[-c(1, 2)],
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
          Event = events$Event[i],
          Measure = events$Measure[i],
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
  
  names(event.models) <- events$Event
  
  
  # Regression diagnostics
  
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
  
  
  # Event regression summary
  
  event.regression <- do.call(
    rbind,
    lapply(
      seq_along(event.models),
      function(i) {
        
        model <- event.models[[i]]
        
        data.frame(
          Event = names(event.models)[i],
          Measure = events$Measure[i],
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


# 9. EVENT IDENTIFICATION STABILITY

event.stability.data <- rbind(
  
  data.frame(
    Event = event.results_11$events$Event,
    Base_Event = sub(
      " [GAT][0-9]+(-[0-9]+)?$",
      "",
      event.results_11$events$Event
    ),
    Measure = event.results_11$events$Measure,
    Smoothing = 11
  ),
  
  data.frame(
    Event = event.results_21$events$Event,
    Base_Event = sub(
      " [GAT][0-9]+(-[0-9]+)?$",
      "",
      event.results_21$events$Event
    ),
    Measure = event.results_21$events$Measure,
    Smoothing = 21
  ),
  
  data.frame(
    Event = event.results_31$events$Event,
    Base_Event = sub(
      " [GAT][0-9]+(-[0-9]+)?$",
      "",
      event.results_31$events$Event
    ),
    Measure = event.results_31$events$Measure,
    Smoothing = 31
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
        Window_11 = ifelse(
          any(x$Smoothing == 11),
          "Yes",
          "No"
        ),
        Window_21 = ifelse(
          any(x$Smoothing == 21),
          "Yes",
          "No"
        ),
        Window_31 = ifelse(
          any(x$Smoothing == 31),
          "Yes",
          "No"
        )
      )
      
    }
  )
)

rownames(event.stability) <- NULL

event.stability


# 10. REGRESSION RESULT STABILITY

regression.stability.data <- rbind(
  
  data.frame(
    Event = event.results_11$regression$Event,
    Base_Event = sub(
      " [GAT][0-9]+(-[0-9]+)?$",
      "",
      event.results_11$regression$Event
    ),
    Measure = event.results_11$regression$Measure,
    Smoothing = 11,
    Significant_NW =
      event.results_11$regression$Significant
  ),
  
  data.frame(
    Event = event.results_21$regression$Event,
    Base_Event = sub(
      " [GAT][0-9]+(-[0-9]+)?$",
      "",
      event.results_21$regression$Event
    ),
    Measure = event.results_21$regression$Measure,
    Smoothing = 21,
    Significant_NW =
      event.results_21$regression$Significant
  ),
  
  data.frame(
    Event = event.results_31$regression$Event,
    Base_Event = sub(
      " [GAT][0-9]+(-[0-9]+)?$",
      "",
      event.results_31$regression$Event
    ),
    Measure = event.results_31$regression$Measure,
    Smoothing = 31,
    Significant_NW =
      event.results_31$regression$Significant
  )
)


regression.stability <- do.call(
  rbind,
  lapply(
    split(
      regression.stability.data,
      list(
        regression.stability.data$Base_Event,
        regression.stability.data$Measure
      ),
      drop = TRUE
    ),
    function(x) {
      
      data.frame(
        Event = x$Base_Event[1],
        Measure = x$Measure[1],
        NW_Significant_11 = ifelse(
          any(
            x$Smoothing == 11 &
              x$Significant_NW == "Yes"
          ),
          "Yes",
          "No"
        ),
        NW_Significant_21 = ifelse(
          any(
            x$Smoothing == 21 &
              x$Significant_NW == "Yes"
          ),
          "Yes",
          "No"
        ),
        NW_Significant_31 = ifelse(
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

rownames(regression.stability) <- NULL

regression.stability


# 11. 21-DAY EVENT REGRESSION COEFFICIENTS

event.coefficient.plot.data <- event.results_21$regression[
  ,
  c(
    "Event",
    "Measure",
    "BTC_Coefficient"
  )
]

event.coefficient.plot.data$Event <- factor(
  event.coefficient.plot.data$Event,
  levels = event.coefficient.plot.data$Event
)

ggplot(
  event.coefficient.plot.data,
  aes(
    x = Event,
    y = BTC_Coefficient,
    fill = Measure
  )
) +
  geom_col() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  scale_x_discrete(
    labels = function(x) {
      sub(
        " [GAT][0-9]+(-[0-9]+)?$",
        "",
        x
      )
    }
  ) +
  labs(
    title = "BTC–J303 Volatility Relationship During Identified Events",
    x = "Event",
    y = "BTC Volatility Coefficient",
    fill = "GPR Measure"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )
