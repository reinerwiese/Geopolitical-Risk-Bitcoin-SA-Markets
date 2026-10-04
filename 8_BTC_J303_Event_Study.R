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
          Episode = events$Episode[i],
          Event = events$Event_Name[i],
          Measure = events$Measure[i],
          Peak_Date = events$Peak_Date[i],
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
  
  event.statistics[-c(1, 2, 3, 4, 5)] <-
    round(
      event.statistics[-c(1, 2, 3, 4, 5)],
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
          Episode = events$Episode[i],
          Event = events$Event_Name[i],
          Measure = events$Measure[i],
          Peak_Date = events$Peak_Date[i],
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
          Episode = events$Episode[i],
          Event = events$Event_Name[i],
          Measure = events$Measure[i],
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
          Event_ID = events$Event_ID[i],
          Episode = events$Episode[i],
          Event = events$Event_Name[i],
          Measure = events$Measure[i],
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


# 9. EVENT IDENTIFICATION STABILITY

event.stability.data <- rbind(
  
  data.frame(
    Event_ID = event.results_11$events$Event_ID,
    Episode = event.results_11$events$Episode,
    Event = event.results_11$events$Event_Name,
    Measure = event.results_11$events$Measure,
    Smoothing = 11,
    Peak_Date = event.results_11$events$Peak_Date,
    Peak_Row = event.results_11$events$Peak_Row
  ),
  
  data.frame(
    Event_ID = event.results_21$events$Event_ID,
    Episode = event.results_21$events$Episode,
    Event = event.results_21$events$Event_Name,
    Measure = event.results_21$events$Measure,
    Smoothing = 21,
    Peak_Date = event.results_21$events$Peak_Date,
    Peak_Row = event.results_21$events$Peak_Row
  ),
  
  data.frame(
    Event_ID = event.results_31$events$Event_ID,
    Episode = event.results_31$events$Episode,
    Event = event.results_31$events$Event_Name,
    Measure = event.results_31$events$Measure,
    Smoothing = 31,
    Peak_Date = event.results_31$events$Peak_Date,
    Peak_Row = event.results_31$events$Peak_Row
  )
)


event.stability <- do.call(
  rbind,
  lapply(
    split(
      event.stability.data,
      list(
        event.stability.data$Measure,
        event.stability.data$Episode
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
        Episode = representative$Episode,
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
    Event_ID = event.results_11$regression$Event_ID,
    Episode = event.results_11$regression$Episode,
    Event = event.results_11$regression$Event,
    Measure = event.results_11$regression$Measure,
    Smoothing = 11,
    Peak_Date = event.results_11$regression$Peak_Date,
    Peak_Row = event.results_11$events$Peak_Row,
    Significant_NW =
      event.results_11$regression$Significant
  ),
  
  data.frame(
    Event_ID = event.results_21$regression$Event_ID,
    Episode = event.results_21$regression$Episode,
    Event = event.results_21$regression$Event,
    Measure = event.results_21$regression$Measure,
    Smoothing = 21,
    Peak_Date = event.results_21$regression$Peak_Date,
    Peak_Row = event.results_21$events$Peak_Row,
    Significant_NW =
      event.results_21$regression$Significant
  ),
  
  data.frame(
    Event_ID = event.results_31$regression$Event_ID,
    Episode = event.results_31$regression$Episode,
    Event = event.results_31$regression$Event,
    Measure = event.results_31$regression$Measure,
    Smoothing = 31,
    Peak_Date = event.results_31$regression$Peak_Date,
    Peak_Row = event.results_31$events$Peak_Row,
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
        regression.stability.data$Measure,
        regression.stability.data$Episode
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
        Episode = representative$Episode,
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

rownames(regression.stability) <- NULL

regression.stability


# 11. 21-DAY EVENT REGRESSION COEFFICIENTS

event.coefficient.plot.data <- event.results_21$regression[
  ,
  c(
    "Event_ID",
    "Episode",
    "Event",
    "Measure",
    "BTC_Coefficient"
  )
]

event.coefficient.plot.data$Event_ID <- factor(
  event.coefficient.plot.data$Event_ID,
  levels = event.coefficient.plot.data$Event_ID
)

ggplot(
  event.coefficient.plot.data,
  aes(
    x = Event_ID,
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
    labels = setNames(
      event.coefficient.plot.data$Event,
      event.coefficient.plot.data$Event_ID
    )
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
