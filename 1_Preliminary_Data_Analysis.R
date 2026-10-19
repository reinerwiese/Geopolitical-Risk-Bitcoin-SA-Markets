# 1. Load Packages
library(ggplot2)
library(moments)
library(tseries)
library(FinTS)

# 2. Data Preparation
data <- na.omit(data0)

btc <- data$BTC_log_returns
J303 <- data$Index_log_returns
GPRD <- data$GPRD
GPRD_ACT <- data$GPRD_ACT
GPRD_THREAT <- data$GPRD_THREAT


# 3. Descriptive Statistics
descriptive_statistics <- function(x){
  
  data.frame(
    
    N = sum(!is.na(x)),
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Minimum = min(x, na.rm = TRUE),
    Maximum = max(x, na.rm = TRUE),
    Skewness = skewness(x, na.rm = TRUE),
    Excess_Kurtosis = kurtosis(x, na.rm = TRUE) - 3
    
  )
  
}

BTC_Stats <- descriptive_statistics(btc)

J303_Stats <- descriptive_statistics(J303)

GPRD_Stats <- descriptive_statistics(GPRD)

GPRD_ACT_Stats <- descriptive_statistics(GPRD_ACT)

GPRD_THREAT_Stats <- descriptive_statistics(GPRD_THREAT)

statistics <- rbind(
  
  Bitcoin = BTC_Stats,
  
  J303 = J303_Stats,
  
  GPRD = GPRD_Stats,
  
  GPRD_ACT = GPRD_ACT_Stats,
  
  GPRD_THREAT = GPRD_THREAT_Stats
  
)

print(round(statistics, 4))


# 4. Time Series Plots

ggplot(data,
       aes(Date, BTC_log_returns)) +
  
  geom_line() +
  
  theme_minimal() +
  
  labs(title = "Bitcoin Daily Log Returns",
       x = "Date",
       y = "Log Returns")


ggplot(data,
       aes(Date, Index_log_returns)) +
  
  geom_line() +
  
  theme_minimal() +
  
  labs(title = "J303 Daily Log Returns",
       x = "Date",
       y = "Log Returns")


ggplot(data,
       aes(Date, GPRD)) +
  
  geom_line() +
  
  theme_minimal() +
  
  labs(title = "Geopolitical Risk Index",
       x = "Date",
       y = "GPRD")


ggplot(data,
       aes(Date, GPRD_ACT)) +
  
  geom_line() +
  
  theme_minimal() +
  
  labs(title = "Geopolitical Risk: Acts",
       x = "Date",
       y = "GPRD_ACT")


ggplot(data,
       aes(Date, GPRD_THREAT)) +
  
  geom_line() +
  
  theme_minimal() +
  
  labs(title = "Geopolitical Risk: Threats",
       x = "Date",
       y = "GPRD_THREAT")


# 5. Return Distributions

ggplot(data,
       aes(BTC_log_returns)) +
  
  geom_histogram(aes(y = after_stat(density)),
                 bins = 40,
                 colour = "black",
                 fill = "grey80") +
  
  geom_density(linewidth = 1) +
  
  theme_minimal() +
  
  labs(title = "Distribution of Bitcoin Returns",
       x = "Log Returns",
       y = "Density")


ggplot(data,
       aes(Index_log_returns)) +
  
  geom_histogram(aes(y = after_stat(density)),
                 bins = 40,
                 colour = "black",
                 fill = "grey80") +
  
  geom_density(linewidth = 1) +
  
  theme_minimal() +
  
  labs(title = "Distribution of J303 Returns",
       x = "Log Returns",
       y = "Density")


# 6. QQ Plots

qqnorm(btc,
       main = "QQ Plot: Bitcoin Returns")

qqline(btc,
       col = "red")


qqnorm(J303,
       main = "QQ Plot: J303 Returns")

qqline(J303,
       col = "red")


# 7. Boxplots

boxplot(btc,
        main = "Bitcoin Returns",
        ylab = "Log Returns")


boxplot(J303,
        main = "J303 Returns",
        ylab = "Log Returns")


# 8. Stationarity Tests

adf_btc <- adf.test(btc)
print(adf_btc)

adf_J303 <- adf.test(J303)
print(adf_J303)


# 9. Normality Tests

jb_btc <- jarque.bera.test(btc)
print(jb_btc)

jb_J303 <- jarque.bera.test(J303)
print(jb_J303)


# 10. Ljung-Box Tests

# Test for autocorrelation in returns

lb_btc_returns <- Box.test(
  btc,
  lag = 20,
  type = "Ljung-Box"
)
print(lb_btc_returns)

lb_J303_returns <- Box.test(
  J303,
  lag = 20,
  type = "Ljung-Box"
)
print(lb_J303_returns)


# Test for autocorrelation in squared returns
# Significant results indicate volatility clustering.

lb_btc_squared <- Box.test(
  btc^2,
  lag = 20,
  type = "Ljung-Box"
)
print(lb_btc_squared)

lb_J303_squared <- Box.test(
  J303^2,
  lag = 20,
  type = "Ljung-Box"
)
print(lb_J303_squared)


# 11. ARCH Test

# Significant ARCH effects indicate time-varying volatility
# and justify the estimation of GARCH-family models.

arch_btc <- ArchTest(
  btc,
  lags = 12
)
print(arch_btc)

arch_J303 <- ArchTest(
  J303,
  lags = 12
)
print(arch_J303)


# 12. Weekday Return Check

weekday_summary <- data.frame(
  Day = weekdays(data$Date),
  BTC = data$BTC_log_returns,
  J303 = data$Index_log_returns
)

weekday_summary$Day <- factor(
  weekday_summary$Day,
  levels = c("Monday",
             "Tuesday",
             "Wednesday",
             "Thursday",
             "Friday")
)

# Bitcoin return standard deviation by weekday

aggregate(BTC ~ Day,
          data = weekday_summary,
          FUN = function(x) sd(x, na.rm = TRUE))

# J303 return standard deviation by weekday

aggregate(J303 ~ Day,
          data = weekday_summary,
          FUN = function(x) sd(x, na.rm = TRUE))


# 13. Preliminary Test Results

preliminary_test_results <- data.frame(
  
  Test = c(
    "Augmented Dickey-Fuller",
    "Augmented Dickey-Fuller",
    "Jarque-Bera",
    "Jarque-Bera",
    "Ljung-Box: Returns",
    "Ljung-Box: Returns",
    "Ljung-Box: Squared Returns",
    "Ljung-Box: Squared Returns",
    "ARCH LM",
    "ARCH LM"
  ),
  
  Series = c(
    "Bitcoin returns",
    "J303 returns",
    "Bitcoin returns",
    "J303 returns",
    "Bitcoin returns",
    "J303 returns",
    "Bitcoin squared returns",
    "J303 squared returns",
    "Bitcoin returns",
    "J303 returns"
  ),
  
  Statistic = unname(c(
    adf_btc$statistic[1],
    adf_J303$statistic[1],
    jb_btc$statistic[1],
    jb_J303$statistic[1],
    lb_btc_returns$statistic[1],
    lb_J303_returns$statistic[1],
    lb_btc_squared$statistic[1],
    lb_J303_squared$statistic[1],
    arch_btc$statistic[1],
    arch_J303$statistic[1]
  )),
  
  Lag_Order = as.numeric(c(
    adf_btc$parameter[1],
    adf_J303$parameter[1],
    NA, NA,
    20, 20, 20, 20, 12, 12
  )),
  
  Degrees_of_Freedom = as.numeric(c(
    NA, NA,
    jb_btc$parameter[1],
    jb_J303$parameter[1],
    lb_btc_returns$parameter[1],
    lb_J303_returns$parameter[1],
    lb_btc_squared$parameter[1],
    lb_J303_squared$parameter[1],
    arch_btc$parameter[1],
    arch_J303$parameter[1]
  )),
  
  P_Value = c(
    adf_btc$p.value,
    adf_J303$p.value,
    jb_btc$p.value,
    jb_J303$p.value,
    lb_btc_returns$p.value,
    lb_J303_returns$p.value,
    lb_btc_squared$p.value,
    lb_J303_squared$p.value,
    arch_btc$p.value,
    arch_J303$p.value
  )
  
)

preliminary_test_results_display <- preliminary_test_results

preliminary_test_results_display$Statistic <- round(
  preliminary_test_results_display$Statistic,
  4
)

preliminary_test_results_display$P_Value <- ifelse(
  preliminary_test_results_display$P_Value < 0.001,
  "<0.001",
  ifelse(
    preliminary_test_results_display$Test ==
      "Augmented Dickey-Fuller" &
      preliminary_test_results_display$P_Value <= 0.01,
    "<0.01",
    formatC(
      preliminary_test_results_display$P_Value,
      format = "g",
      digits = 4
    )
  )
)

print(
  preliminary_test_results_display,
  row.names = FALSE
)