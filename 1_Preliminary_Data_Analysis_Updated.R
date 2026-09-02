  # 1. Load Packages
  library(readxl)
  library(ggplot2)
  library(moments)
  library(tseries)
  library(FinTS)
  
  
  # 2. Import Data
  data <- read_excel(
    "Thesis_Data.xlsx",
    sheet = "Data",
    na = "NA"
  )
  
  
  # 3. Data Preparation
  data <- na.omit(data)
  btc <- data$BTC_log_returns
  J303 <- data$Index_log_returns
  
  
  # 4. Descriptive Statistics
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
  
  statistics <- rbind(
    
    Bitcoin = BTC_Stats,
    
    J303 = J303_Stats
    
  )
  
  print(round(statistics, 4))
  
  
  # 5. Time Series Plots
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
         y = "GPR")
  
  # 6. Return Distributions
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
  
  
  # 7. QQ Plots
  qqnorm(btc,
         main = "QQ Plot: Bitcoin Returns")
  
  qqline(btc,
         col = "red")
  
  
  qqnorm(J303,
         main = "QQ Plot: J303 Returns")
  
  qqline(J303,
         col = "red")
  
  
  # 8. Boxplots
  boxplot(btc,
          main = "Bitcoin Returns",
          ylab = "Log Returns")
  
  
  boxplot(J303,
          main = "J303 Returns",
          ylab = "Log Returns")
  
  
  # 9. Stationarity Tests
  adf.test(btc)
  
  adf.test(J303)
  
  
  # 10. Normality Tests
  jarque.bera.test(btc)
  
  jarque.bera.test(J303)
  
  
  # 11. Ljung-Box Tests
  # Test for autocorrelation in returns
  
  Box.test(btc,
           lag = 20,
           type = "Ljung-Box")
  
  Box.test(J303,
           lag = 20,
           type = "Ljung-Box")
  
  
  # Test for autocorrelation in squared returns
  # Significant results indicate volatility clustering.
  
  Box.test(btc^2,
           lag = 20,
           type = "Ljung-Box")
  
  Box.test(J303^2,
           lag = 20,
           type = "Ljung-Box")
  
  
  # 12. ARCH Test
  # Significant ARCH effects indicate time-varying volatility
  # and justify the estimation of GARCH-family models.
  
  ArchTest(btc, lags = 12)
  
  ArchTest(J303, lags = 12)

