# 1. Load Packages
library(readxl)
library(rugarch)
library(FinTS)


# 2. Import Data
data <- read_excel(
  "Thesis_Data.xlsx",
  sheet = "Data",
  na="NA"
)

data <- na.omit(data)

btc <- data$BTC_log_returns


# 3. Visual Diagnostics

acf(btc,
    main = "ACF of Bitcoin Returns")

pacf(btc,
     main = "PACF of Bitcoin Returns")

acf(btc^2,
    main = "ACF of Squared Bitcoin Returns")

pacf(btc^2,
     main = "PACF of Squared Bitcoin Returns")


# 4. Specify Candidate Models
spec.garch <- ugarchspec(
  
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1,1)
  ),
  
  mean.model = list(
    armaOrder = c(0,0),
    include.mean = TRUE
  ),
  
  distribution.model = "std"
  
)


spec.egarch <- ugarchspec(
  
  variance.model = list(
    model = "eGARCH",
    garchOrder = c(1,1)
  ),
  
  mean.model = list(
    armaOrder = c(0,0),
    include.mean = TRUE
  ),
  
  distribution.model = "std"
  
)


spec.gjr <- ugarchspec(
  
  variance.model = list(
    model = "gjrGARCH",
    garchOrder = c(1,1)
  ),
  
  mean.model = list(
    armaOrder = c(0,0),
    include.mean = TRUE
  ),
  
  distribution.model = "std"
  
)


# 5. Estimate Models
fit.garch <- ugarchfit(
  spec = spec.garch,
  data = btc
)

fit.egarch <- ugarchfit(
  spec = spec.egarch,
  data = btc
)

fit.gjr <- ugarchfit(
  spec = spec.gjr,
  data = btc
)


# 6. Compare Models
comparison <- data.frame(
  
  Model = c(
    "GARCH(1,1)",
    "EGARCH(1,1)",
    "GJR-GARCH(1,1)"
  ),
  
  LogLikelihood = c(
    likelihood(fit.garch),
    likelihood(fit.egarch),
    likelihood(fit.gjr)
  ),
  
  AIC = c(
    infocriteria(fit.garch)[1],
    infocriteria(fit.egarch)[1],
    infocriteria(fit.gjr)[1]
  ),
  
  BIC = c(
    infocriteria(fit.garch)[2],
    infocriteria(fit.egarch)[2],
    infocriteria(fit.gjr)[2]
  ),
  
  Shibata = c(
    infocriteria(fit.garch)[3],
    infocriteria(fit.egarch)[3],
    infocriteria(fit.gjr)[3]
  ),
  
  HannanQuinn = c(
    infocriteria(fit.garch)[4],
    infocriteria(fit.egarch)[4],
    infocriteria(fit.gjr)[4]
  )
  
)

comparison


# 7. Model Summary

show(fit.egarch)


# 8. Extract Results

volatility <- sigma(fit.egarch)

std.residuals <- residuals(
  fit.egarch,
  standardize = TRUE
)

data$BTC_Volatility <- as.numeric(volatility)


# 9. Conditional Volatility

plot(data$Date,
     volatility,
     type = "l",
     main = "Estimated Bitcoin Conditional Volatility",
     xlab = "Date",
     ylab = "Conditional Volatility")


# 10. Standardized Residuals

plot(data$Date,
     std.residuals,
     type = "l",
     main = "Standardized Residuals",
     xlab = "Date",
     ylab = "Standardized Residual")



# 11. Residual Distribution
hist(
  std.residuals,
  breaks = 40,
  probability = TRUE,
  main = "Distribution of Standardized Residuals",
  xlab = "Standardized Residuals",
  ylim = c(0, 0.7)
)

lines(
  density(std.residuals),
  lwd = 2
)

qqnorm(std.residuals,
       main = "QQ Plot of Standardized Residuals")

qqline(std.residuals,
       col = "red")


# 12. Residual Diagnostics
Box.test(
  std.residuals,
  lag = 20,
  type = "Ljung-Box"
)

Box.test(
  std.residuals^2,
  lag = 20,
  type = "Ljung-Box"
)

ArchTest(
  std.residuals,
  lags = 12
)



