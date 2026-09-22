# 1. Load Packages

library(readxl)
library(rugarch)
library(FinTS)


# 2. Import Data

data <- na.omit(data0)

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


# 4. Specify Candidate Volatility Models

# Standard GARCH(1,1)

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


# EGARCH(1,1)

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


# GJR-GARCH(1,1)

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


# 5. Estimate Candidate Volatility Models

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


# 6. Compare Candidate Volatility Models

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


# 7. Test Alternative EGARCH Mean Specifications

# EGARCH with ARMA(1,0)

spec.egarch.ar10 <- ugarchspec(
  
  variance.model = list(
    model = "eGARCH",
    garchOrder = c(1,1)
  ),
  
  mean.model = list(
    armaOrder = c(1,0),
    include.mean = TRUE
  ),
  
  distribution.model = "std"
  
)


# EGARCH with ARMA(0,1)

spec.egarch.ma01 <- ugarchspec(
  
  variance.model = list(
    model = "eGARCH",
    garchOrder = c(1,1)
  ),
  
  mean.model = list(
    armaOrder = c(0,1),
    include.mean = TRUE
  ),
  
  distribution.model = "std"
  
)


# EGARCH with ARMA(1,1)

spec.egarch.ar11 <- ugarchspec(
  
  variance.model = list(
    model = "eGARCH",
    garchOrder = c(1,1)
  ),
  
  mean.model = list(
    armaOrder = c(1,1),
    include.mean = TRUE
  ),
  
  distribution.model = "std"
  
)


# Estimate alternative mean specifications

fit.egarch.ar10 <- ugarchfit(
  spec = spec.egarch.ar10,
  data = btc
)

fit.egarch.ma01 <- ugarchfit(
  spec = spec.egarch.ma01,
  data = btc
)

fit.egarch.ar11 <- ugarchfit(
  spec = spec.egarch.ar11,
  data = btc
)


# 8. Compare EGARCH Mean Specifications

mean_comparison <- data.frame(
  
  Model = c(
    "EGARCH ARMA(0,0)",
    "EGARCH ARMA(1,0)",
    "EGARCH ARMA(0,1)",
    "EGARCH ARMA(1,1)"
  ),
  
  LogLikelihood = c(
    likelihood(fit.egarch),
    likelihood(fit.egarch.ar10),
    likelihood(fit.egarch.ma01),
    likelihood(fit.egarch.ar11)
  ),
  
  AIC = c(
    infocriteria(fit.egarch)[1],
    infocriteria(fit.egarch.ar10)[1],
    infocriteria(fit.egarch.ma01)[1],
    infocriteria(fit.egarch.ar11)[1]
  ),
  
  BIC = c(
    infocriteria(fit.egarch)[2],
    infocriteria(fit.egarch.ar10)[2],
    infocriteria(fit.egarch.ma01)[2],
    infocriteria(fit.egarch.ar11)[2]
  ),
  
  Shibata = c(
    infocriteria(fit.egarch)[3],
    infocriteria(fit.egarch.ar10)[3],
    infocriteria(fit.egarch.ma01)[3],
    infocriteria(fit.egarch.ar11)[3]
  ),
  
  HannanQuinn = c(
    infocriteria(fit.egarch)[4],
    infocriteria(fit.egarch.ar10)[4],
    infocriteria(fit.egarch.ma01)[4],
    infocriteria(fit.egarch.ar11)[4]
  )
  
)

mean_comparison


# 9. Residual Autocorrelation for Alternative Mean Specifications

# EGARCH ARMA(0,0)

Box.test(
  residuals(fit.egarch, standardize = TRUE),
  lag = 20,
  type = "Ljung-Box"
)


# EGARCH ARMA(1,0)

Box.test(
  residuals(fit.egarch.ar10, standardize = TRUE),
  lag = 20,
  type = "Ljung-Box"
)


# EGARCH ARMA(0,1)

Box.test(
  residuals(fit.egarch.ma01, standardize = TRUE),
  lag = 20,
  type = "Ljung-Box"
)


# EGARCH ARMA(1,1)

Box.test(
  residuals(fit.egarch.ar11, standardize = TRUE),
  lag = 20,
  type = "Ljung-Box"
)


# 10. Display Alternative EGARCH Models

show(fit.egarch.ar10)

show(fit.egarch.ma01)

show(fit.egarch.ar11)


# 11. Variance Persistence

persistence(fit.garch)

persistence(fit.egarch)

persistence(fit.gjr)


# 12. Nyblom Parameter Stability Test

nyblom(fit.egarch)


# 13. Extract Conditional Volatility

# EGARCH ARMA(0,0) is used as the reference model.

volatility <- sigma(fit.egarch)

std.residuals <- residuals(
  fit.egarch,
  standardize = TRUE
)

data$BTC_Volatility <- as.numeric(volatility)


# 14. Conditional Volatility Plot

plot(data$Date,
     volatility,
     type = "l",
     main = "Estimated Bitcoin Conditional Volatility",
     xlab = "Date",
     ylab = "Conditional Volatility")


# 15. Standardized Residuals

plot(data$Date,
     std.residuals,
     type = "l",
     main = "Standardized Residuals",
     xlab = "Date",
     ylab = "Standardized Residual")


# 16. Residual Distribution

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


# 17. QQ Plot of Standardized Residuals

qqnorm(
  std.residuals,
  main = "QQ Plot of Standardized Residuals"
)

qqline(
  std.residuals,
  col = "red"
)


# 18. Residual Diagnostics

# Test for autocorrelation in standardized residuals

Box.test(
  std.residuals,
  lag = 20,
  type = "Ljung-Box"
)


# Test for autocorrelation in squared standardized residuals

Box.test(
  std.residuals^2,
  lag = 20,
  type = "Ljung-Box"
)


# Test for remaining ARCH effects

ArchTest(
  std.residuals,
  lags = 12
)


# 19. Weekday Standard Deviation of Standardized Residuals

weekday_residuals <- data.frame(
  Day = weekdays(data$Date),
  Standardized_Residual = as.numeric(std.residuals)
)

weekday_residuals$Day <- factor(
  weekday_residuals$Day,
  levels = c(
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday"
  )
)


# Standard deviation by weekday

aggregate(
  Standardized_Residual ~ Day,
  data = weekday_residuals,
  FUN = function(x) sd(x, na.rm = TRUE)
)


# 20. Formal Test of Weekday Variance Differences

# Fligner-Killeen test for equality of residual variances
# across weekdays

fligner.test(
  Standardized_Residual ~ Day,
  data = weekday_residuals
)