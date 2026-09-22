# 1. Load Packages

library(readxl)
library(rugarch)
library(FinTS)


# 2. Import Data

data <- na.omit(data0)

J303 <- data$Index_log_returns


# 3. Visual Diagnostics

acf(
  J303,
  main = "ACF of J303 Returns"
)

pacf(
  J303,
  main = "PACF of J303 Returns"
)

acf(
  J303^2,
  main = "ACF of Squared J303 Returns"
)

pacf(
  J303^2,
  main = "PACF of Squared J303 Returns"
)


# 4. Specify Candidate Volatility Models

# Standard GARCH(1,1)

spec.garch <- ugarchspec(
  
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1, 1)
  ),
  
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  
  distribution.model = "std"
)


# EGARCH(1,1)

spec.egarch <- ugarchspec(
  
  variance.model = list(
    model = "eGARCH",
    garchOrder = c(1, 1)
  ),
  
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  
  distribution.model = "std"
)


# GJR-GARCH(1,1)

spec.gjr <- ugarchspec(
  
  variance.model = list(
    model = "gjrGARCH",
    garchOrder = c(1, 1)
  ),
  
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  
  distribution.model = "std"
)


# 5. Estimate Candidate Volatility Models

fit.garch <- ugarchfit(
  spec = spec.garch,
  data = J303
)

fit.egarch <- ugarchfit(
  spec = spec.egarch,
  data = J303
)

fit.gjr <- ugarchfit(
  spec = spec.gjr,
  data = J303
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


# 7. Compare Mean Specifications for EGARCH

# EGARCH with ARMA(1,0)

spec.egarch10 <- ugarchspec(
  
  variance.model = list(
    model = "eGARCH",
    garchOrder = c(1, 1)
  ),
  
  mean.model = list(
    armaOrder = c(1, 0),
    include.mean = TRUE
  ),
  
  distribution.model = "std"
)


# EGARCH with ARMA(0,1)

spec.egarch01 <- ugarchspec(
  
  variance.model = list(
    model = "eGARCH",
    garchOrder = c(1, 1)
  ),
  
  mean.model = list(
    armaOrder = c(0, 1),
    include.mean = TRUE
  ),
  
  distribution.model = "std"
)


# EGARCH with ARMA(1,1)

spec.egarch11 <- ugarchspec(
  
  variance.model = list(
    model = "eGARCH",
    garchOrder = c(1, 1)
  ),
  
  mean.model = list(
    armaOrder = c(1, 1),
    include.mean = TRUE
  ),
  
  distribution.model = "std"
)


# Estimate alternative mean specifications

fit.egarch10 <- ugarchfit(
  spec = spec.egarch10,
  data = J303
)

fit.egarch01 <- ugarchfit(
  spec = spec.egarch01,
  data = J303
)

fit.egarch11 <- ugarchfit(
  spec = spec.egarch11,
  data = J303
)


# 8. Compare EGARCH Mean Specifications

arma.comparison <- data.frame(
  
  Model = c(
    "ARMA(0,0)-EGARCH",
    "ARMA(1,0)-EGARCH",
    "ARMA(0,1)-EGARCH",
    "ARMA(1,1)-EGARCH"
  ),
  
  LogLikelihood = c(
    likelihood(fit.egarch),
    likelihood(fit.egarch10),
    likelihood(fit.egarch01),
    likelihood(fit.egarch11)
  ),
  
  AIC = c(
    infocriteria(fit.egarch)[1],
    infocriteria(fit.egarch10)[1],
    infocriteria(fit.egarch01)[1],
    infocriteria(fit.egarch11)[1]
  ),
  
  BIC = c(
    infocriteria(fit.egarch)[2],
    infocriteria(fit.egarch10)[2],
    infocriteria(fit.egarch01)[2],
    infocriteria(fit.egarch11)[2]
  ),
  
  Shibata = c(
    infocriteria(fit.egarch)[3],
    infocriteria(fit.egarch10)[3],
    infocriteria(fit.egarch01)[3],
    infocriteria(fit.egarch11)[3]
  ),
  
  HannanQuinn = c(
    infocriteria(fit.egarch)[4],
    infocriteria(fit.egarch10)[4],
    infocriteria(fit.egarch01)[4],
    infocriteria(fit.egarch11)[4]
  )
  
)

arma.comparison


# 9. Residual Autocorrelation for Alternative Mean Specifications

# ARMA(0,0)-EGARCH

Box.test(
  residuals(fit.egarch, standardize = TRUE),
  lag = 20,
  type = "Ljung-Box"
)


# ARMA(1,0)-EGARCH

Box.test(
  residuals(fit.egarch10, standardize = TRUE),
  lag = 20,
  type = "Ljung-Box"
)


# ARMA(0,1)-EGARCH

Box.test(
  residuals(fit.egarch01, standardize = TRUE),
  lag = 20,
  type = "Ljung-Box"
)


# ARMA(1,1)-EGARCH

Box.test(
  residuals(fit.egarch11, standardize = TRUE),
  lag = 20,
  type = "Ljung-Box"
)


# 10. Display Selected EGARCH Model

# ARMA(0,0)-EGARCH is used as the reference model.

show(fit.egarch)


# 11. Variance Persistence

persistence(fit.garch)

persistence(fit.egarch)

persistence(fit.gjr)


# 12. Nyblom Parameter Stability Test

nyblom(fit.egarch)


# 13. Extract Conditional Volatility

volatility <- sigma(fit.egarch)

std.residuals <- residuals(
  fit.egarch,
  standardize = TRUE
)

data$J303_Volatility <- as.numeric(volatility)


# 14. Conditional Volatility Plot

plot(
  data$Date,
  volatility,
  type = "l",
  main = "Estimated J303 Conditional Volatility",
  xlab = "Date",
  ylab = "Conditional Volatility"
)


# 15. Standardized Residuals

plot(
  data$Date,
  std.residuals,
  type = "l",
  main = "Standardized Residuals",
  xlab = "Date",
  ylab = "Standardized Residual"
)


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