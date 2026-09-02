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


# 4. Specify Candidate Models

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


# 5. Estimate Models

fit.garch <- ugarchfit(
  spec = spec.garch,
  data = J303
)

fit.egarch <- ugarchfit(
  spec = spec.egarch,
  data = J303
)


# 6. Compare Variance Models

comparison <- data.frame(
  
  Model = c(
    "GARCH(1,1)",
    "EGARCH(1,1)"
  ),
  
  LogLikelihood = c(
    likelihood(fit.garch),
    likelihood(fit.egarch)
  ),
  
  AIC = c(
    infocriteria(fit.garch)[1],
    infocriteria(fit.egarch)[1]
  ),
  
  BIC = c(
    infocriteria(fit.garch)[2],
    infocriteria(fit.egarch)[2]
  ),
  
  Shibata = c(
    infocriteria(fit.garch)[3],
    infocriteria(fit.egarch)[3]
  ),
  
  HannanQuinn = c(
    infocriteria(fit.garch)[4],
    infocriteria(fit.egarch)[4]
  )
  
)

comparison


# 7. Compare Mean Specifications for EGARCH

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
  )
  
)

arma.comparison


# 8. Model Summary

show(fit.egarch)


# 9. Extract Results

volatility <- sigma(fit.egarch)

std.residuals <- residuals(
  fit.egarch,
  standardize = TRUE
)

data$J303_Volatility <- as.numeric(volatility)


# 10. Conditional Volatility

plot(
  data$Date,
  volatility,
  type = "l",
  main = "Estimated J303 Conditional Volatility",
  xlab = "Date",
  ylab = "Conditional Volatility"
)


# 11. Standardized Residuals

plot(
  data$Date,
  std.residuals,
  type = "l",
  main = "Standardized Residuals",
  xlab = "Date",
  ylab = "Standardized Residual"
)


# 12. Residual Distribution

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


qqnorm(
  std.residuals,
  main = "QQ Plot of Standardized Residuals"
)

qqline(
  std.residuals,
  col = "red"
)


# 13. Residual Diagnostics

# Test for remaining serial correlation
Box.test(
  std.residuals,
  lag = 20,
  type = "Ljung-Box"
)

# Test for remaining volatility clustering
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
