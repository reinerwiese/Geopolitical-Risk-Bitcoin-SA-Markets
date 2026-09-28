###############################################################
# 9. Post-2017 Robustness Analysis
###############################################################

# 1. Packages

library(dplyr)
library(lmtest)
library(sandwich)


# 2. Check data

stopifnot(exists("data0"))


# 3. Prepare data

full.data <- data0 %>%
  arrange(Date)

post2017.data <- data0 %>%
  filter(Date >= as.Date("2018-01-01")) %>%
  arrange(Date)

cat(
  "\nFull sample:",
  min(full.data$Date),
  "to",
  max(full.data$Date),
  "\n"
)

cat(
  "Post-2017 sample:",
  min(post2017.data$Date),
  "to",
  max(post2017.data$Date),
  "\n"
)

cat(
  "Full-sample observations:",
  nrow(full.data),
  "\n"
)

cat(
  "Post-2017 observations:",
  nrow(post2017.data),
  "\n"
)


# 4. GPR -> Bitcoin volatility

full.gpr <- full.data %>%
  mutate(
    GPR_Lag1 = lag(GPRD, 1),
    GPR_Lag2 = lag(GPRD, 2)
  ) %>%
  na.omit()

post2017.gpr <- post2017.data %>%
  mutate(
    GPR_Lag1 = lag(GPRD, 1),
    GPR_Lag2 = lag(GPRD, 2)
  ) %>%
  na.omit()


# 5. Full-sample GPR models

full.gpr.model1 <- lm(
  BTC_Volatility ~ GPRD,
  data = full.gpr
)

full.gpr.model2 <- lm(
  BTC_Volatility ~ GPRD + GPR_Lag1,
  data = full.gpr
)

full.gpr.model3 <- lm(
  BTC_Volatility ~ GPRD + GPR_Lag1 + GPR_Lag2,
  data = full.gpr
)


# 6. Post-2017 GPR models

post2017.gpr.model1 <- lm(
  BTC_Volatility ~ GPRD,
  data = post2017.gpr
)

post2017.gpr.model2 <- lm(
  BTC_Volatility ~ GPRD + GPR_Lag1,
  data = post2017.gpr
)

post2017.gpr.model3 <- lm(
  BTC_Volatility ~ GPRD + GPR_Lag1 + GPR_Lag2,
  data = post2017.gpr
)


# 7. Newey-West standard errors

nw.full.gpr.1 <- coeftest(
  full.gpr.model1,
  vcov = NeweyWest(full.gpr.model1, prewhite = FALSE)
)

nw.full.gpr.2 <- coeftest(
  full.gpr.model2,
  vcov = NeweyWest(full.gpr.model2, prewhite = FALSE)
)

nw.full.gpr.3 <- coeftest(
  full.gpr.model3,
  vcov = NeweyWest(full.gpr.model3, prewhite = FALSE)
)

nw.post2017.gpr.1 <- coeftest(
  post2017.gpr.model1,
  vcov = NeweyWest(post2017.gpr.model1, prewhite = FALSE)
)

nw.post2017.gpr.2 <- coeftest(
  post2017.gpr.model2,
  vcov = NeweyWest(post2017.gpr.model2, prewhite = FALSE)
)

nw.post2017.gpr.3 <- coeftest(
  post2017.gpr.model3,
  vcov = NeweyWest(post2017.gpr.model3, prewhite = FALSE)
)


# 8. GPR robustness comparison

gpr.post2017.comparison <- data.frame(
  Sample = c(
    "Full sample",
    "Post-2017",
    "Full sample",
    "Post-2017",
    "Full sample",
    "Post-2017"
  ),
  Model = c(
    "No Lag",
    "No Lag",
    "1 Lag",
    "1 Lag",
    "2 Lags",
    "2 Lags"
  ),
  N = c(
    nobs(full.gpr.model1),
    nobs(post2017.gpr.model1),
    nobs(full.gpr.model2),
    nobs(post2017.gpr.model2),
    nobs(full.gpr.model3),
    nobs(post2017.gpr.model3)
  ),
  GPR_Coefficient = c(
    coef(full.gpr.model1)["GPRD"],
    coef(post2017.gpr.model1)["GPRD"],
    coef(full.gpr.model2)["GPRD"],
    coef(post2017.gpr.model2)["GPRD"],
    coef(full.gpr.model3)["GPRD"],
    coef(post2017.gpr.model3)["GPRD"]
  ),
  GPR_NW_pvalue = c(
    nw.full.gpr.1["GPRD", "Pr(>|t|)"],
    nw.post2017.gpr.1["GPRD", "Pr(>|t|)"],
    nw.full.gpr.2["GPRD", "Pr(>|t|)"],
    nw.post2017.gpr.2["GPRD", "Pr(>|t|)"],
    nw.full.gpr.3["GPRD", "Pr(>|t|)"],
    nw.post2017.gpr.3["GPRD", "Pr(>|t|)"]
  )
)

gpr.post2017.comparison$GPR_Coefficient <-
  signif(gpr.post2017.comparison$GPR_Coefficient, 4)

gpr.post2017.comparison$GPR_NW_pvalue <-
  signif(gpr.post2017.comparison$GPR_NW_pvalue, 4)

gpr.post2017.comparison


# 9. Bitcoin -> J303 volatility

full.spillover <- full.data %>%
  mutate(
    BTC_Volatility_Lag1 = lag(BTC_Volatility, 1),
    BTC_Volatility_Lag2 = lag(BTC_Volatility, 2),
    J303_Volatility_Lag1 = lag(J303_Volatility, 1),
    J303_Volatility_Lag2 = lag(J303_Volatility, 2)
  ) %>%
  na.omit()

post2017.spillover <- post2017.data %>%
  mutate(
    BTC_Volatility_Lag1 = lag(BTC_Volatility, 1),
    BTC_Volatility_Lag2 = lag(BTC_Volatility, 2),
    J303_Volatility_Lag1 = lag(J303_Volatility, 1),
    J303_Volatility_Lag2 = lag(J303_Volatility, 2)
  ) %>%
  na.omit()


# 10. Full-sample Bitcoin -> J303 models

full.j303.model0 <- lm(
  J303_Volatility ~ BTC_Volatility,
  data = full.spillover
)

full.j303.model1 <- lm(
  J303_Volatility ~ BTC_Volatility + BTC_Volatility_Lag1,
  data = full.spillover
)

full.j303.model2 <- lm(
  J303_Volatility ~ BTC_Volatility +
    BTC_Volatility_Lag1 +
    BTC_Volatility_Lag2,
  data = full.spillover
)

full.j303.dynamic <- lm(
  J303_Volatility ~
    J303_Volatility_Lag1 +
    J303_Volatility_Lag2 +
    BTC_Volatility +
    BTC_Volatility_Lag1 +
    BTC_Volatility_Lag2,
  data = full.spillover
)


# 11. Post-2017 Bitcoin -> J303 models

post2017.j303.model0 <- lm(
  J303_Volatility ~ BTC_Volatility,
  data = post2017.spillover
)

post2017.j303.model1 <- lm(
  J303_Volatility ~ BTC_Volatility + BTC_Volatility_Lag1,
  data = post2017.spillover
)

post2017.j303.model2 <- lm(
  J303_Volatility ~ BTC_Volatility +
    BTC_Volatility_Lag1 +
    BTC_Volatility_Lag2,
  data = post2017.spillover
)

post2017.j303.dynamic <- lm(
  J303_Volatility ~
    J303_Volatility_Lag1 +
    J303_Volatility_Lag2 +
    BTC_Volatility +
    BTC_Volatility_Lag1 +
    BTC_Volatility_Lag2,
  data = post2017.spillover
)


# 12. Newey-West standard errors

nw.full.j303.0 <- coeftest(
  full.j303.model0,
  vcov = NeweyWest(full.j303.model0, prewhite = FALSE)
)

nw.full.j303.1 <- coeftest(
  full.j303.model1,
  vcov = NeweyWest(full.j303.model1, prewhite = FALSE)
)

nw.full.j303.2 <- coeftest(
  full.j303.model2,
  vcov = NeweyWest(full.j303.model2, prewhite = FALSE)
)

nw.full.j303.dynamic <- coeftest(
  full.j303.dynamic,
  vcov = NeweyWest(full.j303.dynamic, prewhite = FALSE)
)

nw.post2017.j303.0 <- coeftest(
  post2017.j303.model0,
  vcov = NeweyWest(post2017.j303.model0, prewhite = FALSE)
)

nw.post2017.j303.1 <- coeftest(
  post2017.j303.model1,
  vcov = NeweyWest(post2017.j303.model1, prewhite = FALSE)
)

nw.post2017.j303.2 <- coeftest(
  post2017.j303.model2,
  vcov = NeweyWest(post2017.j303.model2, prewhite = FALSE)
)

nw.post2017.j303.dynamic <- coeftest(
  post2017.j303.dynamic,
  vcov = NeweyWest(post2017.j303.dynamic, prewhite = FALSE)
)


# 13. Bitcoin -> J303 robustness comparison

j303.post2017.comparison <- data.frame(
  Sample = c(
    "Full sample",
    "Post-2017",
    "Full sample",
    "Post-2017",
    "Full sample",
    "Post-2017",
    "Full sample",
    "Post-2017"
  ),
  Model = c(
    "No Lag",
    "No Lag",
    "1 Lag",
    "1 Lag",
    "2 Lags",
    "2 Lags",
    "Dynamic 2 Lags",
    "Dynamic 2 Lags"
  ),
  N = c(
    nobs(full.j303.model0),
    nobs(post2017.j303.model0),
    nobs(full.j303.model1),
    nobs(post2017.j303.model1),
    nobs(full.j303.model2),
    nobs(post2017.j303.model2),
    nobs(full.j303.dynamic),
    nobs(post2017.j303.dynamic)
  ),
  BTC_Coefficient = c(
    coef(full.j303.model0)["BTC_Volatility"],
    coef(post2017.j303.model0)["BTC_Volatility"],
    coef(full.j303.model1)["BTC_Volatility"],
    coef(post2017.j303.model1)["BTC_Volatility"],
    coef(full.j303.model2)["BTC_Volatility"],
    coef(post2017.j303.model2)["BTC_Volatility"],
    coef(full.j303.dynamic)["BTC_Volatility"],
    coef(post2017.j303.dynamic)["BTC_Volatility"]
  ),
  BTC_NW_pvalue = c(
    nw.full.j303.0["BTC_Volatility", "Pr(>|t|)"],
    nw.post2017.j303.0["BTC_Volatility", "Pr(>|t|)"],
    nw.full.j303.1["BTC_Volatility", "Pr(>|t|)"],
    nw.post2017.j303.1["BTC_Volatility", "Pr(>|t|)"],
    nw.full.j303.2["BTC_Volatility", "Pr(>|t|)"],
    nw.post2017.j303.2["BTC_Volatility", "Pr(>|t|)"],
    nw.full.j303.dynamic["BTC_Volatility", "Pr(>|t|)"],
    nw.post2017.j303.dynamic["BTC_Volatility", "Pr(>|t|)"]
  ),
  BTC_Lag1_NW_pvalue = c(
    NA,
    NA,
    nw.full.j303.1["BTC_Volatility_Lag1", "Pr(>|t|)"],
    nw.post2017.j303.1["BTC_Volatility_Lag1", "Pr(>|t|)"],
    nw.full.j303.2["BTC_Volatility_Lag1", "Pr(>|t|)"],
    nw.post2017.j303.2["BTC_Volatility_Lag1", "Pr(>|t|)"],
    nw.full.j303.dynamic["BTC_Volatility_Lag1", "Pr(>|t|)"],
    nw.post2017.j303.dynamic["BTC_Volatility_Lag1", "Pr(>|t|)"]
  ),
  BTC_Lag2_NW_pvalue = c(
    NA,
    NA,
    NA,
    NA,
    nw.full.j303.2["BTC_Volatility_Lag2", "Pr(>|t|)"],
    nw.post2017.j303.2["BTC_Volatility_Lag2", "Pr(>|t|)"],
    nw.full.j303.dynamic["BTC_Volatility_Lag2", "Pr(>|t|)"],
    nw.post2017.j303.dynamic["BTC_Volatility_Lag2", "Pr(>|t|)"]
  )
)

j303.post2017.comparison$BTC_Coefficient <-
  signif(j303.post2017.comparison$BTC_Coefficient, 4)

j303.post2017.comparison$BTC_NW_pvalue <-
  signif(j303.post2017.comparison$BTC_NW_pvalue, 4)

j303.post2017.comparison$BTC_Lag1_NW_pvalue <-
  signif(j303.post2017.comparison$BTC_Lag1_NW_pvalue, 4)

j303.post2017.comparison$BTC_Lag2_NW_pvalue <-
  signif(j303.post2017.comparison$BTC_Lag2_NW_pvalue, 4)

j303.post2017.comparison


# 14. Granger causality

full.granger.data <- full.data %>%
  select(Date, BTC_Volatility, J303_Volatility) %>%
  na.omit()

post2017.granger.data <- post2017.data %>%
  select(Date, BTC_Volatility, J303_Volatility) %>%
  na.omit()


# Bitcoin -> J303

full.granger.btc.j303 <- grangertest(
  J303_Volatility ~ BTC_Volatility,
  order = 2,
  data = full.granger.data
)

post2017.granger.btc.j303 <- grangertest(
  J303_Volatility ~ BTC_Volatility,
  order = 2,
  data = post2017.granger.data
)


# J303 -> Bitcoin

full.granger.j303.btc <- grangertest(
  BTC_Volatility ~ J303_Volatility,
  order = 2,
  data = full.granger.data
)

post2017.granger.j303.btc <- grangertest(
  BTC_Volatility ~ J303_Volatility,
  order = 2,
  data = post2017.granger.data
)


# 15. Granger results

full.granger.btc.j303
post2017.granger.btc.j303
full.granger.j303.btc
post2017.granger.j303.btc