# 1. Load required packages
library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)
library(tseries)


# 2. Import data
data <- na.omit(data0)


###############################################################
# Section A: Exploratory Analysis
###############################################################

# 3. Correlation matrix
correlation.matrix <- cor(
  
  data[, c(
    "BTC_Volatility",
    "J303_Volatility"
  )],
  
  method = "pearson"
  
)

round(
  correlation.matrix,
  4
)


# 4. Bitcoin Volatility vs J303 Volatility
ggplot(
  data,
  aes(
    x = BTC_Volatility,
    y = J303_Volatility
  )
) +
  
  geom_point(
    alpha = 0.6
  ) +
  
  geom_smooth(
    aes(
      colour = "Linear"
    ),
    method = "lm",
    se = TRUE
  ) +
  
  geom_smooth(
    aes(
      colour = "LOESS"
    ),
    method = "loess",
    se = TRUE
  ) +
  
  scale_colour_manual(
    values = c(
      "Linear" = "blue",
      "LOESS" = "red"
    )
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Bitcoin Volatility vs J303 Volatility",
    x = "Bitcoin Conditional Volatility",
    y = "J303 Conditional Volatility",
    colour = "Trend"
  )


# 5. Pearson correlation test
btc.j303.cor <- cor.test(
  
  data$BTC_Volatility,
  
  data$J303_Volatility,
  
  method = "pearson"
  
)

btc.j303.cor


# 6. Correlation summary
correlation.summary <- data.frame(
  
  Relationship = "Bitcoin vs J303 Volatility",
  
  Correlation = unname(
    btc.j303.cor$estimate
  ),
  
  P_Value = btc.j303.cor$p.value
  
)

correlation.summary$Correlation <-
  round(
    correlation.summary$Correlation,
    4
  )

correlation.summary$P_Value <-
  signif(
    correlation.summary$P_Value,
    4
  )

correlation.summary


###############################################################
# Section B: Volatility Regression Analysis
###############################################################

# 7. Create lagged variables
data$BTC_Volatility_Lag1 <-
  dplyr::lag(
    data$BTC_Volatility,
    1
  )

data$BTC_Volatility_Lag2 <-
  dplyr::lag(
    data$BTC_Volatility,
    2
  )

data$J303_Volatility_Lag1 <-
  dplyr::lag(
    data$J303_Volatility,
    1
  )

data$J303_Volatility_Lag2 <-
  dplyr::lag(
    data$J303_Volatility,
    2
  )


# 8. No-lag model
j303.btc.0 <- lm(
  
  J303_Volatility ~
    
    BTC_Volatility,
  
  data = data
  
)


# 9. One-lag model
data.1lag <- na.omit(
  data[, c(
    "J303_Volatility",
    "BTC_Volatility",
    "BTC_Volatility_Lag1"
  )]
)

j303.btc.1 <- lm(
  
  J303_Volatility ~
    
    BTC_Volatility +
    
    BTC_Volatility_Lag1,
  
  data = data.1lag
  
)


# 10. Two-lag model
data.2lag <- na.omit(
  data[, c(
    "J303_Volatility",
    "BTC_Volatility",
    "BTC_Volatility_Lag1",
    "BTC_Volatility_Lag2"
  )]
)

j303.btc.2 <- lm(
  
  J303_Volatility ~
    
    BTC_Volatility +
    
    BTC_Volatility_Lag1 +
    
    BTC_Volatility_Lag2,
  
  data = data.2lag
  
)


# 11. Dynamic two-lag model
data.dynamic <- na.omit(
  data[, c(
    "J303_Volatility",
    "J303_Volatility_Lag1",
    "J303_Volatility_Lag2",
    "BTC_Volatility",
    "BTC_Volatility_Lag1",
    "BTC_Volatility_Lag2"
  )]
)

j303.btc.dynamic <- lm(
  
  J303_Volatility ~
    
    J303_Volatility_Lag1 +
    
    J303_Volatility_Lag2 +
    
    BTC_Volatility +
    
    BTC_Volatility_Lag1 +
    
    BTC_Volatility_Lag2,
  
  data = data.dynamic
  
)


# 12. Model summaries
summary(j303.btc.0)

summary(j303.btc.1)

summary(j303.btc.2)

summary(j303.btc.dynamic)


# 13. Newey-West robust standard errors
nw.j303.btc.0 <- coeftest(
  
  j303.btc.0,
  
  vcov = NeweyWest(
    j303.btc.0,
    prewhite = FALSE
  )
  
)

nw.j303.btc.1 <- coeftest(
  
  j303.btc.1,
  
  vcov = NeweyWest(
    j303.btc.1,
    prewhite = FALSE
  )
  
)

nw.j303.btc.2 <- coeftest(
  
  j303.btc.2,
  
  vcov = NeweyWest(
    j303.btc.2,
    prewhite = FALSE
  )
  
)

nw.j303.btc.dynamic <- coeftest(
  
  j303.btc.dynamic,
  
  vcov = NeweyWest(
    j303.btc.dynamic,
    prewhite = FALSE
  )
  
)


nw.j303.btc.0

nw.j303.btc.1

nw.j303.btc.2

nw.j303.btc.dynamic


###############################################################
# Section C: Granger Causality and Diagnostics
###############################################################

# 14. Bitcoin volatility to J303 volatility
granger.btc.j303 <- grangertest(
  
  J303_Volatility ~ BTC_Volatility,
  
  order = 2,
  
  data = data
  
)

granger.btc.j303


# 15. J303 volatility to Bitcoin volatility
granger.j303.btc <- grangertest(
  
  BTC_Volatility ~ J303_Volatility,
  
  order = 2,
  
  data = data
  
)

granger.j303.btc


# 16. Granger causality summary
granger.summary <- data.frame(
  
  Direction = c(
    
    "Bitcoin Volatility -> J303 Volatility",
    
    "J303 Volatility -> Bitcoin Volatility"
    
  ),
  
  Lags = c(
    2,
    2
  ),
  
  F_Statistic = c(
    
    granger.btc.j303$F[2],
    
    granger.j303.btc$F[2]
    
  ),
  
  P_Value = c(
    
    granger.btc.j303$`Pr(>F)`[2],
    
    granger.j303.btc$`Pr(>F)`[2]
    
  )
  
)

granger.summary$F_Statistic <-
  round(
    granger.summary$F_Statistic,
    4
  )

granger.summary$P_Value <-
  signif(
    granger.summary$P_Value,
    4
  )

granger.summary


# 17. Jarque-Bera tests
jb.j303.btc.0 <- jarque.bera.test(
  residuals(j303.btc.0)
)

jb.j303.btc.1 <- jarque.bera.test(
  residuals(j303.btc.1)
)

jb.j303.btc.2 <- jarque.bera.test(
  residuals(j303.btc.2)
)

jb.j303.btc.dynamic <- jarque.bera.test(
  residuals(j303.btc.dynamic)
)

jb.j303.btc.0
jb.j303.btc.1
jb.j303.btc.2
jb.j303.btc.dynamic


# 18. Ljung-Box tests
lb.j303.btc.0 <- Box.test(
  residuals(j303.btc.0),
  lag = 20,
  type = "Ljung-Box"
)

lb.j303.btc.1 <- Box.test(
  residuals(j303.btc.1),
  lag = 20,
  type = "Ljung-Box"
)

lb.j303.btc.2 <- Box.test(
  residuals(j303.btc.2),
  lag = 20,
  type = "Ljung-Box"
)

lb.j303.btc.dynamic <- Box.test(
  residuals(j303.btc.dynamic),
  lag = 20,
  type = "Ljung-Box"
)

lb.j303.btc.0
lb.j303.btc.1
lb.j303.btc.2
lb.j303.btc.dynamic


# 19. Breusch-Godfrey tests
bg.j303.btc.dynamic.2 <- bgtest(
  j303.btc.dynamic,
  order = 2
)

bg.j303.btc.dynamic.20 <- bgtest(
  j303.btc.dynamic,
  order = 20
)

bg.j303.btc.dynamic.2
bg.j303.btc.dynamic.20


# 20. Breusch-Pagan tests
bp.j303.btc.0 <- bptest(
  j303.btc.0
)

bp.j303.btc.1 <- bptest(
  j303.btc.1
)

bp.j303.btc.2 <- bptest(
  j303.btc.2
)

bp.j303.btc.dynamic <- bptest(
  j303.btc.dynamic
)

bp.j303.btc.0
bp.j303.btc.1
bp.j303.btc.2
bp.j303.btc.dynamic


###############################################################
# Section D: Summary Tables
###############################################################

# 21. Regression summary
regression.summary <- data.frame(
  
  Model = c(
    
    "No Lag",
    
    "1 Lag",
    
    "2 Lags",
    
    "Dynamic 2 Lags"
    
  ),
  
  BTC_Coefficient = c(
    
    unname(
      coef(j303.btc.0)["BTC_Volatility"]
    ),
    
    unname(
      coef(j303.btc.1)["BTC_Volatility"]
    ),
    
    unname(
      coef(j303.btc.2)["BTC_Volatility"]
    ),
    
    unname(
      coef(j303.btc.dynamic)["BTC_Volatility"]
    )
    
  ),
  
  BTC_Lag1_Coefficient = c(
    
    NA,
    
    unname(
      coef(j303.btc.1)["BTC_Volatility_Lag1"]
    ),
    
    unname(
      coef(j303.btc.2)["BTC_Volatility_Lag1"]
    ),
    
    unname(
      coef(j303.btc.dynamic)["BTC_Volatility_Lag1"]
    )
    
  ),
  
  BTC_Lag2_Coefficient = c(
    
    NA,
    
    NA,
    
    unname(
      coef(j303.btc.2)["BTC_Volatility_Lag2"]
    ),
    
    unname(
      coef(j303.btc.dynamic)["BTC_Volatility_Lag2"]
    )
    
  ),
  
  J303_Lag1_Coefficient = c(
    
    NA,
    
    NA,
    
    NA,
    
    unname(
      coef(j303.btc.dynamic)["J303_Volatility_Lag1"]
    )
    
  ),
  
  J303_Lag2_Coefficient = c(
    
    NA,
    
    NA,
    
    NA,
    
    unname(
      coef(j303.btc.dynamic)["J303_Volatility_Lag2"]
    )
    
  ),
  
  Adj_R2 = c(
    
    summary(j303.btc.0)$adj.r.squared,
    
    summary(j303.btc.1)$adj.r.squared,
    
    summary(j303.btc.2)$adj.r.squared,
    
    summary(j303.btc.dynamic)$adj.r.squared
    
  ),
  
  BTC_NW_pvalue = c(
    
    nw.j303.btc.0[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    
    nw.j303.btc.1[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    
    nw.j303.btc.2[
      "BTC_Volatility",
      "Pr(>|t|)"
    ],
    
    nw.j303.btc.dynamic[
      "BTC_Volatility",
      "Pr(>|t|)"
    ]
    
  ),
  
  BTC_Lag1_NW_pvalue = c(
    
    NA,
    
    nw.j303.btc.1[
      "BTC_Volatility_Lag1",
      "Pr(>|t|)"
    ],
    
    nw.j303.btc.2[
      "BTC_Volatility_Lag1",
      "Pr(>|t|)"
    ],
    
    nw.j303.btc.dynamic[
      "BTC_Volatility_Lag1",
      "Pr(>|t|)"
    ]
    
  ),
  
BTC_Lag2_NW_pvalue = c(
  
  NA,
  
  NA,
  
  nw.j303.btc.2[
    "BTC_Volatility_Lag2",
    "Pr(>|t|)"
  ],
  
  nw.j303.btc.dynamic[
    "BTC_Volatility_Lag2",
    "Pr(>|t|)"
  ]
  
)

)


regression.summary$BTC_Coefficient <-
  signif(
    regression.summary$BTC_Coefficient,
    4
  )

regression.summary$BTC_Lag1_Coefficient <-
  signif(
    regression.summary$BTC_Lag1_Coefficient,
    4
  )

regression.summary$BTC_Lag2_Coefficient <-
  signif(
    regression.summary$BTC_Lag2_Coefficient,
    4
  )

regression.summary$J303_Lag1_Coefficient <-
  signif(
    regression.summary$J303_Lag1_Coefficient,
    4
  )

regression.summary$J303_Lag2_Coefficient <-
  signif(
    regression.summary$J303_Lag2_Coefficient,
    4
  )

regression.summary$Adj_R2 <-
  round(
    regression.summary$Adj_R2,
    4
  )

regression.summary$BTC_NW_pvalue <-
  signif(
    regression.summary$BTC_NW_pvalue,
    4
  )

regression.summary$BTC_Lag1_NW_pvalue <-
  signif(
    regression.summary$BTC_Lag1_NW_pvalue,
    4
  )

regression.summary$BTC_Lag2_NW_pvalue <-
  signif(
    regression.summary$BTC_Lag2_NW_pvalue,
    4
  )

regression.summary


# 22. Granger causality summary
granger.summary


# 23. Diagnostic summary
diagnostic.summary <- data.frame(
  
  Model = c(
    
    "No Lag",
    
    "1 Lag",
    
    "2 Lags",
    
    "Dynamic 2 Lags"
    
  ),
  
  JB_P_Value = c(
    
    jb.j303.btc.0$p.value,
    
    jb.j303.btc.1$p.value,
    
    jb.j303.btc.2$p.value,
    
    jb.j303.btc.dynamic$p.value
    
  ),
  
  Ljung_Box_P_Value = c(
    
    lb.j303.btc.0$p.value,
    
    lb.j303.btc.1$p.value,
    
    lb.j303.btc.2$p.value,
    
    lb.j303.btc.dynamic$p.value
    
  ),
  
  BP_P_Value = c(
    
    bp.j303.btc.0$p.value,
    
    bp.j303.btc.1$p.value,
    
    bp.j303.btc.2$p.value,
    
    bp.j303.btc.dynamic$p.value
    
  )
  
)

diagnostic.summary$JB_P_Value <-
  signif(
    diagnostic.summary$JB_P_Value,
    4
  )

diagnostic.summary$Ljung_Box_P_Value <-
  signif(
    diagnostic.summary$Ljung_Box_P_Value,
    4
  )

diagnostic.summary$BP_P_Value <-
  signif(
    diagnostic.summary$BP_P_Value,
    4
  )

diagnostic.summary


# 24. Dynamic model serial correlation summary
dynamic.serial.summary <- data.frame(
  
  Test = c(
    
    "Breusch-Godfrey (2 lags)",
    
    "Breusch-Godfrey (20 lags)"
    
  ),
  
  Statistic = c(
    
    unname(
      bg.j303.btc.dynamic.2$statistic
    ),
    
    unname(
      bg.j303.btc.dynamic.20$statistic
    )
    
  ),
  
  P_Value = c(
    
    bg.j303.btc.dynamic.2$p.value,
    
    bg.j303.btc.dynamic.20$p.value
    
  )
  
)

dynamic.serial.summary$Statistic <-
  round(
    dynamic.serial.summary$Statistic,
    4
  )

dynamic.serial.summary$P_Value <-
  signif(
    dynamic.serial.summary$P_Value,
    4
  )

dynamic.serial.summary


