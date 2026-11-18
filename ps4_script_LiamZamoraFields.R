library(tidyverse)
library(fpp3)
library(BVAR)

##PART 2
#-----------------------------------------------------------------------------
liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)

#2.1

autoplot(liquor, .vars = sales) +
  labs(title = "Monthly US Liquor Sales", y = "Sales (millions of dollars)", x = "Year")

liquor %>%
  gg_season(log(sales)) +
  labs(title = "Seasonal Plot of Monthly US Liquor Sales", y = " Log Sales (millions of dollars)", x = "Month")

#There is clearly an upwards trend in the series, along with a seasonal pattern. 
#The summer months have high alcohol sales, but December has the highest sales.
#The seasonal swings are growing with the level, so using logs would be appropriate.


#2.2


#2.2a
liquor_fit <- liquor |>
  model(arima = ARIMA(log(sales)),
        ets   = ETS(log(sales)))

liquor_fit |> select(arima) |> report()
liquor_fit |> select(ets) |> report()

#ARIMA: It chose an ARIMA(1,0,2)(2,1,1)[12] model with drift. This means that 
#it chose one seasonal difference.Two seasonal lags are used for the autoregressive part, 
#and one seasonal lag is used for the moving average part. It chose to do no additional differencing,
#and it chose to use two non-seasonal moving average lags and one non-seasonal autoregressive lag.

#ETS: R specified additive error, trend, and seasonal components on the logged series. 
#It produced a small beta and gamma, meaning that changes in trend 
#and seasonal swings are fairly constant. 

#2.2b
liquor_fit |> select(arima) |> gg_tsresiduals()

liquor_fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 24, dof = 6)
#The residuals do not look like white noise. Reject the null at pvalue 0.


#2.3


#2.3a
liquor_cv <- liquor |>
  stretch_tsibble(.init = 120, .step = 1) |>
  filter(.id < max(.id))

#liquor_cv_fit <- liquor_cv |>
#  model(snaive = SNAIVE(sales),
#        ets    = ETS(log(sales)),
#        arima  = ARIMA(log(sales)))

liquor_cv_fit <- readRDS("results/liquor_cv_fit.rds")

saveRDS(liquor_cv_fit, "results/liquor_cv_fit.rds")
#216 forecast origins were considered.


#2.3b
liquor_cv_fc <- liquor_cv_fit |>
  forecast(h = 1)

liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE)) |> knitr::kable(digits = 2)

#First Month: Jan 2002
#Last month: December 2019

#The ARIMA model is the better model. Both RMSE and MAE are lower.

#2.4

liquor_errs <- liquor_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(liquor) |> select(Month, actual = sales),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

forecast::dm.test(liquor_errs$arima, liquor_errs$ets, h = 1, power = 2)

forecast::dm.test(liquor_errs$arima, liquor_errs$snaive, h = 1, power = 2)

#ARIMA vs ETS: DM = -2.90, p-val = 0.0040

#ARIMA vs SNAIVE: DM = -10.755, p-val = ~0.000

#I would chose the ARIMA model, as it performs better by the metric of RMSE and MAE, 
#and the DM test shows that the ARIMA model is significantly better than both ETS and SNAIVE.


#-----------------------------------------------------------------------------
#PART 3
#-----------------------------------------------------------------------------
#3a
liquor_combo_fc <- liquor_cv_fit |>
  mutate(combo = (arima + ets) / 2) |>
  forecast(h = 1)

liquor_combo_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))
#combo is better than ets on its own, but worse than ARIMA.

#3b
cor(liquor_errs$arima, liquor_errs$ets)

#There is large correlation between the errors of ETS and ARIMA. If we could optimally
#deal with this I would use covariance to reduce the size of a weight. because the ETS model 
#is significantly worse than the ARIMA model via DM test, I would conclude equal weights
# is a bad way to combine these two models.

#-----------------------------------------------------------------------------
#PART 4
#-----------------------------------------------------------------------------
#4.1a
qd_raw <- read_csv("data/fredqd_2026-09.csv", show_col_types = FALSE) |>
  slice(-(1:2))

#4.1b
growth <- function(x) 100 * difference(log(x))

qd <- qd_raw |>
  mutate(date = as.Date(sasdate, "%m/%d/%Y")) |>
  transmute(date,
            gdp    = growth(GDPC1),     # real GDP
            cons   = growth(PCECC96),   # real consumption
            inv    = growth(GPDIC1),    # real investment
            emp    = growth(PAYEMS),    # payroll employment
            infl   = growth(GDPCTPI),   # GDP price index
            unrate = difference(UNRATE),     # unemployment rate
            ffr    = difference(FEDFUNDS),   # federal funds rate
            gs10   = difference(GS10)) |>    # 10-year Treasury yield
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))

y <- qd |> select(-date) |> as.matrix()

nrow(y)
head(y)

#4.1c
N <- nrow(y)
origins <- 120:(N - 1)
actual <- y[121:N, ]


#There will be 120 forcasts. The first will be Q1 1990 and the last will be Q4 2019


#4.2
var_ols_fc <- function(z, p) {
  e <- embed(z, p + 1)            # columns: y_t, then its lags 1 to p
  M <- ncol(z)                    # number of variables
  B <- coef(lm(e[, 1:M] ~ e[, -(1:M)]))
  x <- c(1, t(z[nrow(z):(nrow(z) - p + 1), ]))   # constant, latest p lags
  x %*% B
}

fc_var1 <- NULL
fc_var4 <- NULL
for (T in origins) {
  fc_var1 <- rbind(fc_var1, var_ols_fc(y[1:T, ], 1))
  fc_var4 <- rbind(fc_var4, var_ols_fc(y[1:T, ], 4))
}

rmse_by_var <- function(f) sqrt(colMeans((actual - f)^2))

rmse_by_var(fc_var1)
rmse_by_var(fc_var4)

#The VAR(1) forecasts better for 7 of the 8 variables. VAR(4) only does better on inflation.
#The smaller model does better because the larger model is likely overfitting.

#4.3a
bvar_cv <- function(lambda) {
  pr <- bv_priors(hyper = "lambda",
                  mn = bv_mn(lambda = bv_lambda(mode = lambda, sd = 0.4,
                                                min = 0.999 * lambda,
                                                max = 1.001 * lambda),
                             b = 0))
  fc <- NULL
  for (T in origins) {
    fit_T <- bvar(y[1:T, ], lags = 4, priors = pr,
                  n_draw = 2000, n_burn = 1000, verbose = FALSE)
    x <- c(1, t(y[T:(T - 3), ]))         # constant and 4 lags of each variable
    fc <- rbind(fc, x %*% coef(fit_T))   # forecast of y_{T+1}
  }
  fc
}

set.seed(422)
#fc_bvar_03 <- bvar_cv(0.3)
saveRDS(fc_bvar_03, "results/fc_bvar_03.rds")
fc_bvar_03 <- readRDS("results/fc_bvar_03.rds")

#4.3b
set.seed(422)
#fc_bvar_005 <- bvar_cv(0.05)
saveRDS(fc_bvar_005, "results/fc_bvar_005.rds")
fc_bvar_005 <- readRDS("results/fc_bvar_005.rds")

#var_results <- tibble(
#  Variable = colnames(y),
#  `VAR(1)` = rmse_by_var(fc_var1),
#  `VAR(4)` = rmse_by_var(fc_var4),
#  `BVAR(4), lambda = 0.3` = rmse_by_var(fc_bvar_03),
#  `BVAR(4), lambda = 0.05` = rmse_by_var(fc_bvar_005)
#)

var_results <- readRDS("results/var_results.rds")
saveRDS(var_results, "results/var_results.rds")

var_results |> knitr::kable(digits = 3)

#the BVAR(4) with lam=0.3 performs better than the OLS VAR(4) for all variables. 
#The Minnesota prior has the effect of making more distant lags matter less, 
#which has improved the accuracy of the model. The parameters also get shrunk towards 0.

#VAR(1) performs better than BVAR(4) lam=0.3 for 5 of the 8 variables.

#The BVAR(4) with lam=0.05 performs worse than the BVAR(4) lam=0.3 for all variable but one.
#There may be too much shrinkage with a lamda that small. RMSE is a fair estimate of how
#acculturately it will forecast in the future assuming future data looks like the data in the 
#test period.


#4.3c
e_var4 <- actual[, "gdp"] - fc_var4[, 1]
e_bvar <- actual[, "gdp"] - fc_bvar_03[, 1]

forecast::dm.test(e_var4, e_bvar, h = 1, power = 2)


e_var5 <- actual[, "cons"] - fc_var4[, 2]
e_bvar1 <- actual[, "cons"] - fc_bvar_03[, 2]

forecast::dm.test(e_var5, e_bvar1, h = 1, power = 2)


#I reject the null that BVAR(4) and VAR(4) have equal forecast accuracy for 
#GDP or consumption growth. It doesn't really change my thinking from 4.3b.


