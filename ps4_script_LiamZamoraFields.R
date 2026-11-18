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
#-----------------------------------------------------------------------------
#2.1
#-----------------------------------------------------------------------------
autoplot(liquor, .vars = sales) +
  labs(title = "Monthly US Liquor Sales", y = "Sales (millions of dollars)", x = "Year")

liquor %>%
  gg_season(log(sales)) +
  labs(title = "Seasonal Plot of Monthly US Liquor Sales", y = " Log Sales (millions of dollars)", x = "Month")

#There is clearly an upwards trend in the series, along with a seasonal pattern. 
#The summer months have high alcohol sales, but December has the highest sales.
#The seasonal swings are growing with the level, so using logs would be appropriate.

#-----------------------------------------------------------------------------
#2.2
#-----------------------------------------------------------------------------
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

#2.2
liquor_fit |> select(arima) |> gg_tsresiduals()

liquor_fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 24, dof = 6)
#The residuals do not look like white noise. Reject the null at pvalue 0.

#-----------------------------------------------------------------------------
#2.3
#-----------------------------------------------------------------------------
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

#-----------------------------------------------------------------------------
#2.4
#-----------------------------------------------------------------------------
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

