# This code propose a panel analysis of our results

library(arrow)
library(data.table)
library(fixest)
library(modelsummary)
library(DIDmultiplegtDYN)

# Data

tts = setDT(read_parquet(data_final("tts_panel.parquet")))

# Pass-Through
tts[, `:=` (ln_price = log(price_w), ln_tariff = log(tariff))]

tariff_across_firm = feols(log(price_w) ~ log(tariff) | year_quarter + origin, cluster = "module_manufacturer" , data = tts)
modelsummary(
  tariff_across_firm,
  stars = TRUE,
  gof_omit = "Within|AIC|BIC|RMSE|Std")
  
tariff_within_firm = feols(log(price_w) ~ log(tariff) | year_quarter + module_manufacturer, cluster = "module_manufacturer" , data = tts)
modelsummary(
  tariff_within_firm,
  stars = TRUE,
  gof_omit = "Within|AIC|BIC|RMSE|Std")