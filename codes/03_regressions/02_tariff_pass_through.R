# We have tariff pass through effect here

library(arrow)
library(data.table)
library(fixest)
library(modelsummary)

# Data --------------------------------------------------------------------

tts = setDT(read_parquet(data_final("tts_final.parquet")))

# Pass-Through Estimation ----------------------------------------------------------------
tts[, net_price := price_w - rebate_w]

tts = tts[rebate_w < price_w]
system_vars <- c("PV_system_size_DC", "I(PV_system_size_DC^2)", "elec_price", "mean_week_wage")
dem_vars <- c("population_density", "pct_bachelor_estimate", "median_home_value", "median_household_income")

# Main interaction term
tts[, ln_tariff := log(tariff)]
base_rhs <- "ln_tariff*premium_panel_overall + ln_tariff*premium_installation"

# Combine everything
rhs <- paste(c(base_rhs, system_vars, dem_vars), collapse = " + ")
full_formula_str <- paste("log(price_w) ~", rhs) # Changing for net price to gross price influence the results for AD2010-2013
full_formula_str <- paste("log(net_price) ~", rhs)

# Convert to formula
full_formula <- as.formula(full_formula_str)

tariff_pt =  list(
  
  "Overall" = list(
    feols(full_formula, fixef = c("year_quarter", "origin", "county"), cluster = ~ zip_code, data = tts),
    feols(full_formula, fixef = c("year_quarter", "origin", "county","installer_name"), cluster = ~ zip_code, data = tts)
    ),
  
  "Anti-Dumping : 2010 - 2013" = list(
    feols(full_formula, fixef = c("year_quarter", "origin", "county"), cluster = ~ zip_code, data = tts[year %in% 2010:2013]),
    feols(full_formula, fixef = c("year_quarter", "origin", "county","installer_name"), cluster = ~ zip_code, data = tts[year %in% 2010:2013])
    ),
  
  "Anti-Dumping : 2014 - 2016" = list(
    feols(full_formula, fixef =   c("year_quarter", "origin", "county"), cluster = ~ zip_code,data = tts[year %in% 2013:2016]),
    feols(full_formula, fixef =   c("year_quarter", "origin", "county","installer_name"), cluster = ~ zip_code,data = tts[year %in% 2013:2016])
    ),
  
  "Trade War 2018" = list(
    feols(full_formula, fixef = c("year_quarter", "origin", "county", "utility"), cluster = ~ zip_code, data = tts[year %in% 2017:2018]),
    feols(full_formula, fixef = c("year_quarter", "origin", "county","installer_name"), cluster = ~ zip_code, data = tts[year %in% 2017:2018])
    )
)

coef_name = c(
  "ln_tariff" = "ln Tariff",
  "ln_tariff:premium_panel_overall" = "ln Tariff x Premium Panel",
  "ln_tariff:premium_installation" = "ln Tariff x Premium Installation"
)

modelsummary(
  models = tariff_pt,
  stars = TRUE,
  shape = "cbind",
  coef_map = coef_name,
  gof_omit = "Adj|AIC|BIC|Within|Pseudo|RMSE|Std."
  )

# Phase off Tariff ---------------------------------------------------------

post_st = feols(full_formula, fixef = c("year_quarter", "origin", "county"), cluster = ~zip_code, data = tts[year %in% 2019:2020])


# test --------------------------------------------------------------------

# ad_1 = feols(c(price_w, log(price_w)) ~ log(tariff)*premium_panel_overall | year_quarter + origin + county + installer_name, cluster = ~zip_code, data = tts[year %in% 2010:2013])
# ad_2 = feols(c(price_w, log(price_w)) ~ log(tariff)*premium_panel_overall | year_quarter + origin + county + installer_name, cluster = ~zip_code, data = tts[year %in% 2013:2016])
# st = feols(c(price_w, log(price_w)) ~ log(tariff)*premium_panel_overall | year_quarter + county + installer_name, cluster = ~zip_code, data = tts[year %in% 2017:2018])

