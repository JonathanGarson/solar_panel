# This code evaluates the change in variety and their quality entering the U.S. territory after tariff

library(arrow)
library(data.table)
library(fixest)
library(modelsummary)
library(tibble)
library(glue)

# Data --------------------------------------------------------------------

tts = setDT(read_parquet(data_final("tts_final.parquet")))

# Variety & Quality Effect ----------------------------------------------------------

variety = tts[, .(variety = uniqueN(module_model)), by = .(year_quarter, county)]
variety[, total_variety := sum(variety), by = .(year_quarter)]


quality_brands = tts[, .(avg_efficiency = mean(efficiency_module, na.rm = TRUE)), by = .(year_quarter, module_manufacturer)]
variety_brands = tts[, .(variety = uniqueN(module_model)), by = .(year_quarter, module_manufacturer)]

# Merging Data ------------------------------------------------------------

tts[, tract := NULL]
tts = merge(tts, variety_brands, by = c("year_quarter", "module_manufacturer"))
tts = merge(tts, quality_brands, by = c("year_quarter", "module_manufacturer"))

# Variety Effect of Tariff ------------------------------------------------
# Variety
feols(log(variety) ~ log(tariff) + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price 
      + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income
      | year_quarter + county + installer_name + origin,
      cluster = ~zip_code, data = tts )

variety_change = list(
  "Overall" = list(feols(log(variety) ~ log(tariff) + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price 
                         + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income
                         | year_quarter + county + installer_name + origin,
                         cluster = ~zip_code, data = tts)),
  "Anti-Dumping : 2010-2013" = list(feols(log(variety) ~ log(tariff) + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price 
                                          + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income
                                          | year_quarter + county + installer_name + origin,
                                          cluster = ~zip_code, data = tts[year %in% 2010:2013])),
  "Anti-Dumping : 2014-2016" = list(feols(log(variety) ~ log(tariff) + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price 
                                          + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income
                                          | year_quarter + county + installer_name + origin,
                                          cluster = ~zip_code, data = tts[year %in% 2014:2016])),
  "Trade War 2018" = list(feols(log(variety) ~ log(tariff) + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price 
                                + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income
                                | year_quarter + county + installer_name + origin,
                                cluster = ~zip_code, data = tts[year %in% 2017:2018]))
)

# Quantity
quality_change = list(
  "Overall" = list(feols(log(avg_efficiency) ~ log(tariff) + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price 
        + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income
        | year_quarter + county + installer_name + origin,
        cluster = ~zip_code, data = tts)),
  "Anti-Dumping : 2010-2013" = list(feols(log(avg_efficiency) ~ log(tariff) + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price 
        + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income
        | year_quarter + county + installer_name + origin,
        cluster = ~zip_code, data = tts[year %in% 2010:2013])),
  "Anti-Dumping : 2014-2016" = list(feols(log(avg_efficiency) ~ log(tariff) + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price 
        + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income
        | year_quarter + county + installer_name + origin,
        cluster = ~zip_code, data = tts[year %in% 2014:2016])),
  "Trade War 2018" = list(feols(log(avg_efficiency) ~ log(tariff) + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price 
        + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income
        | year_quarter + county + installer_name + origin,
        cluster = ~zip_code, data = tts[year %in% 2017:2018]))
)

coef_name = c(
  "log(tariff)" = "ln Tariff"
)

gof_list <- tribble(
  ~raw,                  ~clean,           ~fmt,
  "nobs",                "Num.Obs",        "%.0f",
  "r.squared",           "R2",             "%.3f",
  "adj.r.squared",       "R2-Adj.",        "%.3f",
  "FE: county",          "FE: County",         "%.0f",
  "FE: year_quarter",    "FE: Quarter",        "%.0f",
  "FE: installer_name",  "FE: Installer",      "%.0f",
  "FE: origin",          "FE: Origin",         "%.0f",
  "FE: year",            "FE: Year",           "%.0f",
  "FE: year_origin",     "FE: Year × Origin",  "%.0f",
  "FE: quarter_origin",  "FE: Quarter × Origin",  "%.0f"
)

for (t in c(quality_change, variety_change)){
  table = modelsummary(
    models = t,
    stars = TRUE,
    shape = "cbind",
    coef_map = coef_name,
    gof_omit = "Adj|AIC|BIC|Within|Pseudo|RMSE|Std.",
    gof_map = gof_list,
    output = "latex"
  )
  writeLines(as.character(table), glue("output/regression/quality_shift/table_{t}.tex"))
}
