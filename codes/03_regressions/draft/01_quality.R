# This code evaluate the quality variation impact on prices

library(arrow)
library(data.table)
library(fixest)
library(modelsummary)
library(tibble)

# Data --------------------------------------------------------------------

tts = setDT(read_parquet(data_final("tts_final.parquet")))

# Set up var --------------------------------------------------------------

tts[, net_price := price_w - rebate_w]
tts[, log_price := log(price_w)]
tts[, log_net_price := log(net_price )]
tts[, ln_tariff := log(tariff)]

# Quality Premium - Overall Panel --------------------------------------------------------------

qual = list(
"Ln Price" = list(
  feols(log(price_w) ~ premium_panel_overall + premium_installation + premium_panel_overall*china + PV_system_size_DC + (PV_system_size_DC^2) + elec_price + mean_week_wage +
          population_density + pct_bachelor_estimate + median_home_value + median_household_income 
        | year+ county+ installer_name +origin, cluster = ~zip_code, data = tts),
  feols(log(price_w) ~ premium_panel_overall + premium_installation + PV_system_size_DC + (PV_system_size_DC^2) + elec_price + mean_week_wage +
          population_density + pct_bachelor_estimate + median_home_value + median_household_income 
        | year^origin + county + installer_name, cluster = ~zip_code, data = tts)),

"Ln Net Price" = list(
  feols(log(net_price) ~ premium_panel_overall + premium_installation + PV_system_size_DC + (PV_system_size_DC^2) + elec_price + mean_week_wage +
          population_density + pct_bachelor_estimate + median_home_value + median_household_income 
        | year+ county+ installer_name +origin, cluster = ~zip_code, data = tts),
  feols(log(net_price) ~ premium_panel_overall + premium_installation + PV_system_size_DC + (PV_system_size_DC^2) + elec_price + mean_week_wage +
          population_density + pct_bachelor_estimate + median_home_value + median_household_income 
        | year^origin + county + installer_name, cluster = ~zip_code, data = tts))
)

keep_coef = c(
  "premium_panel_overall" = "Premium Panel",
  "premium_installation" = "Premium Installation"
)

gof_list <- tribble(
  ~raw,                  ~clean,           ~fmt,
  "nobs",                "Num.Obs",        "%.0f",
  "r.squared",           "R2",             "%.3f",
  "adj.r.squared",       "R2-Adj.",        "%.3f",
  "FE: county",          "FE: County",         "%.0f",
  "FE: installer_name",  "FE: Installer",      "%.0f",
  "FE: year",            "FE: Year",           "%.0f",
  "FE: origin",          "FE: Origin",         "%.0f",
  "FE: year^origin",     "FE: Year × Origin",  "%.0f"
)

qual_table = modelsummary(
  model = qual,
  stars = TRUE,
  shape = 'cbind',
  coef_map = keep_coef,
  gof_omit = "Within|AIC|BIC|RMSE|Std.",
  gof_map = gof_list,
  output = "latex"
)

writeLines(as.character(qual_table), "output/regression/quality_premium/quality_1_full_sample.tex")

# Quality & Origin Premium Pricing ----------------------------------------

qual_origin = list(
  "Ln Price" = list(
    feols(log(price_w) ~ premium_panel_overall + premium_installation 
          + premium_panel_overall*china + premium_panel_overall*usa + premium_panel_overall*korea  
          + premium_installation*china + premium_installation*usa + premium_installation*korea  
          + PV_system_size_DC + (PV_system_size_DC^2) + elec_price + mean_week_wage +
            population_density + pct_bachelor_estimate + median_home_value + median_household_income 
          | year + county + installer_name, cluster = ~zip_code, data = tts)),
  
  "Ln Net Price" = list(
    feols(log(net_price) ~ premium_panel_overall + premium_installation 
          + premium_panel_overall*china + premium_panel_overall*usa + premium_panel_overall*korea  
          + premium_installation*china + premium_installation*usa + premium_installation*korea  
          + PV_system_size_DC + (PV_system_size_DC^2) + elec_price + mean_week_wage +
            population_density + pct_bachelor_estimate + median_home_value + median_household_income 
          | year+ county+ installer_name , cluster = ~zip_code, data = tts))
)

keep_coef_origin = c(
  "premium_panel_overall" = "Premium Panel",
  "premium_panel_overall:usa" = "Premium Panel x USA",
  "premium_panel_overall:korea" = "Premium Panel x Korea",
  "premium_installation" = "Premium Installation",
  "premium_installation:china" = "Premium Installation x China",
  "premium_installation:usa" = "Premium Installation x USA",
  "premium_installation:korea" = "Premium Installation x Korea"
)

qual_origin_table = modelsummary(
  model = qual_origin,
  stars = TRUE,
  shape = 'cbind',
  coef_map = keep_coef_origin,
  gof_omit = "Within|AIC|BIC|RMSE|Std.",
  gof_map = gof_list,
  output = "latex"
)

writeLines(as.character(qual_table), "output/regression/quality_premium/quality_1_origin.tex")

# Quality & Income --------------------------------------------------------

qual_income = list(
  "Premium Panel" = list(
    feols(premium_panel_overall ~ median_household_income +
          + PV_system_size_DC + (PV_system_size_DC^2) + elec_price + mean_week_wage +
            population_density + pct_bachelor_estimate + median_home_value 
          | year + county + installer_name + origin, cluster = ~zip_code, data = tts)),
  
  "Premium Installation" = list(feols(premium_installation ~ median_household_income + 
          + PV_system_size_DC + (PV_system_size_DC^2) + elec_price + mean_week_wage +
            population_density + pct_bachelor_estimate + median_home_value 
          | year + county + installer_name + origin, cluster = ~zip_code, data = tts)
    ),
  
  "Chinese Preference" = list(
    feols(china ~ median_household_income + 
          + PV_system_size_DC + (PV_system_size_DC^2) + elec_price + mean_week_wage +
            population_density + pct_bachelor_estimate + median_home_value 
          | year+ county+ installer_name , cluster = ~zip_code, data = tts))
  )

keep_coef_income = c(
  "median_household_income" = "Income"
  )

qual_origin_table = modelsummary(
  model = qual_income,
  stars = TRUE,
  shape = 'cbind',
  coef_map = keep_coef_income,
  gof_omit = "Within|AIC|BIC|RMSE|Std.",
  gof_map = gof_list,
  output = "latex"
)

writeLines(as.character(qual_income), "output/regression/quality_premium/quality_1_income.tex")

