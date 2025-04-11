# This code estimate the curvature of demand to recover the primitives and test if the convexity of demand is compatible with incomplete pass-through

library(arrow)
library(data.table)
library(fixest)
library(glue)

# Data --------------------------------------------------------------------

tts = read_parquet(data_final("tts_final.parquet"))

# Cleaning Up -------------------------------------------------------------
# We reorganize the data at the zip code level

names(tts)
short = tts[,  .(mean_install_p_z = mean(total_installed_price, na.rm = T), 
                    mean_price_w_z = mean(price_w, na.rm = T),
                    mean_rebate_z = mean(rebate_w, na.rm = T),
                    mean_size_z = mean(PV_system_size_DC, na.rm = T),
                    mean_pop_z = population,
                    mean_density_z = population_density,
                    median_home_value_z = median_home_value,
                    median_household_income_z = median_household_income,
                    mean_premium_overall_z = mean(premium_panel_overall, na.rm = T),
                    mean_premium_ad1_z = mean(premium_panel_ad1, na.rm = T),
                    mean_premium_ad1_z = mean(premium_panel_ad2, na.rm = T),
                    mean_premium_st_z = mean(premium_panel_st, na.rm = T),
                    mean_premium_installation_z = mean(premium_installation, na.rm = T),
                    mean_elec_z = elec_price,
                    h_mean_z = h_mean,
                    h_median_z = h_median,
                    state = state,
                    county = county, 
                    ln_tariff = log(tariff),
                    tariff = tariff,
                    demand_zip_code = demand_zip_code
                    )
            , by = .(zip_code, year)]
setorder(short, zip_code, year)
setkey(short, zip_code, year)

# Set Up ------------------------------------------------------------------

short[, post_incentive_price_w_z := mean_price_w_z - mean_rebate_z]
short[, post_incentive_price_w_z_sqr := (post_incentive_price_w_z)^2]

# Regression --------------------------------------------------------------
set_control <- paste("median_home_value_z + median_household_income_z + mean_density_z +", "mean_size_z + I(mean_size_z^2) + mean_rebate_z")
dep_var = "demand_zip_code"
  
iv_formula_str <- glue(
  "{dep_var} ~ {set_control} | 
   year + county | 
   post_incentive_price_w_z + post_incentive_price_w_z_sqr ~ tariff")

demand <- feols(as.formula(iv_formula_str),
                             cluster = ~zip_code, data = short)
fitstat(demand, type = c("ivf", "cd", "kpr", "wh", "sargan"))
