# This code evaluate the subsidy pass-through

library(arrow)
library(data.table)
library(fixest)
library(modelsummary)

# Data --------------------------------------------------------------------

tts= read_parquet(data_final("TTS_final.parquet"))

# The effect of subsidy on price ------------------------------------------

tts[, post_incentive_price_w := 0.7*(price_w - rebate_w)]
tts[, ln_rebate_w := log(rebate_w)]
tts[, ln_post_incentive_price_w := log(post_incentive_price_w)]

rebate_pt = feols(ln_post_incentive_price_w ~ rebate_w*premium_panel_overall + rebate_w*premium_installation + median_home_value + median_household_income + population_density
                  | year + state + module_manufacturer + installer_name + utility_service_territory, cluster = ~zip_code, data = tts)
fitstat(rebate_pt, type = c("f", "wald"))

pass_through_ad1 = feols(post_incentive_price_w ~ tariff_2012_treated*china + tariff_2012_treated*tariff_2012*premium_panel_overall + rebate_w + median_home_value + median_household_income + population_density
                  | year + state + module_manufacturer + installer_name, cluster = ~zip_code, data = tts)
fitstat(pass_through_ad1, type = c("f", "wald"))



