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
tts[, ln_price_w := log(price_w)]
tts[, ln_tariff := log(tariff)]
tts[, ln_post_incentive_price_w := log(post_incentive_price_w)]

set_control = c("median_home_value + median_household_income + population_density + PV_system_size_DC + PV_system_size_DC^2 + 
                elec_price + h_median + rebate_w")
formula = as.formula(glue("{quality_var} + {set_control}"))

# WEIRD
pass_through = feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_overall+ {set_control}| year + state + installer_name")) , 
                     cluster = ~zip_code , data = tts[year %in% 2010:2016])
pass_through_2 = feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_overall+ {set_control}| year + state + installer_name + module_manufacturer")) , 
                     cluster = ~zip_code , data = tts)
# CONSISTENT
pass_through_ad_1 = feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_ad1+ {set_control}| year + state + installer_name")) , 
                     cluster = ~zip_code , data = tts[year %in% 2010:2013])
pass_through_2_ad1 = feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_ad1+ {set_control}| year + state + installer_name + module_manufacturer")) , 
                     cluster = ~zip_code , data = tts[year %in% 2010:2013])

pass_through_ad_2 = feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_ad1+ {set_control}| year + state + installer_name")) , 
                     cluster = ~zip_code , data = tts[year %in% 2013:2016])
pass_through_2_ad2 = feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_ad1+ {set_control}| year + state + installer_name + module_manufacturer")) , 
                     cluster = ~zip_code , data = tts[year %in% 2013:2016])

# Effect on quality change after implementation ---------------------------
quality_ad_1 = feols(as.formula(glue("premium_panel_ad1 ~ ln_tariff + {set_control}| year + state + installer_name")) , 
                     cluster = ~zip_code , data = tts[year %in% 2010:2013])
quality_2_ad1 = feols(as.formula(glue("premium_panel_ad1 ~ ln_tariff + {set_control}| year + state + installer_name + module_manufacturer")) , 
                           cluster = ~zip_code , data = tts[year %in% 2010:2013])

quality_ad_2 = feols(as.formula(glue("premium_panel_ad2 ~ ln_tariff + {set_control}| year + state + installer_name")) , 
                          cluster = ~zip_code , data = tts[year %in% 2013:2016])
quality_2_ad2 = feols(as.formula(glue("premium_panel_ad2 ~ ln_tariff + {set_control}| year + state + installer_name + module_manufacturer")) , 
                           cluster = ~zip_code , data = tts[year %in% 2013:2016])


