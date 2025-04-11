# We reorganise our data in panel manufacturer installation installer

library(arrow)
library(data.table)
library(fixest)

# Data --------------------------------------------------------------------

tts = read_parquet(data_final("tts_final.parquet"))

# Panel -------------------------------------------------------------------
names(tts)
cols_to_keep <- c("state", "zip_code", "year", "year_quarter", "module_manufacturer", "installer_name",
                  "PV_system_size_DC", "total_installed_price", "rebate_or_grant",
                  "new_construction", "ground_mounted" , "module_quantity",
                  "price_w", "rebate_w", "county", "population", "population_density", "land_area_in_sqmi",
                  "median_home_value", "median_household_income", "market_share_period", "china", "korea",
                  "usa", "norway", "germany", "japan", "premium_panel_overall", "premium_panel_ad1",
                  "premium_panel_ad2", "premium_panel_st", "premium_installation", "tariff", "elec_price",
                  "mean_price_year", "tot_emp", "jobs_1000", "h_mean", "h_median", "a_mean", "a_median")

tts = tts[, ..cols_to_keep]
tts[, group_id := .GRP, by = .(installer_name, module_manufacturer, zip_code)]

tts