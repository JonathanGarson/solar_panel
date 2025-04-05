# This is a reproduction of TTS data on quality

library(arrow)
library(data.table)
library(fixest)
library(lubridate)
library(zoo)

# Data --------------------------------------------------------------------

tts = setDT(read_parquet(data_raw("TTS.parquet")))
cpi = fread(data_raw("us_cpi.csv"))

# Cleaning ----------------------------------------------------------------
# We follow as close as possible the Tracking the Sun appendix to reproduce their result from our data
tts[, install_date_fmt := dmy(installation_date)]
tts[, year := year(install_date_fmt)]
tts[, year_quarter := as.yearqtr(install_date_fmt, format('%Y-%m-%d'))]
tts = tts[year == 2018  & third_party_owned == 0]

clean_column <- c("module_manufacturer_1", "module_manufacturer_2", "module_manufacturer_3", "installer_name",
                  "total_installed_price", "PV_system_size_DC", "installer_name")

for (col in clean_column) {
  tts[get(col) == "Tesla Energy", (col) := NA]
  tts[get(col) == "-1", (col) := NA]
}

# Excluding too small systems
tts = tts[PV_system_size_DC < 20]

# Deflating 2018 $ value
month = setdiff(colnames(cpi), c("Year", "HALF1", "HALF2"))
cpi[, year_cpi := rowMeans(.SD), .SDcols = month]
base_cpi = cpi[Year == 2018,]$year_cpi
cpi = cpi[Year %in% 2010:2023, deflated_cpi := year_cpi/base_cpi]
tts = merge(tts, cpi[, .(Year, deflated_cpi)], by.x = "year", by.y = "Year")
tts[, total_installed_price := total_installed_price/deflated_cpi]
tts[, rebate_or_grant := rebate_or_grant/deflated_cpi]

# Excluding data entry error
tts[, price_w := total_installed_price/(PV_system_size_DC*1000)]
tts[, rebate_w := rebate_or_grant/(PV_system_size_DC*1000)]
tts = tts[price_w > rebate_w,]
tts = tts[price_w > 1 & price_w < 20]
tts = tts[technology_type == "pv-only"]

tts[, system_ID_1 := ifelse(system_ID_1 == "-1", NA, system_ID_1)]
tts = na.omit(tts, cols = c("system_ID_1", "installer_name"))

# Building Key variable ---------------------------------------------------
# Premium
efficiency_col = c("efficiency_module_1","efficiency_module_2","efficiency_module_3")

tts[, premium_1 := ifelse(efficiency_module_1 >= 0.20,1, 0)]
tts[, premium_2 := ifelse(efficiency_module_2 >= 0.20,1, 0)]
tts[, premium_3 := ifelse(efficiency_module_3 >= 0.20,1, 0)]

tts[, micro_inverter_1 := fcase(micro_inverter_1 == "Y", 1, 
                                micro_inverter_1 == "N", 0,
                                default = NA)]
# tts[, micro_inverter_2 := ifelse(micro_inverter_2 == "Y", 1, 0)]
# tts[, micro_inverter_3 := ifelse(micro_inverter_3 == "Y", 1, 0)]

tts[, ground_mounted := fcase(ground_mounted == "1", 1, 
                               ground_mounted == "0", 0,
                               default = NA)]
tts[, new_construction := fcase(new_construction == "1", 1,
                                new_construction == "0", 0,
                                default = NA)]

zip_code = fread(data_temp("zip_county_data.csv"))
zip_code = zip_code[, zipcode := as.character(zipcode)]
zip_code = unique(zip_code)
tts = merge(tts, zip_code, by.x = "zip_code", by.y = "zipcode", all.x = TRUE)

installer_counts <- tts[, .(installs_by_installer = .N), by = .(county, installer_name, year)]
zip_totals <- tts[, .(total_installs_zip = .N), by = .(county, year)]
market_share <- merge(installer_counts, zip_totals, by = c('county', 'year'))
market_share[, market_share_installer := installs_by_installer / total_installs_zip]
market_share[, hhi_index_c := sum(market_share_installer^2), by = .(county, year)]
tts <- merge(tts, market_share[, .(county, installer_name, hhi_index_c, year)],
             by = c("county", "installer_name", "year"), all.x = TRUE)
tts[, market_size := .N, by = .(county, year)]
tts[, hhi_index_c_sqr := hhi_index_c^2]

# Regression --------------------------------------------------------------
tts[, log_price_w := log(price_w)]
tts[, PV_sqr := (PV_system_size_DC)^2]

# reproduction_1 = feols(price_w ~  premium_1 | state + year_quarter + installer_name , cluster = ~zip_code, data = tts)
# reproduction_2 = feols(price_w ~  premium_1 + micro_inverter_1 + DC_optimizer + PV_system_size_DC + new_construction + ground_mounted
#                        + PV_sqr| state + year_quarter + installer_name + module_manufacturer_1, cluster = "hetero", data = tts)

tts_clean = tts[, .SD, .SDcols = c("price_w", "PV_system_size_DC", "premium_1", "micro_inverter_1", "DC_optimizer", 
                                   "ground_mounted", "year_quarter", "installer_name","zip_code", "module_manufacturer_1",
                                   "state", "installer_name", "new_construction")]
  
rep = feols(price_w ~ PV_system_size_DC + PV_system_size_DC^2 + premium_1 + micro_inverter_1 + DC_optimizer + ground_mounted + market_size + hhi_index_c + hhi_index_c_sqr
            | state + year_quarter + installer_name + module_manufacturer_1, cluster = ~zip_code, data = tts)

sub = model.frame(reproduction)
