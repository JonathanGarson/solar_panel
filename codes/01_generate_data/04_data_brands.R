# Produce the dataset on brands, number of commercialized products, average efficiency per year and solar panels products.
# We produce a dataset on a year - brand model

rm(ts)
gc()

library(arrow)
library(data.table)

# Load Data ---------------------------------------------------------------

ts = read_parquet(data_temp("TTS_clean_names.parquet"))

# Sales per Year ------------------------------------------------------------
#convert to double module quantity

ts$module_quantity_1 = as.numeric(ts$module_quantity_1)
ts$module_quantity_2 = as.numeric(ts$module_quantity_2)
ts$module_quantity_3 = as.numeric(ts$module_quantity_3)

ts_sales = melt(ts,
               measure.vars = list(
                 module_quantity = c("module_quantity_1", "module_quantity_2", "module_quantity_3"),
                 manufacturer = c("module_manufacturer_1", "module_manufacturer_2", "module_manufacturer_3")
               ))
ts_sales[, manufacturer := fifelse(manufacturer == "-1", NA_character_, manufacturer)]
ts_sales = ts_sales[!is.na(manufacturer) & !is.na(module_quantity)]

# Aggregate sales by year and manufacturer
ts_sales_year = ts_sales[, .(brand_sales_year = sum(module_quantity, na.rm = TRUE)), by = .(year, manufacturer)]
ts_sales_year = ts_sales_year[brand_sales_year >= 3000]
ts_sales_quarter = ts_sales[, .(brand_sales_quarter = sum(module_quantity, na.rm = TRUE)), by = .(year_quarter, manufacturer)]
ts_sales_quarter[, year := as.numeric(substr(year_quarter, 1,4))]

# Number of model commercialized per year ---------------------------------
# we consider the amount of commercialized model on the market by each brand, not the introduction of new brands
ts_model = melt(ts, 
                measure.vars = list(
                  model = c("module_model_1","module_model_2","module_model_3"),
                  manufacturer = c("module_manufacturer_1", "module_manufacturer_2", "module_manufacturer_3")
                ))
ts_model[, manufacturer := fifelse(manufacturer == "-1", NA_character_, manufacturer)]
ts_model = ts_model[!is.na(manufacturer) & !is.na(model)]

# Aggregate sales by year and manufacturer
ts_model_year = ts_model[, .(brand_model_year = uniqueN(model)), by = .(year, manufacturer)]
ts_model_quarter = ts_model[, .(brand_model_quarter = uniqueN(model, na.rm = TRUE)), by = .(year_quarter, manufacturer)]
ts_model_quarter[, year := as.numeric(substr(year_quarter, 1,4))]

# Average efficiency by Brand by year -------------------------------------

ts_eff_1_year = unique(ts[year %in% 2002:2023, .(avg_eff_model_1 = mean(efficiency_module_1)), by = .(module_manufacturer_1, year)])
ts_eff_1_quarter = unique(ts[year %in% 2002:2023, .(avg_eff_model_1 = mean(efficiency_module_1)), by = .(module_manufacturer_1, year_quarter)])
setnames(ts_eff_1_year, c("module_manufacturer_1", "avg_eff_model_1"), c("manufacturer", "avg_eff_year"))
setnames(ts_eff_1_quarter, c("module_manufacturer_1", "avg_eff_model_1"), c("manufacturer", "avg_eff_quarter"))

# Merge and Export data ------------------------------------------------------------

ts_brands_year = merge(ts_sales_year, ts_model_year, by = c("manufacturer", "year"))
ts_brands_year = merge(ts_sales_year, ts_eff_1_year, by = c("manufacturer", "year"))

ts_brands_quarter= merge(ts_sales_quarter, ts_model_quarter, by = c("manufacturer", "year_quarter"))
ts_brands_quarter= merge(ts_sales_quarter, ts_eff_1_quarter, by = c("manufacturer", "year_quarter"))

write_parquet(ts_brands_year, data_final("TTS_brands_year.parquet"))
write_parquet(ts_brands_quarter, data_final("TTS_brands_quarter.parquet"))

