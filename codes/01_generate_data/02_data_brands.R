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
ts_sales = ts_sales[, .(brand_sales_year = sum(module_quantity, na.rm = TRUE)), by = .(year, manufacturer)]
ts_sales = ts_sales[brand_sales_year >= 3000]

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
ts_model = ts_model[, .(brand_model_year = uniqueN(model)), by = .(year, manufacturer)]

# Average efficiency by Brand by year -------------------------------------

ts_eff_1 = unique(ts[year %in% 2002:2023, .(avg_eff_model_1 = mean(efficiency_module_1)), by = .(module_manufacturer_1, year)])
setnames(ts_eff_1, c("module_manufacturer_1", "avg_eff_model_1"), c("manufacturer", "avg_eff"))

# Merge and Export data ------------------------------------------------------------

ts_brands = merge(ts_sales, ts_model, by = c("manufacturer", "year"))
ts_brands = merge(ts_sales, ts_eff_1, by = c("manufacturer", "year"))

write_parquet(ts_brands, data_final("TTS_brands.parquet"))

