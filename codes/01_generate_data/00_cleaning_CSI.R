# We clean the CSI data to collect the tier rate

library(arrow)
library(data.table)
library(glue)
library(zoo)

# Data --------------------------------------------------------------------
csi = setDT(read_parquet(data_raw("csi/csi_raw.parquet")))

# Cleaning colnames
col = tolower(colnames(csi))
col = gsub(pattern = "\\.", replacement = "_", x = col, perl = TRUE)
setnames(csi, colnames(csi), col)

# Time Column
csi[, install_date := as.Date(app_complete_date)]
csi[, `:=`(
  year = year(install_date),
  year_quarter = paste0(year(install_date), "Q", quarter(install_date)),
  year_week = paste0(year(install_date), "_W", sprintf("%02d", isoweek(install_date)))
)]

utility_mapping = unique(csi[, .(service_zip,year,utility)])
utility_mapping = na.omit(utility_mapping, cols = c("service_zip", "year"))
setnames(utility_mapping, "service_zip", "zip_code")
utility_mapping[, zip_code := as.character(zip_code)]

# Utility
csi = csi[utility != ""]

# Only keep year between 2010 and 2020
csi = csi[year %in% 2010:2020]

# Get rid of multistage system
csi[, previous_application := fcase(
  previous_application == "Yes", 1,
  previous_application == "", 0,
  default = 0
)]
csi = csi[previous_application == 0]

# We trim for too big system size
csi = csi[system_size_dc < 20]

# Residential Data Only
csi = csi[customer_sector == "Residential"]

# No TPO
csi = csi[third_party_owned == "No"]

# No price refered
csi = csi[!is.na(total_system_cost),]

# Only Solar Panel
csi = csi[technology_type == "Photovoltaic"]

# Price smaller than ITC
csi = csi[total_system_cost > itc_cost_basis]

# Get rid of somah program
csi = csi[interconnection_program != "SOMAH"]

# Getting rid of useless colnames
all_cols <- colnames(csi)
has_number <- grepl("(\\d+)$", all_cols)
col_with_number <- all_cols[has_number]
col_without_number <- all_cols[!has_number]
numbers <- as.integer(sub(".*[._](\\d+)$", "\\1", col_with_number))
col_with_small_number <- col_with_number[numbers < 4]

col_keep = c(col_without_number, col_with_small_number)
csi = csi[, ..col_keep]

# Create Var --------------------------------------------------------------
csi[, price_w := total_system_cost/(system_size_dc*1000)]
csi[, .N, by = year]

# Export
cols <- c(
  "service_zip", "service_county", "service_city", "install_date", "year", "year_quarter", "year_week", "utility",
  "system_size_dc", "mounting_method", "app_received_date", "app_complete_date", "self_installer", "installer_name",
  "total_system_cost", "itc_cost_basis", "nem_tariff",
  "generator_model_1", "generator_manufacturer_1", "generator_quantity_1",
  "generator_model_2", "generator_manufacturer_2", "generator_quantity_2",
  "generator_model_3", "generator_manufacturer_3", "generator_quantity_3",
  "inverter_model_1", "inverter_manufacturer_1", "inverter_quantity_1",
  "inverter_model_2", "inverter_manufacturer_2", "inverter_quantity_2",
  "inverter_model_3", "inverter_manufacturer_3", "inverter_quantity_3"
)
csi = csi[, ..cols]

fwrite(utility_mapping, data_temp("utility_mapping_california.csv"))
write_parquet(csi, data_final("csi_clean.parquet"))

