# This dataset propose a quick cleaning method for exploratory purpose only

library(arrow)
library(data.table)
library(ggplot2)

# Load Data ---------------------------------------------------------------

ts = read_parquet(data_raw("TTS.parquet"))

# Data Cleaning -----------------------------------------------------------
#getting rid of variables concerning the orientation of the solar panel
cons_column = setdiff(colnames(ts), c("azimuth_1","azimuth_2","azimuth_3","tilt_1","tilt_2","tilt_3"))
ts[, .SD, .SDcols = cons_column]

#getting rid of observations with no information for: installer, zip, manafucturer, module model, customer segment 
string_filter = c("installation_date", "zip_code","installer_name", "module_manufacturer_1", "module_model_1", "technology_module_1")
num_filter = c("efficiency_module_1", "name_plate_capacity_module_1")

ts_f = ts[apply(ts[, ..string_filter] != "-1", 1, all)]
ts_f = ts[apply(ts[, ..string_filter] != -1, 1, all)]
ts_f = ts_f[installation_date != "NaT",]

#adding a year column
ts_f[, year := as.numeric(substr(installation_date, 8, 11))]
 
unique(ts_f$module_manufacturer_1)
#list of manufacturer to merge


