# This data set generates the final dataset we use for TTS data.

library(arrow)
library(data.table)

# Data --------------------------------------------------------------------

tts = read_parquet(data_temp("TTS_clean_names.parquet"))

# Data Selection ----------------------------------------------------------

# Low income household are hard to identify maybe for the amount of subsidy

# Get rid of likely misspecification like solar panel being more than 100 subsidied
# A lot of this value are misreported rebated before 2015 in California, I wondered if it is not the result of misreported overlap between program
# tts = tts[total_installed_price > rebate_or_grant, ]

# Customer segment we are interested in
tts = tts[customer_segment %in% c("RES"),]

# Amount of double system ID - 
# Duplicated sytem ID 2 corresponds to extension of previous system it is desirable to keep them 
# Since price information are system independent for system ID 1

# System expansion change the value of the system
tts[expansion_system == -1, expansion_system := NA]
# Not an issue since they all have an ID

# Clean the data for module manufacturer discrepency
tts[module_manufacturer_1 == "alps technology", module_manufacturer_1 := NA_character_]
tts[module_manufacturer_2 == "alps technology", module_manufacturer_2 := NA_character_]
tts[module_manufacturer_3 == "alps technology", module_manufacturer_3 := NA_character_]

tts[, nb_manufacturer := fcase(
  !is.na(module_manufacturer_1) & is.na(module_manufacturer_2), "one manufacturer : 1",
  is.na(module_manufacturer_1) & !is.na(module_manufacturer_2), "one manufacturer : 2",
  !is.na(module_manufacturer_1) & !is.na(module_manufacturer_2), "two manufacturers",
  default = "other")]
tts[, .N, by = nb_manufacturer]
tts = tts[nb_manufacturer == "one manufacturer : 1",]

tts[, system_manufacturing_situation := fcase(
  module_manufacturer_1 == module_manufacturer_2, "unique",
  module_manufacturer_1 != module_manufacturer_2, "different",
  default = "other")]
tts[, .N, by = system_manufacturing_situation]
tts = tts[additional_modules != 1,]

# Clean out of battery pack up (pollute price evaluation and chosen independently of solar panels brand)
# tts = tts[technology_type == "pv-only",]

# Reduce to our period 2010-2020
tts = tts[year %in% c(2010:2020)]

# Clean ZIP code of more than 5 letter ZIP code
tts = tts[nchar(zip_code) <= 5,]

# We get rid of column now being useless
column_to_suppress = setdiff(grep(pattern = "_2$|_3$", colnames(tts), value = TRUE), c("inverter_manufacturer_2", "inverter_model_2", "inverter_quantity_2",
                                                                  "inverter_manufacturer_3", "inverter_model_3","inverter_quantity_3",
                                                                  "micro_inverter_2", "micro_inverter_3","built_in_meter_inverter_2",
                                                                  "built_in_meter_inverter_3","output_capacity_inverter_2","output_capacity_inverter_3"))
tts = tts[, .SD, .SDcols = setdiff(colnames(tts), c(column_to_suppress, "azimuth_1","tilt_1","additional_modules"))]

toclean_colnames = setdiff(grep(pattern = "_1$", colnames(tts), value = TRUE), c("inverter_manufacturer_1","inverter_model_1","inverter_quantity_1","micro_inverter_1", 
                                                                               "built_in_meter_inverter_1", "output_capacity_inverter_1"))
clean_colnames = c("data_provider","system_ID","module_manufacturer","module_model","module_quantity","technology_module","BIPV_module","bifacial_module"
                   ,"nameplate_capacity_module","efficiency_module")
setnames(tts, toclean_colnames, clean_colnames)

# We also get rid of Tesla and Solar city, even for HO system, following the recommendation of the LNBL
tts = tts[module_manufacturer != "tesla"]
tts = tts[installer_name != "Tesla Energy"]
tts = tts[installer_name != "SolarCity"]

# Conserving smaller than 10kW size system, as bigger model can be of double use
tts = tts[PV_system_size_DC <= 20,]

# Exporting
write_parquet(tts, data_temp("TTS_clean.parquet"))
