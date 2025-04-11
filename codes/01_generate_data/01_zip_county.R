# This code use the ZIP code from the TTS files to link it to a county and we also match it with census tract level data

library(arrow)
library(data.table)
library(dplyr)
library(zipcodeR)

# Data --------------------------------------------------------------------

tts_clean = read_parquet(data_temp("TTS_clean.parquet"), col_select = c("zip_code","state"))

# Matching ZIP code to county ---------------------------------------------
tts_clean = tts_clean[state == "ca"]
uniqueN(tts_clean$zip_code)
zip_to_county = reverse_zipcode(tts_clean$zip_code)
setDT(zip_to_county)

keep = c("zipcode", "major_city", "county", "population", "population_density", "land_area_in_sqmi", "median_home_value","median_household_income")
zip_to_county = zip_to_county[, .SD, .SDcols = keep]
uniqueN(zip_to_county$zipcode)

zcta_db = zcta_crosswalk
setDT(zcta_db)
zcta_main_tract <- zcta_db[, .SD[1], by = ZCTA5]

zip_to_county = merge(zip_to_county, zcta_main_tract, by.y = "ZCTA5", by.x = "zipcode")
uniqueN(zip_to_county$county)
uniqueN(zip_to_county$TRACT)

col = colnames(zip_to_county)
setnames(zip_to_county, col, tolower(col))

fwrite(zip_to_county, data_temp("zip_county_data.csv"))
