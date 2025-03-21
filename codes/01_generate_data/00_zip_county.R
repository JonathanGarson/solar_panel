# This code use the ZIP code from the TTS files to link it to a county

library(arrow)
library(data.table)
library(zipcodeR)

# Data --------------------------------------------------------------------

tts_clean = read_parquet(data_temp("TTS_clean.parquet"), col_select = c("zip_code"))

# Matching ZIP code to county ---------------------------------------------

uniqueN(tts_clean$zip_code)
zip_to_county = reverse_zipcode(tts_clean$zip_code)
setDT(zip_to_county)

keep = c("zipcode", "major_city", "county", "population", "population_density", "land_area_in_sqmi", "median_home_value","median_household_income")
zip_to_county = zip_to_county[, .SD, .SDcols = keep]

tracts_list <- lapply(zip_to_county$zipcode, function(z) {
  tryCatch({
    get_tracts(z)
  }, error = function(e) {
    message(sprintf("Skipping ZIP code %s: %s", z, e$message))
    return(NULL)
  })
})

tracts_dt <- rbindlist(tracts_list, fill = TRUE)
setDT(zip_to_county)

zip_to_county = merge(zip_to_county, tracts_dt, by.x = "zipcode", by.y = "ZCTA5")

fwrite(zip_to_county, data_temp("zip_county_data.csv"))
