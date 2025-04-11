# This code merge the TTS clean data file with the zipcode file augmented of the real estate value of 2010

library(arrow)
library(data.table)

# Data --------------------------------------------------------------------

tts_clean = read_parquet(data_temp("TTS_clean.parquet"))
zip_code = fread(data_temp("zip_county_data.csv"))
census = fread(data_temp("census_demographic.csv"))

# Merging -----------------------------------------------------------------

zip_code = zip_code[, zipcode := as.character(zipcode)]
zip_code = unique(zip_code)
uniqueN(zip_code)
uniqueN(tts_clean$zip_code)
zip_code = merge(zip_code, census, by.x = "geoid", by.y = "geo_id")
# zip_code = merge(zip_code, census, by.x = "tract", by.y = "tract_code")

tts_clean = merge(tts_clean, zip_code, by.x = "zip_code", by.y = "zipcode", all.x = TRUE)

# Remove columns with suffixes from a merge (e.g. "_x", "_y")
to_suppress = c("county.y", "state.y")
tts_clean[, (to_suppress) := NULL]
setnames(tts_clean,  c("county.x", "state.x"), c("county", "state"))

write_parquet(tts_clean, data_temp("TTS_merged.parquet"))
