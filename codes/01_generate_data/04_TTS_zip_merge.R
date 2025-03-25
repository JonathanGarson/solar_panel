# This code merge the TTS clean data file with the zipcode file augmented of the real estate value of 2010

library(arrow)
library(data.table)

# Data --------------------------------------------------------------------

tts_clean = read_parquet(data_temp("TTS_clean.parquet"))
zip_code = fread(data_temp("zip_county_data.csv"))

# Merging -----------------------------------------------------------------

zip_code = zip_code[, zipcode := as.character(zipcode)]
zip_code = unique(zip_code)
tts_clean = merge(tts_clean, zip_code, by.x = "zip_code", by.y = "zipcode", all.x = TRUE)

write_parquet(tts_clean, data_temp("TTS_merged.parquet"))
