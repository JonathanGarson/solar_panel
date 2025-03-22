# This code generates clean zip county files

library(arrow)
library(data.table)
library(DescTools)
library(readxl)

# Data --------------------------------------------------------------------

zip_county = as.data.table(read_excel(data_raw("FIPS/ZIP_COUNTY_122023.xlsx")))
zip_county = as.data.table(fread(data_raw("FIPS/ZIP-COUNTY-FIPS_2017-06.csv")))

# Cleaning ----------------------------------------------------------------
# Changing the names
old_cols_names = colnames(zip_county)
new_cols_names = tolower(colnames(zip_county))
setnames(zip_county, old = old_cols_names, new = new_cols_names)

# setting to lower cases and removing white spaces
zip_county[, county := tolower(trimws(county))]
zip_county[, city := tolower(trimws(usps_zip_pref_city))]
zip_county[, state := tolower(trimws(usps_zip_pref_state))]

# ZIP code do not necessarily match county, so we propose to attribute to a county the ZIP codes 
# with the highest number of attribution to a specific county.
zip_county = unique(zip_county[, .SD, .SDcols = c("zip", "city", "county", "state")])
zip_county_ranked = zip_county[, .N, by = .(city, zip)] # Count occurrences of each ZIP code per city
zip_county_most_common = zip_county_ranked[order(city, -N), .SD[1], by = city] # Keep only the ZIP code with the highest occurrence per city
zip_county_final = merge(zip_county_most_common, zip_county, by = c("city", "zip"), all.x = TRUE) # Merge back with the original `zip_county` to retrieve other columns
zip_county_final[, N := NULL]

# Exporting ---------------------------------------------------------------
write_parquet(zip_county_unique,data_temp("zip_county_clean.parquet"))
