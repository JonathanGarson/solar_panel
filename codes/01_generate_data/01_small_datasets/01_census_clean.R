# Cleaning Census Data

library(data.table)

# Data --------------------------------------------------------------------

# census_educ = fread(data_raw("census/education/acs2015_education.csv"), header = TRUE)
census_occup = fread(data_raw("census/occupation_2015/acs2015_occupation.csv"), header = TRUE)
census_educ = fread(data_raw("census/education_2010/acs2010_education.csv"), header = TRUE)
census_md = fread(data_raw("census/education/acs2015_education_metadata.csv"), header = TRUE)

# Clean -------------------------------------------------------------------

# Education

educ_column = c(
  "GEO_ID",
  "NAME",
  "S1501_C02_014E",
  "S1501_C02_014M",
  "S1501_C02_015E",
  "S1501_C02_015M"
)

census_educ = census_educ[, ..educ_column]

educ_column_name = c(
  "GEOD_ID" = "geo_id",
  "NAME" = "geo_area",
  "S1501_C02_014E" = "pct_high_school_estimate",
  "S1501_C02_014M" = "pct_high_school_sd",
  "S1501_C02_015E" = "pct_bachelor_estimate",
  "S1501_C02_015M" = "pct_bachelor_estimate_sd"
)

setnames(census_educ, colnames(census_educ), educ_column_name)
census_educ = census_educ[2:.N,]

census_educ[, c("tract_code", "county", "state") := tstrsplit(
  gsub("Census Tract ", "", geo_area), ",\\s*"
)]

census_educ[, c("tract_code", "county", "state") := lapply(.SD, trimws), .SDcols = c("tract_code", "county", "state")]
census_educ = census_educ[, .(geo_id, tract_code, county, state, pct_high_school_estimate, pct_high_school_sd,pct_bachelor_estimate,pct_bachelor_estimate_sd)]
census_educ = unique(census_educ)

# Occupation

col_occup = c(
  "GEO_ID",
  "NAME",
  "B25013_001E",
  "B25013_001M",
  "B25013_002E",
  "B25013_002M"
)
census_occup = census_occup[, ..col_occup]

col_rename_occupation = c(
  "GEO_ID" = "geo_id",
  "NAME" = "geo_area",
  "B25013_001E" = "total",
  "B25013_001M" = "total_sd",
  "B25013_002E" = "ow_occupied_housing",
  "B25013_002M" = "ow_occupied_housing_sd"
)

setnames(census_occup, colnames(census_occup), col_rename_occupation)
census_occup = census_occup[2:.N,]

census_occup[, c("tract_code", "county", "state") := tstrsplit(
  gsub("Census Tract ", "", geo_area), ",\\s*"
)]
census_occup[, c("tract_code", "county", "state") := lapply(.SD, trimws), .SDcols = c("tract_code", "county", "state")]
census_occup = census_occup[, .(geo_id, tract_code, county, state, total, total_sd, ow_occupied_housing, ow_occupied_housing_sd)]
census_occup = unique(census_occup)

# Merging -----------------------------------------------------------------

census = merge(census_educ,census_occup, by = "geo_id")
census[, c("county.y", "state.y", "tract_code.y") := NULL]
setnames(census, c("county.x", "state.x", "tract_code.x"), c(c("county", "state", "tract_code")))
census[, geo_id := sub(".*US", "", geo_id)]
exclude_cols <- c("geo_id", "tract_code", "county", "state")
cols_to_convert <- setdiff(colnames(census), exclude_cols)
census[, (cols_to_convert) := lapply(.SD, as.numeric), .SDcols = cols_to_convert]

# Exporting ---------------------------------------------------------------
fwrite(census, data_temp("census_demographic.csv"))

