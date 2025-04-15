# This code generates the demand data set

library(arrow)
library(data.table)

# Data --------------------------------------------------------------------

demand_tts = read_parquet(data_final("tts_final.parquet"))

# We start by creating a cartesian join of all zip code and data
zip_code = unique(demand_tts$zip_code)
full_date_range <- seq(min(demand_tts$year), max(demand_tts$year))
complete_panel = CJ(zip_code = zip_code, year = full_date_range)

# WE MUST REORGANISE IN THE FOLLOWING WAY : ZIP YEAR COUNTY DEMAND COVARIATES -- AGGREGATION IS NEEDED

demand_agg = unique(demand_tts[, .(
  demand_extensive = .N,
  demand_intensive = sum(module_quantity, na.rm = TRUE),
  price_w = mean(price_w, na.rm = TRUE),
  elec_price = mean(elec_price, na.rm = TRUE),
  rebate_w = mean(rebate_w, na.rm = TRUE),
  mean_week_wage = mean(mean_week_wage, na.rm = TRUE),
  educ = mean(pct_bachelor_estimate, na.rm = TRUE),
  population_density = mean(population_density, na.rm = TRUE),
  median_home_value = mean(median_home_value, na.rm = TRUE)
  ),
  by = .(zip_code, year)])

# We merge the two
demand_full <- merge(complete_panel, demand_agg, by = c("zip_code", "year"), all.x = TRUE)
demand_full[is.na(demand_extensive), demand_extensive := 0]

