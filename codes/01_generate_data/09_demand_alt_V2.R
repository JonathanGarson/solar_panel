library(arrow)
library(data.table)
library(zoo)
library(zipcodeR)

# Data --------------------------------------------------------------------

dt = read_parquet(data_final("tts_final.parquet"))

# Identify Top Panels -----------------------------------------------------

# Identify top 10 most sold panel models per year
top_panels_by_year <- dt[, .N, by = .(module_model, year)][order(year, -N), .SD[1:10], by = year]

# Mark installations using top panels
dt[, high_quality := as.integer(module_model %in% top_panels_by_year$module_model)]

# Define demand and controls by quality segment
obs_quality <- dt[, .(
  demand = .N,
  demand_intensive = sum(module_quantity, na.rm = TRUE),
  price_w = mean(price_w, na.rm = TRUE),
  elec_price = mean(elec_price, na.rm = TRUE),
  rebate_w = mean(rebate_w, na.rm = TRUE),
  mean_week_wage = mean(mean_week_wage, na.rm = TRUE),
  educ = mean(pct_bachelor_estimate, na.rm = TRUE),
  population = mean(population_density, na.rm = TRUE),
  population_density = mean(population_density, na.rm = TRUE),
  median_home_value = mean(median_home_value, na.rm = TRUE),
  PV_system_size_DC = mean(PV_system_size_DC, na.rm = TRUE)
), by = .(zip_code, year, high_quality)]

zip_codes <- unique(dt$zip_code)
years <- seq(min(dt$year), max(dt$year))
quality_group <- c(0, 1)

panel <- CJ(zip_code = zip_codes, year = years, high_quality = quality_group)

dt_panel_quality <- merge(panel, obs_quality, by = c("zip_code", "year", "high_quality"), all.x = TRUE)

# Merge with county
county_lookup <- unique(dt[, .(county, zip_code)])
dt_panel_quality <- merge(dt_panel_quality, county_lookup, by = "zip_code")

# Interpolate missing controls by county & quality
cols_to_interpolate <- c("price_w", "elec_price", "rebate_w", "mean_week_wage", "educ",
                         "population", "population_density", "median_home_value", "PV_system_size_DC")

setorderv(dt_panel_quality, c("zip_code", "high_quality", "year"))

dt_panel_quality[
  , (cols_to_interpolate) := lapply(.SD, function(col)
    na.approx(x = year, object = col, na.rm = FALSE, rule = 2)
  ),
  by = .(county, high_quality),
  .SDcols = cols_to_interpolate
]

# Assign average tariff per quality group (high vs. low)
tariff_quality <- dt[, .(tariff = mean(tariff, na.rm = TRUE)), by = .(year, high_quality)]
dt_panel_quality <- merge(dt_panel_quality, tariff_quality, by = c("year", "high_quality"), all.x = TRUE)


iv_model <- feols(
  demand ~ elec_price + rebate_w + educ + population_density + median_home_value + PV_system_size_DC   
  | zip_code + year + origin
  | log(price_w) ~ tariff, 
  data = dt_panel_quality
)
summary(iv_model, stage = 1:2)
