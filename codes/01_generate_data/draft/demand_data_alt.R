library(arrow)
library(data.table)
library(lubridate)
library(stringr)
library(zoo)
library(zipcodeR)

# Data --------------------------------------------------------------------

dt = setDT(read_parquet(data_final("tts_final.parquet")))

# Demand ------------------------------------------------------------------
# 1. We create a panel of zip_code, year, origin
zip_code = unique(dt$zip_code)
dt[, year_quarter_num := as.numeric(str_sub(year_quarter, 1, 4)) + 
     0.25 * (as.numeric(str_sub(year_quarter, 7, 7)) - 1)]
year = seq(min(dt$year_quarter), max(dt$year_quarter))
origin = unique(dt$origin)

dt_panel = CJ(zip_code, year, origin)

# 2. We obtain the tariff at the zip_code level
tariff_origin = dt[, .(tariff = mean(tariff)), by = .(origin, year)]
dt_panel = merge(dt_panel, tariff_origin, by = c("origin", "year"), all.x = TRUE)

# 3. Create observables for control at the zip_code level
obs = dt[, .(
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
), by = .(zip_code, year, origin)]

dt_panel = merge(dt_panel, obs, by = c("zip_code", "year", "origin"), all.x = TRUE)

# Cleaning Missing values
dt_panel[, demand := ifelse(is.na(demand), 0, demand)]
dt_panel[, demand_intensive := ifelse(is.na(demand_intensive), 0, demand_intensive)]

# We merge with county id for interpolation
county = unique(dt[, .(county, zip_code)])
dt_panel = merge(dt_panel, county, by = "zip_code")

cols_to_interpolate = c("price_w","elec_price","rebate_w","mean_week_wage","educ","population",
                        "population_density", "median_home_value","PV_system_size_DC")

setorderv(dt_panel, c("zip_code","origin","year"))

dt_panel[
  , 
  (cols_to_interpolate) := lapply(.SD, function(col) 
    na.approx(
      x     = year,       # use year as the independent variable
      object     = col,   # the column to interpolate
      na.rm = FALSE,      # keep leading/trailing NAs (so you can fill them later if you want)
      rule  = 2           # rule=2: carry forward/backward end values
    )
  ), 
  by        = .(county, origin), 
  .SDcols   = cols_to_interpolate
]

dt_na = dt_panel[is.na(price_w),]
missing_value = unique(dt_na$county)
dt_panel_clean = dt_panel[!county %in% missing_value]
# dt_panel_clean = na.omit(dt_panel)
dt_panel_clean[, nonz := ifelse(demand > 0, 1, 0)]

fwrite(dt_panel_clean, data_final("demand_final_alt.csv"))

# Alt 2  - No Origin ------------------------------------------------------

# 1. We create a panel of zip_code, year, origin
zip_code = unique(dt$zip_code)
year = seq(min(dt$year), max(dt$year))
origin = unique(dt$origin)

dt_panel = CJ(zip_code, year, origin)

# 2. We obtain the tariff at the zip_code level
tariff_origin = dt[, .(tariff = mean(tariff)), by = .(origin, year)]
dt_panel = merge(dt_panel, tariff_origin, by = c("origin", "year"), all.x = TRUE)

# 3. Create observables for control at the zip_code level
obs = dt[, .(
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
), by = .(zip_code, year, origin)]

dt_panel = merge(dt_panel, obs, by = c("zip_code", "year", "origin"), all.x = TRUE)

# Cleaning Missing values
dt_panel[, demand := ifelse(is.na(demand), 0, demand)]
dt_panel[, demand_intensive := ifelse(is.na(demand_intensive), 0, demand_intensive)]

# We merge with county id for interpolation
county = unique(dt[, .(county, zip_code)])
dt_panel = merge(dt_panel, county, by = "zip_code")

cols_to_interpolate = c("price_w","elec_price","rebate_w","mean_week_wage","educ","population",
                        "population_density", "median_home_value","PV_system_size_DC")

setorderv(dt_panel, c("zip_code","origin","year"))

dt_panel[
  , 
  (cols_to_interpolate) := lapply(.SD, function(col) 
    na.approx(
      x     = year,       # use year as the independent variable
      object     = col,   # the column to interpolate
      na.rm = FALSE,      # keep leading/trailing NAs (so you can fill them later if you want)
      rule  = 2           # rule=2: carry forward/backward end values
    )
  ), 
  by        = .(county, origin), 
  .SDcols   = cols_to_interpolate
]

dt_na = dt_panel[is.na(price_w),]
missing_value = unique(dt_na$county)
dt_panel_clean = dt_panel[!county %in% missing_value]
# dt_panel_clean = na.omit(dt_panel)
dt_panel_clean[, nonz := ifelse(demand > 0, 1, 0)]

fwrite(dt_panel_clean, data_final("demand_final_alt.csv"))

