# This code generates the demand data set
# ISSUE WITH BALANCE DATA SET

library(arrow)
library(data.table)
library(zoo)

# Data --------------------------------------------------------------------

demand_tts = read_parquet(data_final("tts_final.parquet"))
wages = fread(data_temp("elec_contractor_wage_emp.csv"))
elec = fread(data_temp("elec_price.csv"))

# Clean -------------------------------------------------------------------
# elec
elec[, year := as.numeric(gsub(pattern = "Q\\d+", replacement = "", year_quarter))]
elec = unique(elec[state == "ca", .(state, year, mean_price_year)])

# wage
wages[, state := "ca"]

# merging 
data = merge(wages, elec, by = c("state", "year"))
matching_code = demand_tts[, .(zip_code, year, county, price_w, rebate_w, module_quantity, population, 
                               population_density,median_home_value, median_household_income, pct_bachelor_estimate,
                               PV_system_size_DC, china, usa, korea, japan, germany, norway)]
matching_code = merge(matching_code, data, by = c("year", "county"))
setnames(matching_code, "mean_price_year", "elec_price_year")

demand_agg = unique(matching_code[, .(
  demand_extensive = (.N/population)*1000,
  demand_intensive = (sum(module_quantity, na.rm = TRUE)/population)*1000,
  price_w = mean(price_w, na.rm = TRUE),
  elec_price = mean(elec_price_year, na.rm = TRUE),
  rebate_w = mean(rebate_w, na.rm = TRUE),
  mean_week_wage = mean(mean_week_wage, na.rm = TRUE),
  educ = mean(pct_bachelor_estimate, na.rm = TRUE),
  population = mean(population_density, na.rm = TRUE),
  population_density = mean(population_density, na.rm = TRUE),
  median_home_value = mean(median_home_value, na.rm = TRUE),
  PV_system_size_DC = mean(PV_system_size_DC, na.rm = TRUE),
  china = mean(china),
  usa = mean(usa),
  korea = mean(korea),
  japan = mean(japan),
  germany = mean(germany),
  norway = mean(norway)
),
by = .(zip_code, year)])

# We start by creating a cartesian join of all zip code and data
zip_code = unique(demand_tts$zip_code)
full_date_range <- seq(min(demand_tts$year), max(demand_tts$year))
complete_panel = CJ(zip_code = zip_code, year = full_date_range)
county = unique(demand_tts[, .(zip_code, county, tract)])
complete_panel = merge(complete_panel, county, by = "zip_code", all.x = TRUE)

demand =  merge(complete_panel, demand_agg, by = c("zip_code", "year"), all.x = TRUE)
demand[is.na(demand_extensive) | is.na(demand_intensive),
       `:=`(demand_extensive = 0, demand_intensive = 0)]

# Interpolation
setorder(demand, zip_code, year)

# Specify the names of the numeric columns you want to interpolate.
cols_to_interpolate <- c("price_w", "elec_price", 
                         "rebate_w", "mean_week_wage", "educ", "population",
                         "china","usa","korea","japan","germany","norway", 
                         "population_density", "median_home_value", "PV_system_size_DC")

# For each zip_code group, apply linear interpolation (na.approx) over 'year'
interp_fun <- function(col, yrs) {
  ok <- which(!is.na(col))
  # need at least two non‑NA *and* two distinct years
  if (length(ok) < 2 || length(unique(yrs[ok])) < 2) {
    return(col)
  }
  zoo::na.approx(col, x = yrs, na.rm = FALSE, rule = 2)
}

# apply by tract
demand[ , (cols_to_interpolate) := lapply(.SD, function(col)
  interp_fun(col, year)
),
by      = zip_code,
.SDcols = cols_to_interpolate
]
# We adapt the logic of the interpolation but for that we do it by adopting the nearest neighbor approach

demand = na.omit(demand)

# Share of 0
# nrow(demand[demand_extensive == 0])/nrow(demand)

fwrite(demand, data_final("demand_final.csv"))

# Test --------------------------------------------------------------------

# library(panelr)
# library(plm)
# test = panel_data(demand_tts, id = "tract", wave = "year")
# pdf <- pdata.frame(test, index = c("tract", "year"))
# pd <- pdim(pdf)
# pd
# # $nT      # number of observations per individual (min, max, avg)
# # $Tn      # number of observations per time period (min, max, avg)
# # $balanced # TRUE/FALSE
# 
# # 1c) Just test balancedness
# is.pbalanced(pdf)  # TRUE if every id has every year
