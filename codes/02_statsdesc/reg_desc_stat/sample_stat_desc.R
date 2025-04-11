# In this code we do descriptive statistics of our sample
# We try to recover the number of observation, prices, brands, brand by installer, rate of installations, system size
# Mean price, mean rebate, share of product treated by the tariff, concentration of the market at California level
# Tariff level per origin (weighted by firm share) and the nominal rate per firm
# Demographic data : population density, household median income, median value of the house

library(arrow)
library(data.table)
library(gt)
library(ggplot2)

# Data --------------------------------------------------------------------

tts = setDT(read_parquet(data_final("tts_final.parquet")))

# DEMOGRAPHICS -------------------------------------------------------
# THIS PART DEALS WITH THE DEMAND SIDE AND PLACE WHERE THE PANELS ARE INSTALLED
setkey(tts, county, year_quarter)

table = tts[, .(price_w = mean(price_w, na.rm = T), 
                rebate_w = mean(price_w, na.rm = T), 
                premium_panel = mean(premium_panel_overall, na.rm = T),
                premium_installation = mean(premium_installation, na.rm = T),
                PV_system_size_DC = mean(PV_system_size_DC, na.rm = T), 
                population_density = mean(population_density, na.rm = T),
                median_household_income = mean(median_household_income, na.rm = T), 
                median_home_value = mean(median_household_income, na.rm = T), 
                ow_occupied_housing = mean(ow_occupied_housing, na.rm = T), 
                pct_bachelor_estimate = mean(pct_bachelor_estimate, na.rm =T)),
            by = .(tract, china)]


# Compute both mean and SE by group
summary_dt <- tts[, .(
  price_w_mean = mean(price_w, na.rm = TRUE),
  price_w_se = sd(price_w, na.rm = TRUE) ,
  
  rebate_w_mean = mean(rebate_w, na.rm = TRUE),
  rebate_w_se = sd(rebate_w, na.rm = TRUE) ,
  
  premium_panel_mean = mean(premium_panel_overall, na.rm = TRUE),
  premium_panel_se = sd(premium_panel_overall, na.rm = TRUE),
  
  premium_installation_mean = mean(premium_installation, na.rm = TRUE),
  premium_installation_se = sd(premium_installation, na.rm = TRUE),
  
  PV_system_size_DC_mean = mean(PV_system_size_DC, na.rm = TRUE),
  PV_system_size_DC_se = sd(PV_system_size_DC, na.rm = TRUE),
  
  population_density_mean = mean(population_density, na.rm = TRUE),
  population_density_se = sd(population_density, na.rm = TRUE),
  
  median_household_income_mean = mean(median_household_income, na.rm = TRUE),
  median_household_income_se = sd(median_household_income, na.rm = TRUE),
  
  median_home_value_mean = mean(median_home_value, na.rm = TRUE),
  median_home_value_se = sd(median_home_value, na.rm = TRUE),
  
  ow_occupied_housing_mean = mean(ow_occupied_housing, na.rm = TRUE),
  ow_occupied_housing_se = sd(ow_occupied_housing, na.rm = TRUE),
  
  pct_bachelor_estimate_mean = mean(pct_bachelor_estimate, na.rm = TRUE),
  pct_bachelor_estimate_se = sd(pct_bachelor_estimate, na.rm = TRUE)
), by = .(china)]

long_summary <- melt(summary_dt, id.vars = "china", variable.name = "stat", value.name = "value")

# Extract stat type
long_summary[, type := fifelse(grepl("_mean$", stat), "mean", 
                               fifelse(grepl("_se$", stat), "se", NA_character_))]

# Extract base variable name by removing suffix
long_summary[, variable := gsub("_(mean|se)$", "", stat)]

# Spread into wide format with columns: variable | mean_chinese | mean_nonchinese | se_chinese | se_nonchinese
wide_summary <- dcast(long_summary, variable ~ type + china, value.var = "value")

nice_labels <- c(
  price_w = "Price ($/W)",
  rebate_w = "Rebate ($/W)",
  premium_panel = "Premium Panel",
  premium_installation = "Premium Installation",
  PV_system_size_DC = "System Size (kW DC)",
  population_density = "Population Density",
  median_household_income = "Median Household Income",
  median_home_value = "Median Home Value",
  ow_occupied_housing = "Owner-Occupied Housing (%)",
  pct_bachelor_estimate = "Share with BA Degree (%)"
)

# Apply relabeling
wide_summary[, variable := nice_labels[variable]]

# ADD EFFICIENCY
# ADD SUM OBSERVATION/SHARE OF SAMPLE FOR BOTH
# VERIFY SE PREMIUM INSTALLATION
# CHECK PRICE DIFFERENCE



gt_table <- wide_summary |>
  gt(rowname_col = "variable") |>
  
  # Grouping columns
  tab_spanner(label = "Mean", columns = c("mean_0", "mean_1")) |>
  tab_spanner(label = "Standard Error", columns = c("se_0", "se_1")) |>
  
  # Rename columns
  cols_label(
    mean_0 = "Non-Chinese",
    mean_1 = "Chinese",
    se_0 = "Non-Chinese",
    se_1 = "Chinese"
  ) |>
  
  # Format selected rows as dollars
  fmt_currency(
    columns = c("mean_0", "mean_1"),
    rows = variable %in% c("Median Household Income", "Median Home Value"),
    currency = "USD"
  ) |>
  
  # Optionally format the rest with 2 decimals
  fmt_number(
    columns = everything(),
    rows = !variable %in% c("Median Household Income", "Median Home Value"),
    decimals = 2
  ) |>
  
  tab_header(title = "Summary Statistics by Origin of Equipment")


# OFFER SIDE DESCRIPTIVE --------------------------------------------------
# IN THIS SECTION WE SHOW THE NUMBER OF FIRMS OPERATING, HOW MANY BRANDS AN INSTALLER INSTALL ON AVERAGE, 
# HHI FOR INSTALLER IN BOTH ZONES


# TARIFF EXPOSITION -------------------------------------------------------
# DISPLAY THE EVOLUTION OF TARIFF OVER TIME BY COUNTRY OF ORIGIN AND BRAND (FACET)


# PRICE EVOLUTION ---------------------------------------------------------

