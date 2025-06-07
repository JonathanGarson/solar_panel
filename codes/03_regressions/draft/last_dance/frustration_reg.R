source(file = "codes/last_dance/code_panel.R")
library(fixest)
Sys.setenv(RGL_USE_NULL = TRUE)
library(DIDmultiplegtDYN)

# Reg ---------------------------------------------------------------------
panel_clean[, treated := ifelse(tariff > 1, 1, 0)]
panel_clean[, ln_price := log(price_w)]
panel_clean[, ln_tariff := log(tariff)]

# Count number of periods per group
obs_count <- panel_clean[, .N, by = module_manufacturer]
max_periods <- max(obs_count$N)

# Keep only complete groups
complete_modules <- obs_count[N == max_periods, module_manufacturer]
panel_balanced <- panel_clean[module_manufacturer %in% complete_modules]

test = feols(log(price_w) ~ i(year_quarter,treated, ref= "2014Q1")| year_quarter + module_manufacturer, cluster = "module_manufacturer", data = panel_balanced)

