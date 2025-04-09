# This script provides the tables and regression to test our quality measures effect on price
# We are testing the quality scope of solar panel before implemenation of trade policy to understand our context

library(arrow)
library(data.table)
library(fixest)
library(modelsummary)
library(stringr)
library(glue)

# Data --------------------------------------------------------------------

tts = setDT(read_parquet(data_final("tts_final.parquet")))
market_assignments_ioc = fread(data_temp("market_assignments_ioc.csv"))

# Table 1 - Effect of Quality 1 on Price ------------------------------------

## Complete -----------------------------------------------------

# Create the log price variable
tts[, ln_price_w := log(price_w)]

# coef_map 
coef_map = c(
  "premium_panel_overall" = "Premium Panel Overall",
  "premium_panel_overall:usa" = "Premium Panel Overall x USA Brands",
  "premium_panel_overall:korea" = "Premium Panel Overall x Korea Brands",
  "premium_panel_overall:china" = "Premium Panel Overall x China Brands",
  "premium_panel_ad1" = "Premium Panel Relative",
  "premium_panel_ad1:usa" = "Premium Panel Relative x USA Brands",
  "premium_panel_ad1:korea" = "Premium Panel Relative x Korea Brands",
  "premium_panel_ad1:china" = "Premium Panel Relative x China Brands",
  "premium_panel_ad2" = "Premium Panel Relative",
  "premium_panel_ad2:usa" = "Premium Panel Relative x USA Brands",
  "premium_panel_ad2:korea" = "Premium Panel Relative x Korea Brands",
  "premium_panel_ad2:china" = "Premium Panel Relative x China Brands",
  "premium_panel_st" = "Premium Panel Relative",
  "premium_panel_st:usa" = "Premium Panel Relative x USA Brands",
  "premium_panel_st:korea" = "Premium Panel Relative x Korea Brands",
  "premium_panel_st:china" = "Premium Panel Relative x China Brands"
)

# --- Helper function (as given) ---
set_control = c("median_home_value + median_household_income + population_density + PV_system_size_DC + PV_system_size_DC^2 + 
                elec_price + h_median + rebate_w")

run_wave_models <- function(data, quality_var) {
  list(
    "No FE"  = feols(
      as.formula(glue("price_w ~  {quality_var} + {set_control}")),
      cluster = ~zip_code, data = data
    ),
    
    "FE: Year + State" = feols(
      as.formula(glue("price_w ~  {quality_var} + {set_control}| year + state")),
      cluster = ~zip_code, data = data
    ),
    
    "FE: Year + State + Installer" = feols(
      as.formula(glue("price_w ~ {quality_var} + {set_control}| year + state + installer_name")),
      cluster = ~zip_code, data = data
    ),
    
    "FE: Year + State + Module Manufacturer + Installer" = feols(
      as.formula(glue("price_w ~ {quality_var} + {set_control}| year + state + installer_name +  module_manufacturer")),
      cluster = ~zip_code, data = data
    )
  )
}

# --- Run all models ---
models_1 <- list(
  "Quality 1 Overall" = list(
    run_wave_models(tts, "premium_panel_overall*china + premium_panel_overall*korea + premium_panel_overall*usa")[["No FE"]],
    run_wave_models(tts, "premium_panel_overall*china + premium_panel_overall*korea + premium_panel_overall*usa")[["FE: Year + State"]],
    run_wave_models(tts, "premium_panel_overall*china + premium_panel_overall*korea + premium_panel_overall*usa")[["FE: Year + State + Installer"]],
    run_wave_models(tts, "premium_panel_overall*china + premium_panel_overall*korea + premium_panel_overall*usa")[["FE: Year + State + Module Manufacturer + Installer"]]
  ),
  
  "Quality 1 : 2010 - 2013" = list(
    run_wave_models(tts, "premium_panel_ad1*china + premium_panel_ad1*korea + premium_panel_ad1*usa")[["No FE"]],
    run_wave_models(tts, "premium_panel_ad1*china + premium_panel_ad1*korea + premium_panel_ad1*usa")[["FE: Year + State"]],
    run_wave_models(tts, "premium_panel_ad1*china + premium_panel_ad1*korea + premium_panel_ad1*usa")[["FE: Year + State + Installer"]],
    run_wave_models(tts, "premium_panel_ad1*china + premium_panel_ad1*korea + premium_panel_ad1*usa")[["FE: Year + State + Module Manufacturer + Installer"]]
  ),  
  
  "Quality 1 : 2014 - 2016" = list(
    run_wave_models(tts, "premium_panel_ad2*china + premium_panel_ad2*korea + premium_panel_ad2*usa")[["No FE"]],
    run_wave_models(tts, "premium_panel_ad2*china + premium_panel_ad2*korea + premium_panel_ad2*usa")[["FE: Year + State"]],
    run_wave_models(tts, "premium_panel_ad2*china + premium_panel_ad2*korea + premium_panel_ad2*usa")[["FE: Year + State + Installer"]],
    run_wave_models(tts, "premium_panel_ad2*china + premium_panel_ad2*korea + premium_panel_ad2*usa")[["FE: Year + State + Module Manufacturer + Installer"]]
  ),
  
  "Quality 1 : 2017 - 2020" = list(
    run_wave_models(tts, "premium_panel_st*china + premium_panel_st*korea + premium_panel_st*usa")[["No FE"]],
    run_wave_models(tts, "premium_panel_st*china + premium_panel_st*korea + premium_panel_st*usa")[["FE: Year + State"]],
    run_wave_models(tts, "premium_panel_st*china + premium_panel_st*korea + premium_panel_st*usa")[["FE: Year + State + Installer"]],
    run_wave_models(tts, "premium_panel_st*china + premium_panel_st*korea + premium_panel_st*usa")[["FE: Year + State + Module Manufacturer + Installer"]]
  )
)

# --- Create a table for "Quality 1 Overall" ---
models_quality1_overall <- models_1[["Quality 1 Overall"]]

# Compute add_rows for overall models
results_list_overall <- list()
for(i in seq_along(models_quality1_overall)) {
  m <- models_quality1_overall[[i]]
  f_p    <- fitstat(m, type = "f")$f$p
  wald_p <- fitstat(m, type = "wald")$wald$p
  my = fitstat(m, type = "my")$my
  col_name <- paste0("Overall_model", i)
  results_list_overall[[col_name]] <- c(f_p, wald_p, my)
}
df_overall_add_rows <- data.frame(
  term = c("F-test p-value", "Wald-test p-value", "Dependent Variable Mean"),
  results_list_overall,
  check.names = FALSE
)

table_premium_overall <- modelsummary(
  models_quality1_overall,
  coef_map = coef_map,
  stars = TRUE,
  escape = TRUE,
  gof_omit = "Adj|AIC|BIC|Within|Pseudo|RMSE|Std.",
  add_rows = df_overall_add_rows,
  notes = "Notes: The dependent variable is a price in $ per W, so the estimate reports a dollar variation in price. Standard errors are clustered at the zip code level.",
  output = "latex"
)

# --- Create a table for the three period panels ---
models_quality1_periods <- list(
  "2010 - 2013" = models_1[["Quality 1 : 2010 - 2013"]],
  "2014 - 2016" = models_1[["Quality 1 : 2014 - 2016"]],
  "2017 - 2020" = models_1[["Quality 1 : 2017 - 2020"]]
)

# Compute add_rows for period models (nested naming for each panel)
results_list_period <- list()
for(panel in names(models_quality1_periods)) {
  models_panel <- models_quality1_periods[[panel]]
  for(i in seq_along(models_panel)) {
    m <- models_panel[[i]]
    f_p    <- fitstat(m, type = "f")$f$p
    wald_p <- fitstat(m, type = "wald")$wald$p
    my = fitstat(m, type = "my")$my
    col_name <- paste0(gsub("[[:space:][:punct:]]+", "_", panel), "_model", i)
    results_list_period[[col_name]] <- c(f_p, wald_p, my)
  }
}
df_period_add_rows <- data.frame(
  term = c("F-test p-value", "Wald-test p-value", "Dependent Variable Mean"),
  results_list_period,
  check.names = FALSE
)

table_premium_relative <- modelsummary(
  models_quality1_periods,
  coef_map = coef_map,
  stars = TRUE,
  shape = 'cbind',
  escape = TRUE,
  gof_omit = "Adj|AIC|BIC|Within|Pseudo|RMSE|Std.",
  add_rows = df_period_add_rows,
  notes = "Notes: The dependent variable is a price in $ per W, so the estimate reports a dollar variation in price. Standard errors are clustered at the zip code level.",
  output = "latex"
)

table_premium_overall_char <- as.character(table_premium_overall)
table_premium_relative_char <- as.character(table_premium_relative)

writeLines(table_premium_overall_char, "output/regression/descriptive/table_premium_overall.tex")
writeLines(table_premium_relative_char, "output/regression/descriptive/table_premium_relative.tex")

## Short ---------------------------------------------------------

for (q in c("quality_1", "quality_2")){
  models_1 <- list(
    "Quality 1 Overall" = list(
      run_wave_models(tts, glue("{q}*china + {q}*korea + {q}*usa"))[["No FE"]],
      run_wave_models(tts, glue("{q}*china + {q}*korea + {q}*usa"))[["FE: Year + State"]],
      run_wave_models(tts, glue("{q}*china + {q}*korea + {q}*usa"))[["FE: Year + State + Module Manufacturer"]],
      run_wave_models(tts, glue("{q}*china + {q}*korea + {q}*usa"))[["FE: Year + State + Module Manufacturer + Installer"]]
      ),
    
    "TPO" = list(
      run_wave_models(tts, glue("{q}*china + {q}*korea + {q}*usa"))[["No FE"]],
      run_wave_models(tts, glue("{q}*china + {q}*korea + {q}*usa"))[["FE: Year + State"]],
      run_wave_models(tts, glue("{q}*china + {q}*korea + {q}*usa"))[["FE: Year + State + Module Manufacturer"]],
      run_wave_models(tts, glue("{q}*china + {q}*korea + {q}*usa"))[["FE: Year + State + Module Manufacturer + Installer"]]
      )
    )
  
  df_add_row <- data.frame(
    term = c("F-test p-value", "Wald-test p-value"),
    HO_model1 = c(
      fitstat(models_1$HO[[1]], type = "f")$f$p,
      fitstat(models_1$HO[[1]], type = "wald")$wald$p
    ),
    HO_model2 = c(
      fitstat(models_1$HO[[2]], type = "f")$f$p,
      fitstat(models_1$HO[[2]], type = "wald")$wald$p
    ),
    HO_model3 = c(
      fitstat(models_1$HO[[3]], type = "f")$f$p,
      fitstat(models_1$HO[[3]], type = "wald")$wald$p
    ),
    HO_model4 = c(
      fitstat(models_1$HO[[4]], type = "f")$f$p,
      fitstat(models_1$HO[[4]], type = "wald")$wald$p
    ),
    TPO_model1 = c(
      fitstat(models_1$TPO[[1]], type = "f")$f$p,
      fitstat(models_1$TPO[[1]], type = "wald")$wald$p
    ),
    TPO_model2 = c(
      fitstat(models_1$TPO[[2]], type = "f")$f$p,
      fitstat(models_1$TPO[[2]], type = "wald")$wald$p
    ),
    TPO_model3 = c(
      fitstat(models_1$TPO[[3]], type = "f")$f$p,
      fitstat(models_1$TPO[[3]], type = "wald")$wald$p
    ),
    TPO_model4 = c(
      fitstat(models_1$TPO[[4]], type = "f")$f$p,
      fitstat(models_1$TPO[[4]], type = "wald")$wald$p
    )
  )
  
  # Map raw coefficient names to prettier labels
  coef_map <- c(
    # "(Intercept)" = "Intercept",
    "quality_1" = "Premium Panels",
    "quality_2" = "Premium Installations",
    "quality_1:china" = "Premium Panels × Chinese Brand",
    "quality_1:korea" = "Premium Panels × Korean Brand",
    "quality_1:usa" = "Premium Panels × USA Brand",
    "quality_2:china" = "Premium Installations × Chinese Brand",
    "quality_2:korea" = "Premium Installations × Korean Brand",
    "quality_2:usa" = "Premium Installations × USA Brand"
  )
  
 
  # Create the side-by-side (cbind) table
  table1_quality <- modelsummary(
    models_1,
    coef_map = coef_map,
    stars = F,
    shape = 'cbind',
    escape = TRUE,
    gof_omit = "Adj|AIC|BIC|Within|Pseudo|RMSE|Std.",
    add_rows = df_add_row,
    notes = "Notes: The dependent variable is a log price per W, so the estimate reports percent variations in price.
    HO and TPO correspond to 'Host Owned' and 'Third Party Owned' systems. The standard errors are clustered at the zip code level shown between parenthesis.",
    # output = "latex"
  )
  
  table1_quality_char = as.character(table1_quality)
  table1_quality_char <- gsub("\\bX\\b", "Yes", table1_quality_char)
  
  # writeLines(table1_quality_char, glue("output/regression/descriptive/table1_{q}.tex"))
}

# Table 2 - Quality 2 -----------------------------------------------------

coef_map = c(
  "premium_installation" = "Premium Installation Overall",
  "premium_installation:usa" = "Premium Installation Overall x USA Brands",
  "premium_installation:korea" = "Premium Installation Overall x Korea Brands",
  "premium_installation:china" = "Premium Installation Overall x China Brands"
)

models_2 = list(
  "Overall" = list(
    run_wave_models(tts, "premium_installation*china + premium_installation*korea + premium_installation*usa")[["No FE"]],
    run_wave_models(tts, "premium_installation*china + premium_installation*korea + premium_installation*usa")[["FE: Year + State"]],
    run_wave_models(tts, "premium_installation*china + premium_installation*korea + premium_installation*usa")[["FE: Year + State + Installer"]],
    run_wave_models(tts, "premium_installation*china + premium_installation*korea + premium_installation*usa")[["FE: Year + State + Module Manufacturer + Installer"]]
    )
  ,

  "Quality 2 : 2010 - 2013" = list(
    run_wave_models(tts[year %in% 2010:2013], "premium_installation*china + premium_installation*korea + premium_installation*usa")[["No FE"]],
    run_wave_models(tts[year %in% 2010:2013], "premium_installation*china + premium_installation*korea + premium_installation*usa")[["FE: Year + State"]],
    run_wave_models(tts[year %in% 2010:2013], "premium_installation*china + premium_installation*korea + premium_installation*usa")[["FE: Year + State + Installer"]],
    run_wave_models(tts[year %in% 2010:2013], "premium_installation*china + premium_installation*korea + premium_installation*usa")[["FE: Year + State + Module Manufacturer + Installer"]]
  ),

  "Quality 2 : 2014 - 2016" = list(
    run_wave_models(tts[year %in% 2014:2016], "premium_installation*china + premium_installation*korea + premium_installation*usa")[["No FE"]],
    run_wave_models(tts[year %in% 2014:2016], "premium_installation*china + premium_installation*korea + premium_installation*usa")[["FE: Year + State"]],
    run_wave_models(tts[year %in% 2014:2016], "premium_installation*china + premium_installation*korea + premium_installation*usa")[["FE: Year + State + Installer"]],
    run_wave_models(tts[year %in% 2014:2016], "premium_installation*china + premium_installation*korea + premium_installation*usa")[["FE: Year + State + Module Manufacturer + Installer"]]
  ),

  "Quality 2 : 2017 - 2020" = list(
    run_wave_models(tts[year %in% 2017:2020], "premium_installation*china + premium_installation*korea + premium_installation*usa")[["No FE"]],
    run_wave_models(tts[year %in% 2017:2020], "premium_installation*china + premium_installation*korea + premium_installation*usa")[["FE: Year + State"]],
    run_wave_models(tts[year %in% 2017:2020], "premium_installation*china + premium_installation*korea + premium_installation*usa")[["FE: Year + State + Installer"]],
    run_wave_models(tts[year %in% 2017:2020], "premium_installation*china + premium_installation*korea + premium_installation*usa")[["FE: Year + State + Module Manufacturer + Installer"]]
  )
)

# --- Create a table for "Quality 2 Overall" ---
models_quality2_overall <- models_2[["Overall"]]

# Compute add_rows for overall models
results_list_overall <- list()
for(i in seq_along(models_quality2_overall)) {
  m <- models_quality2_overall[[i]]
  f_p    <- fitstat(m, type = "f")$f$p
  wald_p <- fitstat(m, type = "wald")$wald$p
  my = fitstat(m, type = "my")$my
  col_name <- paste0("Overall_model", i)
  results_list_overall[[col_name]] <- c(f_p, wald_p, my)
}

df_overall_add_rows <- data.frame(
  term = c("F-test p-value", "Wald-test p-value", "Dependent Variable Mean"),
  results_list_overall,
  check.names = FALSE
)

table_premium_install_overall <- modelsummary(
  models_quality2_overall,
  coef_map = coef_map,
  stars = TRUE,
  escape = TRUE,
  # shape = "cbind",
  gof_omit = "Adj|AIC|BIC|Within|Pseudo|RMSE|Std.",
  add_rows = df_overall_add_rows,
  notes = "Notes: The dependent variable is a price in $ per W, so the estimate reports a dollar variation in price. Standard errors are clustered at the zip code level.",
  output = "latex"
)

table_premium_install_overall_char <- as.character(table_premium_install_overall)
writeLines(table_premium_install_overall_char, "output/regression/descriptive/table_premium_install_overall.tex")

# Test --------------------------------------------------------------------
tts[, micro_inverter_1 := fcase(micro_inverter_1 == "Y", 1, 
                                micro_inverter_1 == "N", 0,
                                default = NA)]

tts[, ground_mounted := fcase(ground_mounted == "1", 1, 
                              ground_mounted == "0", 0,
                              default = NA)]
tts[, new_construction := fcase(new_construction == "1", 1,
                                new_construction == "0", 0,
                                default = NA)]
tts[, premium := ifelse(efficiency_module > 0.20, 1, 0)]

market_assignments_ioc[, zip_code := as.character(zip_code)]
tts = merge(tts, market_assignments_ioc, by = "zip_code")

# Building HHI zip_code level
installer_counts <- tts[, .(installs_by_installer = .N), by = .(county, installer_name, year)]
zip_totals <- tts[, .(total_installs_zip = .N), by = .(county, year)]
market_share <- merge(installer_counts, zip_totals, by = c('county', 'year'))
market_share[, market_share_installer := installs_by_installer / total_installs_zip]
market_share[, hhi_index_c := sum(market_share_installer^2), by = .(county, year)]
tts <- merge(tts, market_share[, .(county, installer_name, hhi_index_c, year)],
             by = c("county", "installer_name", "year"), all.x = TRUE)
tts[, market_size := .N, by = .(county, year)]
tts[, hhi_index_c_sqr := hhi_index_c^2]

# Building HHI market_id level
installer_counts <- tts[, .(installs_by_installer = .N), by = .(market_id, installer_name, year)]
zip_totals <- tts[, .(total_installs_zip = .N), by = .(market_id, year)]
market_share <- merge(installer_counts, zip_totals, by = c('market_id', 'year'))
market_share[, market_share_installer := installs_by_installer / total_installs_zip]
market_share[, hhi_index_md := sum(market_share_installer^2), by = .(market_id, year)]
tts <- merge(tts, market_share[, .(market_id, installer_name, hhi_index_md, year)],
             by = c("market_id", "installer_name", "year"), all.x = TRUE)
tts[, market_size := .N, by = .(market_id, year)]
tts[, hhi_index_md_sqr := hhi_index_md^2]

feols(price_w ~ PV_system_size_DC + PV_system_size_DC^2 + premium_panel_overall + micro_inverter_1 + DC_optimizer + ground_mounted 
      | state + year_quarter + installer_name + module_manufacturer , cluster = ~zip_code, data = tts[year == 2018])

# VERY CLOSE TO SHAUGHNESSY BUT DO NOT HOLD FOR OTHER YEAR THAN 2017 AND 2018
feols(price_w ~ premium_panel_ad1 +  premium_panel_ad2 + premium_panel_st+ median_home_value + median_household_income + population_density + PV_system_size_DC + PV_system_size_DC^2 +
        micro_inverter_1 + DC_optimizer + ground_mounted+ hhi_index_md + hhi_index_md_sqr + market_size 
      | state + year_quarter + installer_name + module_manufacturer , cluster = ~zip_code, data = tts[state == "ca"])
feols(price_w ~ premium_panel_overall + median_home_value + median_household_income + population_density + PV_system_size_DC + PV_system_size_DC^2 +
        micro_inverter_1 + DC_optimizer + ground_mounted 
      | state + year_quarter + installer_name + module_manufacturer , cluster = ~zip_code, data = tts[year == 2018])


rep = feols(price_w ~ PV_system_size_DC + PV_system_size_DC^2 + premium_1 + micro_inverter_1 + DC_optimizer + ground_mounted 
            | state + year_quarter + installer_name + module_manufacturer_1, cluster = ~zip_code, data = tts_clean)


