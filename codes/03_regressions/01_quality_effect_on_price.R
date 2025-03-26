# This script provides the tables and regression to test our quality measures effect on price
# We are testing the quality scope of solar panel before implemenation of trade policy to understand our context

library(arrow)
library(data.table)
library(fixest)
library(modelsummary)
library(gt)
library(flextable)

# Data --------------------------------------------------------------------

tts = read_parquet(data_final("tts_final.parquet"))

# Table 1 - Effect of Quality on Price ------------------------------------

## Panel A - Quality 1 -----------------------------------------------------

# Create the log price variable
tts[, ln_price_w := log(price_w)]

# Helper function to run models for a wave and quality variable
run_wave_models <- function(data, quality_var) {
  list(
    "No FE"  = feols(as.formula(paste0("price_w ~ ", quality_var)),
                     cluster = ~zip_code, data = data),
    
    "FE: Year + Installer + Zip" = feols(
      as.formula(paste0("ln_price_w ~ ", quality_var, " | year_quarter + installer_name + zip_code")),
      cluster = ~state, data = data),
    
    "FE: Year + Module Manufacturer + Zip" = feols(
      as.formula(paste0("ln_price_w ~ ", quality_var, " | year_quarter + module_manufacturer + zip_code")),
      cluster = ~state, data = data),
    
    "FE: + Module Manufacturer" = feols(
      as.formula(paste0("ln_price_w ~ ", quality_var, " | year_quarter + installer_name + zip_code + module_manufacturer")),
      cluster = ~state, data = data)
  )
}

# Subset data by tariff wave
first_wave  <- tts[year_quarter >= "2010Q1" & year_quarter <= "2012Q1"]
second_wave <- tts[year_quarter >= "2013Q1" & year_quarter <= "2014Q2"]
third_wave  <- tts[year_quarter >= "2016Q1" & year_quarter <= "2017Q4"]

# Run all models and name them carefully for each wave and quality measure
models_1 <- list(
  "Quality 1" = list(run_wave_models(tts, "quality_1")[["No FE"]],
  run_wave_models(tts, "quality_1")[["FE: Year + Installer + Zip"]],
  run_wave_models(tts, "quality_1")[["FE: Year + Module Manufacturer + Zip"]],
  run_wave_models(tts, "quality_1")[["FE: + Module Manufacturer"]]),
  
  "Quality 2" = list(run_wave_models(tts, "quality_2")[["No FE"]],
  run_wave_models(tts, "quality_2")[["FE: Year + Installer + Zip"]],
  run_wave_models(tts, "quality_2")[["FE: Year + Module Manufacturer + Zip"]],
  run_wave_models(tts, "quality_2")[["FE: + Module Manufacturer"]])
  )

p_value_1 = fitstat(models_1$`Quality 1`[[1]], type = c("f"))$f$p
p_value_2 = fitstat(models_1$`Quality 1`[[2]], type = c("f"))$f$p
p_value_3 = fitstat(models_1$`Quality 1`[[3]], type = c("f"))$f$p
p_value_4 = fitstat(models_1$`Quality 1`[[4]], type = c("f"))$f$p
p_value_5 = fitstat(models_1$`Quality 2`[[1]], type = c("f"))$f$p
p_value_6 = fitstat(models_1$`Quality 2`[[2]], type = c("f"))$f$p
p_value_7 = fitstat(models_1$`Quality 2`[[3]], type = c("f"))$f$p
p_value_8 = fitstat(models_1$`Quality 2`[[4]], type = c("f"))$f$p

add_row = data.frame(
  test = "F-test p-value",
  p1 = p_value_1,
  p2 = p_value_2,
  p3 = p_value_3,
  p4 = p_value_4,
  p5 = p_value_5,
  p6 = p_value_6,
  p7 = p_value_7,
  p8 = p_value_8 
)

# Map raw coefficient names to prettier labels
coef_map <- c(
  "quality_1" = "Premium Panels",
  "quality_2" = "Premium Installations",
  "quality_1_ad1" = "Quality 1",
  "quality_2_ad1" = "Quality 2",
  "quality_1_ad2" = "Quality 1",
  "quality_2_ad2" = "Quality 2",
  "quality_1_st"  = "Quality 1",
  "quality_2_st"  = "Quality 2"
)

# Create the side-by-side (cbind) table
table1_quality <- modelsummary(
  models_1,
  coef_map = coef_map,
  stars = F,
  shape = "cbind",
  gof_omit = "Adj|AIC|BIC|Log|Within|Pseudo|RMSE|Std.",
  add_rows = add_row,
  output = "latex"
)
table1_quality_char = as.character(table1_quality)
writeLines(table1_quality_char, "output/regression/descriptive/table1_quality.tex")



# Panel B - Quality 2 -----------------------------------------------------

# Overall
reg_quality_ad1 = feols(price_w ~ quality_2_ad1, cluster = ~zip_code, data = tts)
reg_quality_ad1_fe = feols(price_w ~ quality_2_ad1, fixef = c("year_quarter", "installer_name", "zip_code"), cluster = ~zip_code, data = tts)
reg_quality_ad1_fep = feols(price_w ~ quality_2_ad1, fixef = c("year_quarter", "installer_name", "zip_code", "module_manufacturer"), cluster = ~zip_code, data = tts)


