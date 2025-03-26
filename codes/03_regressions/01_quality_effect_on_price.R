# This script provides the tables and regression to test our quality measures effect on price
# We are testing the quality scope of solar panel before implemenation of trade policy to understand our context

library(arrow)
library(data.table)
library(fixest)
library(modelsummary)
library(kableExtra)

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
      cluster = ~zip_code, data = data),
    
    "FE: Year + Module Manufacturer + Zip" = feols(
      as.formula(paste0("ln_price_w ~ ", quality_var, " | year_quarter + module_manufacturer + zip_code")),
      cluster = ~zip_code, data = data),
    
    "FE: + Module Manufacturer" = feols(
      as.formula(paste0("ln_price_w ~ ", quality_var, " | year_quarter + installer_name + zip_code + module_manufacturer")),
      cluster = ~zip_code, data = data)
  )
}

# Subset data by tariff wave
first_wave  <- tts[year_quarter >= "2010Q1" & year_quarter <= "2012Q1"]
second_wave <- tts[year_quarter >= "2013Q1" & year_quarter <= "2014Q2"]
third_wave  <- tts[year_quarter >= "2016Q1" & year_quarter <= "2017Q4"]

# Run all models and name them carefully for each wave and quality measure
models_1 <- list(
  "(1)" = run_wave_models(tts, "quality_1")[["No FE"]],
  "(2)" = run_wave_models(tts, "quality_1")[["FE: Year + Installer + Zip"]],
  "(3)" = run_wave_models(tts, "quality_1")[["FE: Year + Module Manufacturer + Zip"]],
  "(4)" = run_wave_models(tts, "quality_1")[["FE: + Module Manufacturer"]],
  
  "(5)"  = run_wave_models(tts, "quality_2")[["No FE"]],
  "(6)"= run_wave_models(tts, "quality_2")[["FE: Year + Installer + Zip"]],
  "(7)" = run_wave_models(tts, "quality_2")[["FE: Year + Module Manufacturer + Zip"]],
  "(8)"= run_wave_models(tts, "quality_2")[["FE: + Module Manufacturer"]]
  )

p_value_1 = fitstat(models_1$`(1)`, type = c("f"))$f$p
p_value_2 = fitstat(models_1$`(2)`, type = c("f"))$f$p
p_value_3 = fitstat(models_1$`(3)`, type = c("f"))$f$p
p_value_4 = fitstat(models_1$`(5)`, type = c("f"))$f$p
p_value_5 = fitstat(models_1$`(4)`, type = c("f"))$f$p
p_value_6 = fitstat(models_1$`(6)`, type = c("f"))$f$p
p_value_7 = fitstat(models_1$`(7)`, type = c("f"))$f$p
p_value_8 = fitstat(models_1$`(8)`, type = c("f"))$f$p

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

models_2 = list(
"2010 - 2012" = list("(1)"  = run_wave_models(first_wave, "quality_1_ad1")[["No FE"]],
                  "(2)" = run_wave_models(first_wave, "quality_1_ad1")[["FE: Year + Installer + Zip"]],
                  "(3)" = run_wave_models(first_wave, "quality_1_ad1")[["FE: + Module Manufacturer"]],

                  "(4)"  = run_wave_models(first_wave, "quality_2_ad1")[["No FE"]],
                  "(5)"= run_wave_models(first_wave, "quality_2_ad1")[["FE: Year + Installer + Zip"]],
                  "(6)"= run_wave_models(first_wave, "quality_2_ad1")[["FE: + Module Manufacturer"]]),

"2013 - 2014" =  list("(7)" = run_wave_models(second_wave, "quality_1_ad2")[["No FE"]],
                   "(8)" = run_wave_models(second_wave, "quality_1_ad2")[["FE: Year + Installer + Zip"]],
                   "(9)" = run_wave_models(second_wave, "quality_1_ad2")[["FE: + Module Manufacturer"]],

                   "(10)" = run_wave_models(second_wave, "quality_2_ad2")[["No FE"]],
                   "(11)" = run_wave_models(second_wave, "quality_2_ad2")[["FE: Year + Installer + Zip"]],
                   "(12)" = run_wave_models(second_wave, "quality_2_ad2")[["FE: + Module Manufacturer"]]),

"2016- 2018" =  list("(13)"  = run_wave_models(third_wave, "quality_1_st")[["No FE"]],
                   "(14)"  = run_wave_models(third_wave, "quality_1_st")[["FE: Year + Installer + Zip"]],
                   "(15)"  = run_wave_models(third_wave, "quality_1_st")[["FE: + Module Manufacturer"]],

                   "(16)"  = run_wave_models(third_wave, "quality_2_st")[["No FE"]],
                   "(17)"  = run_wave_models(third_wave, "quality_2_st")[["FE: Year + Installer + Zip"]],
                   "(18)"  = run_wave_models(third_wave, "quality_2_st")[["FE: + Module Manufacturer"]])
)

# Map raw coefficient names to prettier labels
coef_map <- c(
  "quality_1" = "Quality 1",
  "quality_2" = "Quality 2",
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
  statistic = "({std.error})",
  stars = TRUE,
  gof_omit = "Adj|AIC|BIC|Log|Within|Pseudo|RMSE",
  add_rows = add_row,
  output = "kableExtra"
)

# Apply kableExtra styling and force LaTeX format
table1_quality <- table1_quality %>% 
  kable_styling(full_width = FALSE, latex_options = c("hold_position"))
save_kable(table1_quality, file = "output/regression/descriptive/table1_quality.tex")

# Panel B - Quality 2 -----------------------------------------------------

# Overall
reg_quality_ad1 = feols(price_w ~ quality_2_ad1, cluster = ~zip_code, data = tts)
reg_quality_ad1_fe = feols(price_w ~ quality_2_ad1, fixef = c("year_quarter", "installer_name", "zip_code"), cluster = ~zip_code, data = tts)
reg_quality_ad1_fep = feols(price_w ~ quality_2_ad1, fixef = c("year_quarter", "installer_name", "zip_code", "module_manufacturer"), cluster = ~zip_code, data = tts)


