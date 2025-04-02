# This script provides the tables and regression to test our quality measures effect on price
# We are testing the quality scope of solar panel before implemenation of trade policy to understand our context

library(arrow)
library(data.table)
library(fixest)
library(modelsummary)
library(stringr)
library(glue)

# Data --------------------------------------------------------------------

tts = read_parquet(data_final("tts_final.parquet"))

# Table 1 - Effect of Quality on Price ------------------------------------

## Panel A - Quality 1 -----------------------------------------------------

# Create the log price variable
setDT(tts)
tts[, ln_price_w := log(price_w)]

# Helper function to run models for a wave and quality variable
run_wave_models <- function(data, quality_var) {
  list(
    "No FE"  = feols(
      as.formula(paste0("ln_price_w ~ ", quality_var)),
                     cluster = ~zip_code, data = data),

    "FE: Year + State" = feols(
      as.formula(paste0("ln_price_w ~ ", quality_var, "  | year + state")),
      cluster = ~zip_code, data = data),
    
    "FE: Year + State + Module Manufacturer" = feols(
      as.formula(paste0("ln_price_w ~ ", quality_var, "  | year + state + module_manufacturer")),
      cluster = ~zip_code, data = data),
    
    "FE: Year + State + Module Manufacturer + Installer" = feols(
      as.formula(paste0("ln_price_w ~ ", quality_var, "  | year + state + module_manufacturer + installer_name")),
      cluster = ~zip_code, data = data)
  )
}

# Run all models and name them carefully for each wave and quality measure
for (q in c("quality_1", "quality_2")){
  models_1 <- list(
    "HO" = list(
      run_wave_models(tts[ho == 1], glue("{q}*china + {q}*korea + {q}*usa"))[["No FE"]],
      run_wave_models(tts[ho == 1], glue("{q}*china + {q}*korea + {q}*usa"))[["FE: Year + State"]],
      run_wave_models(tts[ho == 1], glue("{q}*china + {q}*korea + {q}*usa"))[["FE: Year + State + Module Manufacturer"]],
      run_wave_models(tts[ho == 1], glue("{q}*china + {q}*korea + {q}*usa"))[["FE: Year + State + Module Manufacturer + Installer"]]
      ),
    
    "TPO" = list(
      run_wave_models(tts[tpo == 1], glue("{q}*china + {q}*korea + {q}*usa"))[["No FE"]],
      run_wave_models(tts[tpo == 1], glue("{q}*china + {q}*korea + {q}*usa"))[["FE: Year + State"]],
      run_wave_models(tts[tpo == 1], glue("{q}*china + {q}*korea + {q}*usa"))[["FE: Year + State + Module Manufacturer"]],
      run_wave_models(tts[tpo == 1], glue("{q}*china + {q}*korea + {q}*usa"))[["FE: Year + State + Module Manufacturer + Installer"]]
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
  
  writeLines(table1_quality_char, glue("output/regression/descriptive/table1_{q}.tex"))
}

# Table 1 - Period -----------------------------------------------------

# Subset data by tariff wave
first_wave  <- tts[year_quarter >= "2010Q1" & year_quarter <= "2012Q1"]
second_wave <- tts[year_quarter >= "2013Q1" & year_quarter <= "2014Q2"]
third_wave  <- tts[year_quarter >= "2016Q1" & year_quarter <= "2017Q4"]


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


# Overall
reg_quality_ad1 = feols(price_w ~ quality_2_ad1, cluster = ~zip_code, data = tts)
reg_quality_ad1_fe = feols(price_w ~ quality_2_ad1, fixef = c("year_quarter", "installer_name", "zip_code"), cluster = ~zip_code, data = tts)
reg_quality_ad1_fep = feols(price_w ~ quality_2_ad1, fixef = c("year_quarter", "installer_name", "zip_code", "module_manufacturer"), cluster = ~zip_code, data = tts)


coef_map <- c(
  "quality_1" = "Premium Panels",
  "quality_2" = "Premium Installations",
  # "quality_1_ad1" = "Quality 1",
  # "quality_2_ad1" = "Quality 2",
  # "quality_1_ad2" = "Quality 1",
  # "quality_2_ad2" = "Quality 2",
  # "quality_1_st"  = "Quality 1",
  # "quality_2_st"  = "Quality 2"
)


# Experimentation ---------------------------------------------------------

feols(ln_price_w ~ new_construction + quality_1*china + quality_1*korea,
      fixef = c("module_manufacturer", "installer_name", "zip_code", "year_quarter"), cluster = ~zip_code,data = tts[ho == 1]) 

