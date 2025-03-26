# This script provides the tables and regression to test our quality measures effect on price
# We are testing the quality scope of solar panel before implemenation of trade policy to understand our context

library(arrow)
library(data.table)
library(fixest)
library(modelsummary)

# Data --------------------------------------------------------------------

tts = read_parquet(data_final("tts_final.parquet"))

# Table 1 - Effect of Quality on Price ------------------------------------

## Panel A - Quality 1 -----------------------------------------------------

# First Wave of Tariff
first_wave = tts[year_quarter >= "2010Q1" & year_quarter <= "2012Q1"]

reg_quality_1_ad1 = feols(price_w ~ quality_1_ad1, cluster = ~state, data = first_wave)
reg_quality_1_ad1_fe = feols(price_w ~ quality_1_ad1, fixef = c("year_quarter", "installer_name", "zip_code"), cluster = ~state, data = first_wave)
reg_quality_1_ad1_fep = feols(price_w ~ quality_1_ad1, fixef = c("year_quarter", "installer_name", "zip_code", "module_manufacturer"), cluster = ~state, data = first_wave)

# Second Wave of Tariff
second_wave = tts[year_quarter >= "2013Q1" & year_quarter <= "2014Q2"]

reg_quality_1_ad2 = feols(price_w ~ quality_1_ad2, data = second_wave)
reg_quality_1_ad2_fe = feols(price_w ~ quality_1_ad2, fixef = c("year_quarter", "installer_name", "zip_code"), cluster = ~state, data = second_wave)
reg_quality_1_ad2_fep = feols(price_w ~ quality_1_ad2, fixef = c("year_quarter", "installer_name", "zip_code", "module_manufacturer"), cluster = ~state, data = second_wave)

# Third wave of Tariff
third_wave = tts[year_quarter >= "2016Q1" & year_quarter <= "2017Q4"]

reg_quality_1_st = feols(price_w ~ quality_1_st, data = third_wave)
reg_quality_1_st_fe = feols(price_w ~ quality_1_st, fixef = c("year_quarter", "installer_name", "zip_code"), cluster = ~state, data = third_wave)
reg_quality_1_st_fep = feols(price_w ~ quality_1_st, fixef = c("year_quarter", "installer_name", "zip_code", "module_manufacturer"), cluster = ~state, data = third_wave)

modelsummary(
  list(
    "First Wave" = reg_quality_1_ad1_fe,
    "Second Wave" = reg_quality_1_ad2_fe,
    "Third Wave" = reg_quality_1_st_fe
  ),
  title = "Effect of Quality on Price per Watt across Tariff Waves",
  stars = TRUE,
  gof_omit = "Adj|AIC|BIC|Log|Within|Pseudo|R2",
  statistic = "({std.error})",
  notes = c("Standard errors clustered at the state level", "All models include fixed effects")
)

# Panel B - Quality 2 -----------------------------------------------------

# Overall
reg_quality_ad1 = feols(price_w ~ quality_2_ad1, cluster = ~zip_code, data = tts)
reg_quality_ad1_fe = feols(price_w ~ quality_2_ad1, fixef = c("year_quarter", "installer_name", "zip_code"), cluster = ~zip_code, data = tts)
reg_quality_ad1_fep = feols(price_w ~ quality_2_ad1, fixef = c("year_quarter", "installer_name", "zip_code", "module_manufacturer"), cluster = ~zip_code, data = tts)


