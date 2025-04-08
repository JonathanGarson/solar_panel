# This code evaluate the subsidy pass-through

library(arrow)
library(data.table)
library(fixest)
library(modelsummary)
library(glue)

# Data --------------------------------------------------------------------

tts= read_parquet(data_final("TTS_final.parquet"))

# The effect of subsidy on price ------------------------------------------

tts[, post_incentive_price_w := 0.7*(price_w - rebate_w)]
tts[, ln_rebate_w := log(rebate_w)]
tts[, ln_price_w := log(price_w)]
tts[, ln_tariff := log(tariff)]
tts[, ln_tariff_temp := log(tariff_temp)]
tts[, ln_post_incentive_price_w := log(post_incentive_price_w)]

set_control = c("median_home_value + median_household_income + population_density + PV_system_size_DC + PV_system_size_DC^2 + 
                elec_price + h_median + rebate_w")

model_pass_through = list(
    # WEIRD
    "Overall" = list(
      feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_overall+ {set_control}")) , 
                         cluster = ~zip_code , data = tts[year %in% 2010:2016]),
      feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_overall+ {set_control}| year + state + installer_name")) , 
                         cluster = ~zip_code , data = tts[year %in% 2010:2016]),
      feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_overall+ {set_control}| year + state + installer_name + module_manufacturer")) , 
                         cluster = ~zip_code , data = tts)),
    
    # CONSISTENT
    "2010-2013" = list(
      feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_ad1+ {set_control}")) , 
                         cluster = ~zip_code , data = tts[year %in% 2010:2013]),
      feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_ad1+ {set_control}| year + state + installer_name")) , 
                         cluster = ~zip_code , data = tts[year %in% 2010:2013]),
      feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_ad1+ {set_control}| year + state + installer_name + module_manufacturer")), 
                         cluster = ~zip_code , data = tts[year %in% 2010:2013])),
    
     "2014-2016" = list(
       feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_ad2+ {set_control}")) , 
                         cluster = ~zip_code , data = tts[year %in% 2014:2016]),
       feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_ad2+ {set_control}| year + state + installer_name")) , 
                         cluster = ~zip_code , data = tts[year %in% 2014:2016]),
       feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_ad2+ {set_control}| year + state + installer_name + module_manufacturer")) , 
                         cluster = ~zip_code , data = tts[year %in% 2014:2016]))
)

# model_pass_through_o = model_pass_through[["Overall"]]
results_list_period <- list()
for(panel in names(model_pass_through)) {
  models_panel <- model_pass_through[[panel]]
  for(i in seq_along(models_panel)) {
    m <- models_panel[[i]]
    f_p    <- fitstat(m, type = "f")$f$p
    # wald_p <- fitstat(m, type = "wald")$wald$p
    # my = fitstat(m, type = "my")$my
    col_name <- paste0(gsub("[[:space:][:punct:]]+", "_", panel), "_model", i)
    results_list_period[[col_name]] <- c(f_p)
  }
}
df_overall_add_rows <- data.frame(
  term = c("F-test p-value"),
  results_list_period,
  check.names = FALSE
)

rename_coef = c("ln_tariff" = "log Tariffs",
                "ln_tariff:premium_panel_overall" = "log Tariffs \times Premium Overall",
                "ln_tariff:premium_panel_ad1" = "log Tariffs \times Premium First Wave AD",
                "ln_tariff:premium_panel_ad2" = "log Tariffs \times Premium Second Wave AD"
                  )

table_pt_1 = modelsummary(
  models = model_pass_through,
  stars = TRUE, 
  shape = "cbind",
  gof_omit = "Adj|AIC|BIC|Within|Pseudo|RMSE|Std.",
  add_rows = df_overall_add_rows,
  coef_map = rename_coef,
  output = "latex"
)

table_pt_1_char = as.character(table_pt_1)
writeLines(table_pt_1_char, "output/regression/pass_through_quality1.tex")
    
# Quality 2 ---------------------------------------------------------------

model_pass_through_2 = list(
  "Overall" = list(
    feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_installation+ {set_control}")) , 
          cluster = ~zip_code , data = tts[year %in% 2010:2016]),
    feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_installation+ {set_control}| year + state + installer_name")) , 
          cluster = ~zip_code , data = tts[year %in% 2010:2016]),
    feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_installation+ {set_control}| year + state + installer_name + module_manufacturer")) , 
          cluster = ~zip_code , data = tts)),
  
  "2010-2013" = list(
    feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_installation+ {set_control}")) , 
          cluster = ~zip_code , data = tts[year %in% 2010:2013]),
    feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_installation+ {set_control}| year + state + installer_name")) , 
          cluster = ~zip_code , data = tts[year %in% 2010:2013]),
    feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_installation+ {set_control}| year + state + installer_name + module_manufacturer")), 
          cluster = ~zip_code , data = tts[year %in% 2010:2013])),
  
  "2014-2016" = list(
    feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_installation+ {set_control}")) , 
          cluster = ~zip_code , data = tts[year %in% 2014:2016]),
    feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_installation+ {set_control}| year + state + installer_name")) , 
          cluster = ~zip_code , data = tts[year %in% 2014:2016]),
    feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_installation+ {set_control}| year + state + installer_name + module_manufacturer")) , 
          cluster = ~zip_code , data = tts[year %in% 2014:2016]))
)

# model_pass_through_o = model_pass_through[["Overall"]]
model_pass_through_2 <- list()
for(panel in names(model_pass_through_2)) {
  models_panel <- model_pass_through_2[[panel]]
  for(i in seq_along(models_panel)) {
    m <- models_panel[[i]]
    f_p    <- fitstat(m, type = "f")$f$p
    # wald_p <- fitstat(m, type = "wald")$wald$p
    # my = fitstat(m, type = "my")$my
    col_name <- paste0(gsub("[[:space:][:punct:]]+", "_", panel), "_model", i)
    results_list_period[[col_name]] <- c(f_p)
  }
}
df_overall_add_rows <- data.frame(
  term = c("F-test p-value"),
  results_list_period,
  check.names = FALSE
)

rename_coef = c("ln_tariff" = "log Tariffs",
                "ln_tariff:premium_installation" = "log Tariffs \times Premium Installation"
)

table_pt_2 = modelsummary(
  models = model_pass_through_2,
  stars = TRUE, 
  shape = "cbind",
  gof_omit = "Adj|AIC|BIC|Within|Pseudo|RMSE|Std.",
  add_rows = df_overall_add_rows,
  coef_map = rename_coef,
  output = "latex"
)

table_pt_2_char = as.character(table_pt_2)
writeLines(table_pt_2_char, "output/regression/pass_through_quality2.tex")

# Effect on quality change after implementation ---------------------------
quality_ad_1 = feols(as.formula(glue("premium_panel_ad1 ~ ln_tariff + {set_control}| year + state + installer_name")) , 
                     cluster = ~zip_code , data = tts[year %in% 2010:2013])
quality_2_ad1 = feols(as.formula(glue("premium_panel_ad1 ~ ln_tariff + {set_control}| year + state + installer_name + module_manufacturer")) , 
                           cluster = ~zip_code , data = tts[year %in% 2010:2013])

quality_ad_2 = feols(as.formula(glue("premium_panel_ad2 ~ ln_tariff + {set_control}| year + state + installer_name")) , 
                          cluster = ~zip_code , data = tts[year %in% 2013:2016])
quality_2_ad2 = feols(as.formula(glue("premium_panel_ad2 ~ ln_tariff + {set_control}| year + state + installer_name + module_manufacturer")) , 
                           cluster = ~zip_code , data = tts[year %in% 2013:2016])

