# This code evaluate the subsidy pass-through

library(arrow)
library(data.table)
library(fixest)
library(modelsummary)
library(glue)

# Data --------------------------------------------------------------------

tts= read_parquet(data_final("TTS_final.parquet"))

# The effect of subsidy on price ------------------------------------------

tts[, post_incentive_price_w := price_w - rebate_w]
tts[, ln_post_incentive_price_w := log(post_incentive_price_w)]
tts[, ln_rebate_w := log(rebate_w)]
tts[, ln_price_w := log(price_w)]
tts[, ln_proxy_panel_price_w := log(proxy_panel_price_w)]
tts[, ln_tariff := log(tariff)]
tts[, ln_tariff_temp := log(tariff_temp)]

set_control = c("median_home_value + median_household_income + population_density + PV_system_size_DC + PV_system_size_DC^2 + 
                elec_price + h_median + rebate_w")

model_pass_through = list(
    "Overall" = list(
      feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_overall+ {set_control}")) , 
                         cluster = ~zip_code , data = tts[year %in% 2010:2020]),
      feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_overall+ {set_control}| year_quarter + state + installer_name")) , 
                         cluster = ~zip_code , data = tts[year %in% 2010:2020]),
      feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_overall+ {set_control}| year_quarter + state + installer_name + module_manufacturer")) , 
                         cluster = ~zip_code , data = tts[year %in% 2010:2020])),
    
    "2010-2013" = list(
      feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_ad1+ {set_control}")) , 
                         cluster = ~zip_code , data = tts[year %in% 2010:2013]),
      feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_ad1+ {set_control}| year_quarter + state + installer_name")) , 
                         cluster = ~zip_code , data = tts[year %in% 2010:2013]),
      feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_ad1+ {set_control}| year_quarter + state + installer_name + module_manufacturer")), 
                         cluster = ~zip_code , data = tts[year %in% 2010:2013])),
    
     "2014-2016" = list(
       feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_ad2+ {set_control}")) , 
                         cluster = ~zip_code , data = tts[year %in% 2014:2016]),
       feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_ad2+ {set_control}| year_quarter + state + installer_name")) , 
                         cluster = ~zip_code , data = tts[year %in% 2014:2016]),
       feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_ad2+ {set_control}| year_quarter + state + installer_name + module_manufacturer")) , 
                         cluster = ~zip_code , data = tts[year %in% 2014:2016])),
    
    "2017-2020" = list(
      feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_st+ {set_control}")) , 
            cluster = ~zip_code , data = tts[year %in% 2017:2020]),
      feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_st+ {set_control}| year_quarter + state + installer_name")) , 
            cluster = ~zip_code , data = tts[year %in% 2017:2020]),
      feols(as.formula(glue("ln_post_incentive_price_w ~ ln_tariff + ln_tariff*premium_panel_st+ {set_control}| year_quarter + state + installer_name + module_manufacturer")) , 
            cluster = ~zip_code , data = tts[year %in% 2017:2020]))
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
                "ln_tariff:premium_panel_ad2" = "log Tariffs \times Premium Second Wave AD",
                "ln_tariff:premium_panel_st" = "log Tariffs \times Premium Safeguard Tariff"
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
writeLines(table_pt_1_char, "output/regression/pass_through/pass_through_quality1.tex")
    
# Quality 2

model_pass_through_2 = list(
  "Overall" = list(
    feols(as.formula(glue("ln_price_w ~ ln_tariff + ln_tariff*premium_installation+ {set_control}")) , 
          cluster = ~zip_code , data = tts[year %in% 2010:2020]),
    feols(as.formula(glue("ln_price_w ~ ln_tariff + ln_tariff*premium_installation+ {set_control}| year_quarter + state + installer_name")) , 
          cluster = ~zip_code , data = tts[year %in% 2010:2020]),
    feols(as.formula(glue("ln_price_w ~ ln_tariff + ln_tariff*premium_installation+ {set_control}| year_quarter + state + installer_name + module_manufacturer")) , 
          cluster = ~zip_code , data = tts[year %in% 2010:2020])),
  
  "2010-2013" = list(
    feols(as.formula(glue("ln_price_w ~ ln_tariff + ln_tariff*premium_installation+ {set_control}")) , 
          cluster = ~zip_code , data = tts[year %in% 2010:2013]),
    feols(as.formula(glue("ln_price_w ~ ln_tariff + ln_tariff*premium_installation+ {set_control}| year_quarter + state + installer_name")) , 
          cluster = ~zip_code , data = tts[year %in% 2010:2013]),
    feols(as.formula(glue("ln_price_w ~ ln_tariff + ln_tariff*premium_installation+ {set_control}| year_quarter + state + installer_name + module_manufacturer")), 
          cluster = ~zip_code , data = tts[year %in% 2010:2013])),
  
  "2014-2016" = list(
    feols(as.formula(glue("ln_price_w ~ ln_tariff + ln_tariff*premium_installation+ {set_control}")) , 
          cluster = ~zip_code , data = tts[year %in% 2014:2016]),
    feols(as.formula(glue("ln_price_w ~ ln_tariff + ln_tariff*premium_installation+ {set_control}| year_quarter + state + installer_name")) , 
          cluster = ~zip_code , data = tts[year %in% 2014:2016]),
    feols(as.formula(glue("ln_price_w ~ ln_tariff + ln_tariff*premium_installation+ {set_control}| year_quarter + state + installer_name + module_manufacturer")) , 
          cluster = ~zip_code , data = tts[year %in% 2014:2016])),
  
  "2017-2020" = list(
    feols(as.formula(glue("ln_price_w ~ ln_tariff + ln_tariff*premium_installation+ {set_control}")) , 
          cluster = ~zip_code , data = tts[year %in% 2017:2020]),
    feols(as.formula(glue("ln_price_w ~ ln_tariff + ln_tariff*premium_installation+ {set_control}| year_quarter + state + installer_name")) , 
          cluster = ~zip_code , data = tts[year %in% 2017:2020]),
    feols(as.formula(glue("ln_price_w ~ ln_tariff + ln_tariff*premium_installation+ {set_control}| year_quarter + state + installer_name + module_manufacturer")) , 
          cluster = ~zip_code , data = tts[year %in% 2017:2020]))
)

# model_pass_through_o = model_pass_through[["Overall"]]
# model_pass_through_2 <- list()
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
writeLines(table_pt_2_char, "output/regression/pass_through/pass_through_quality2.tex")

# Effect on quality change after implementation ---------------------------
qualit1_shift = list(
  "2010-2013" = list(
    feols(as.formula(glue("premium_panel_ad1 ~ ln_tariff + {set_control}| year + state + installer_name")) , 
                       cluster = ~zip_code , data = tts[year %in% 2010:2013]),
    feols(as.formula(glue("premium_panel_ad1 ~ ln_tariff + {set_control}| year + state + installer_name + module_manufacturer")) , 
                        cluster = ~zip_code , data = tts[year %in% 2010:2013])),
  
  "2014-2016" = list(
    feols(as.formula(glue("premium_panel_ad2 ~ ln_tariff + {set_control}| year + state + installer_name")) , 
                       cluster = ~zip_code , data = tts[year %in% 2013:2016]),
    feols(as.formula(glue("premium_panel_ad2 ~ ln_tariff + {set_control}| year + state + installer_name + module_manufacturer")) , 
                        cluster = ~zip_code , data = tts[year %in% 2013:2016])),
  
  "2017-2020" = list(feols(as.formula(glue("premium_panel_st ~ ln_tariff + {set_control}| year + state + installer_name")) , 
                     cluster = ~zip_code , data = tts[year %in% 2017:2020]),
                   feols(as.formula(glue("premium_panel_st ~ ln_tariff + {set_control}| year + state + installer_name + module_manufacturer")) , 
                       cluster = ~zip_code , data = tts[year %in% 2017:2020]))
)

quality_shift_list = list()
for(panel in names(qualit1_shift)) {
  models_panel <- qualit1_shift[[panel]]
  for(i in seq_along(models_panel)) {
    m <- models_panel[[i]]
    f_p    <- fitstat(m, type = "f")$f$p
    col_name <- paste0(gsub("[[:space:][:punct:]]+", "_", panel), "_model", i)
    quality_shift_list[[col_name]] <- c(f_p)
  }
}
df_overall_add_rows <- data.frame(
  term = c("F-test p-value"),
  quality_shift_list,
  check.names = FALSE
)

rename_coef = c("ln_tariff" = "log Tariffs")

table_quality_shift = modelsummary(
  models = qualit1_shift,
  stars = TRUE, 
  shape = "cbind",
  gof_omit = "Adj|AIC|BIC|Within|Pseudo|RMSE|Std.",
  add_rows = df_overall_add_rows,
  coef_map = rename_coef,
  output = "latex"
)

table_quality_shift_char = as.character(table_quality_shift)
writeLines(table_quality_shift_char, "output/regression/quality_shift/table_quality1_shift.tex")

# Quality 2

qualit2_shift = list(
  "2010-2013" = list(
    feols(as.formula(glue("premium_installation ~ ln_tariff + {set_control}| year + state + installer_name")) , 
          cluster = ~zip_code , data = tts[year %in% 2010:2013]),
    feols(as.formula(glue("premium_installation ~ ln_tariff + {set_control}| year + state + installer_name + module_manufacturer")) , 
          cluster = ~zip_code , data = tts[year %in% 2010:2013])),
  
  "2014-2016" = list(
    feols(as.formula(glue("premium_installation ~ ln_tariff + {set_control}| year + state + installer_name")) , 
          cluster = ~zip_code , data = tts[year %in% 2013:2016]),
    feols(as.formula(glue("premium_installation ~ ln_tariff + {set_control}| year + state + installer_name + module_manufacturer")) , 
          cluster = ~zip_code , data = tts[year %in% 2013:2016])),
  
  "2017-2020" = list(
    feols(as.formula(glue("premium_installation ~ ln_tariff + {set_control}| year + state + installer_name")) , 
                           cluster = ~zip_code , data = tts[year %in% 2017:2020]),
    feols(as.formula(glue("premium_installation ~ ln_tariff + {set_control}| year + state + installer_name + module_manufacturer")) , 
                           cluster = ~zip_code , data = tts[year %in% 2017:2020]))
)

quality_shift_list = list()
for(panel in names(qualit2_shift)) {
  models_panel <- qualit2_shift[[panel]]
  for(i in seq_along(models_panel)) {
    m <- models_panel[[i]]
    f_p    <- fitstat(m, type = "f")$f$p
    col_name <- paste0(gsub("[[:space:][:punct:]]+", "_", panel), "_model", i)
    quality_shift_list[[col_name]] <- c(f_p)
  }
}
df_overall_add_rows <- data.frame(
  term = c("F-test p-value"),
  quality_shift_list,
  check.names = FALSE
)

rename_coef = c("ln_tariff" = "log Tariffs")

table_quality2_shift = modelsummary(
  models = qualit2_shift,
  stars = TRUE, 
  shape = "cbind",
  gof_omit = "Adj|AIC|BIC|Within|Pseudo|RMSE|Std.",
  add_rows = df_overall_add_rows,
  coef_map = rename_coef,
  output = "latex"
)

table_quality2_shift_char = as.character(table_quality2_shift)
writeLines(table_quality2_shift_char, "output/regression/quality_shift/table_quality2_shift.tex")

# # Test --------------------------------------------------------------------
# panel_firm_qtr <- tts[
#   , .(avg_ln_price = mean(ln_price_w, na.rm = TRUE),
#       ln_tariff = ln_tariff),
#   by = .(module_manufacturer, year_quarter)
# ]
# chinese = c("canadian solar", "jinko solar", "trina solar", "yingli energy (china)")
# feols(avg_ln_price ~ ln_tariff | module_manufacturer + year_quarter, data = panel_firm_qtr[module_manufacturer %in% chinese & year_quarter < "2017Q4"])
# 
