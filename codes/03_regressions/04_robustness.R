# This code implement robustness check for the pass-through estimation

library(arrow)
library(data.table)
library(fixest)
library(modelsummary)
library(tibble)

# Data --------------------------------------------------------------------

rob = setDT(read_parquet(data_final("tts_final.parquet")))

# AD1 - Pass-Through Robustness-------------------------------------------------

rob[, net_price := price_w - rebate_w]
rob[, year_origin := paste0(year, origin)]
rob[, quarter_origin := paste0(year_quarter, origin)]
rob[, ln_tariff := log(tariff)]

mkt = rob[, .(origin, year)]
mkt[, sum_install := .N, by = year]
mkt[, sum_install_origin := .N,  by = .(origin, year)]
mkt[, market_share_year:= sum_install_origin/sum_install]
setorder(mkt, year)
mkt = unique(na.omit(mkt[, .(year, market_share_year,origin)]))

mkt_comp = rob[, .(origin, year, module_manufacturer)]
mkt_comp[, sum_install := .N, by = year]
mkt_comp[, sum_install_origin := .N,  by = .(module_manufacturer, year)]
mkt_comp[, market_share_year_comp:= sum_install_origin/sum_install]
setorder(mkt_comp, year)
mkt_comp = unique(na.omit(mkt_comp[, .(year, market_share_year_comp, module_manufacturer, origin)]))

rob[, tract := NULL]
rob = merge(rob, mkt, by = c("origin", "year"), all.x = TRUE)
rob = merge(rob, mkt_comp, by = c("module_manufacturer", "year"), all.x = TRUE)
rob[, origin.y := NULL]
setnames(rob, "origin.x", "origin")

# Average market share over 2010-2011 for each module_manufacturer
pre_tariff_share = rob[year %in% 2010:2011, 
                       .(pre_mkt_share = mean(market_share_year_comp, na.rm = TRUE)),
                       by = .(module_manufacturer)]
rob = merge(rob, pre_tariff_share, by = "module_manufacturer", all.x = TRUE)
rob_post_ad1 = rob[origin == "china" & year %in% 2012:2013]

## IV  ---------------------------------------------------------------------
# We instrument the market share of Chinese firm in 2010 to predict tariff and then price increase

iv = feols(
  log(net_price) ~ PV_system_size_DC + I(PV_system_size_DC^2) + elec_price + mean_week_wage +
    population_density + pct_bachelor_estimate + median_home_value + median_household_income
  | county + year_quarter + installer_name
  | ln_tariff ~ pre_mkt_share,
  cluster = ~zip_code,
  data = rob_post_ad1
)

gof_list <- tribble(
  ~raw,                  ~clean,           ~fmt,
  "nobs",                "Num.Obs",        "%.0f",
  "r.squared",           "R2",             "%.3f",
  "adj.r.squared",       "R2-Adj.",        "%.3f",
  "FE: county",          "FE: County",         "%.0f",
  "FE: year_quarter",    "FE: Quarter",        "%.0f",
  "FE: installer_name",  "FE: Installer",      "%.0f",
  "FE: origin",          "FE: Origin",         "%.0f",
  "FE: year",            "FE: Year",           "%.0f",
  "FE: year_origin",     "FE: Year × Origin",  "%.0f",
  "FE: quarter_origin",  "FE: Quarter × Origin",  "%.0f"
)

iv_clean = list(
    # First stage
    "First Stage" = summary(iv, stage = 1),
    # Second stage
    "Second Stage" = iv
    )


coef_name = c(
  "fit_ln_tariff" = "Fitted Resid. Ln Tariff",
  "pre_mkt_share" = "Pre-Tariff Market Share"
)

iv_ad1 = modelsummary(
  model = iv_clean,
  star = TRUE,
  shape = "rbind",
  gof_map = gof_list,
  coef_map = coef_name,
  output = "latex"
  )

writeLines(as.character(iv_ad1), "output/regression/robustness/iv_ad1.tex")


# Shuffling tariff -----------------------------------------------

treated_brands = unique(rob[origin == "china", (module_manufacturer)])
set.seed(123)
rob[, placebo_tariff := tariff] 
rob[!module_manufacturer %in% treated_brands, placebo_tariff := sample(tariff, .N, replace = FALSE)]

system_vars <- c("PV_system_size_DC", "I(PV_system_size_DC^2)", "elec_price", "mean_week_wage")
dem_vars <- c("population_density", "pct_bachelor_estimate", "median_home_value", "median_household_income")

# Main interaction term
rob[, ln_placebo_tariff := log(placebo_tariff)]
base_rhs <- "ln_placebo_tariff*premium_panel_overall + ln_placebo_tariff*premium_installation"

# Combine everything
rhs <- paste(c(base_rhs, system_vars, dem_vars), collapse = " + ")

for (p in c("log(price_w) ~","log(net_price) ~")){
    # Changing for net price to gross price influence the results for AD2010-2013
    full_formula_str <- paste(p, rhs) 
    
    # Convert to formula
    full_formula <- as.formula(full_formula_str)
    
    placebo_pt =  list(
      
      "Overall" = list(
        feols(full_formula, fixef = c("year_quarter","county", "installer_name", "origin"), cluster = ~ zip_code, data = rob),
        feols(full_formula, fixef = c("year_quarter","county","installer_name", "quarter_origin"), cluster = ~ zip_code, data = rob)
      ),
      
      "Anti-Dumping : 2010 - 2013" = list(
        feols(full_formula, fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = rob[year %in% 2010:2013]),
        feols(full_formula, fixef = c("year_quarter", "county","installer_name", "quarter_origin"), cluster = ~ zip_code, data = rob[year %in% 2010:2013])
      ),
      
      "Anti-Dumping : 2014 - 2016" = list(
        feols(full_formula, fixef =   c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code,data = rob[year %in% 2013:2016]),
        feols(full_formula, fixef =   c("year_quarter", "county","installer_name", "quarter_origin"), cluster = ~ zip_code,data = rob[year %in% 2013:2016])
      ),
      
      "Trade War 2018" = list(
        feols(full_formula, fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = rob[year %in% 2017:2018]),
        feols(full_formula, fixef = c("year_quarter", "county","installer_name", "quarter_origin"), cluster = ~ zip_code, data = rob[year %in% 2017:2018])
      )
    )
  
  coef_name = c(
    "ln_placebo_tariff" = "ln Placebo Tariff",
    "ln_placebo_tariff:premium_panel_overall" = "ln Placebo Tariff x Premium Panel",
    "ln_placebo_tariff:premium_installation" = "ln Placebo Tariff x Premium Installation"
  )
  
  gof_list <- tribble(
    ~raw,                  ~clean,           ~fmt,
    "nobs",                "Num.Obs",        "%.0f",
    "r.squared",           "R2",             "%.3f",
    "adj.r.squared",       "R2-Adj.",        "%.3f",
    "FE: county",          "FE: County",         "%.0f",
    "FE: year_quarter",    "FE: Quarter",        "%.0f",
    "FE: installer_name",  "FE: Installer",      "%.0f",
    "FE: origin",          "FE: Origin",         "%.0f",
    "FE: year",            "FE: Year",           "%.0f",
    "FE: year_origin",     "FE: Year × Origin",  "%.0f",
    "FE: quarter_origin",  "FE: Quarter × Origin",  "%.0f"
  )
  
  placebo_pass_through = modelsummary(
    models = placebo_pt,
    stars = TRUE,
    shape = "cbind",
    coef_map = coef_name,
    gof_omit = "Adj|AIC|BIC|Within|Pseudo|RMSE|Std.",
    gof_map = gof_list,
    output = "latex"
  )
  if (p == "log(net_price) ~"){
    writeLines(as.character(placebo_pass_through), "output/regression/pass_through/placebo_pass_through_netprice.tex")}
  else {
    writeLines(as.character(placebo_pass_through), "output/regression/pass_through/placebo_pass_through_grossprice.tex")
  }
}

# Draft -------------------------------------------------------------------

# # AD2 - Price Variation Competition ---------------------------------------
# # In this section we use another IV to predict the variation in prices of non affected firms
# # The idea is to see more general effect of the implementation of tariff on price change
# # Average market share over 2010-2011 for each module_manufacturer
# pre_tariff_share_ad2 = rob[year %in% 2013:2014, .(pre_mkt_share = mean(market_share_year_comp, na.rm = TRUE)), by = .(module_manufacturer)]
# rob = merge(rob, pre_tariff_share_ad2, by = "module_manufacturer", all.x = TRUE)
# rob_post_ad2 = rob[origin == "china" & year %in% 2015:2016]
# 
# ## IV  ---------------------------------------------------------------------
# # We instrument the market share of Chinese firm in 2010 to predict tariff and then price increase
# 
# iv_2 = feols(
#   log(net_price) ~ PV_system_size_DC + I(PV_system_size_DC^2) + elec_price + mean_week_wage +
#     population_density + pct_bachelor_estimate + median_home_value + median_household_income
#   | county + year_quarter + installer_name
#   | ln_tariff ~ pre_mkt_share,
#   cluster = ~zip_code,
#   data = rob_post_ad2
# )
# 
# gof_list <- tribble(
#   ~raw,                  ~clean,           ~fmt,
#   "nobs",                "Num.Obs",        "%.0f",
#   "r.squared",           "R2",             "%.3f",
#   "adj.r.squared",       "R2-Adj.",        "%.3f",
#   "FE: county",          "FE: County",         "%.0f",
#   "FE: year_quarter",    "FE: Quarter",        "%.0f",
#   "FE: installer_name",  "FE: Installer",      "%.0f",
#   "FE: origin",          "FE: Origin",         "%.0f",
#   "FE: year",            "FE: Year",           "%.0f",
#   "FE: year_origin",     "FE: Year × Origin",  "%.0f",
#   "FE: quarter_origin",  "FE: Quarter × Origin",  "%.0f"
# )
# 
# iv_clean_2 = list(
#   # First stage
#   "First Stage" = summary(iv_2, stage = 1),
#   # Second stage
#   "Second Stage" = iv_2
# )
# 
# coef_name = c(
#   "fit_ln_tariff" = "Fitted Resid. Ln Tariff",
#   "pre_mkt_share" = "Pre-Tariff Market Share"
# )
# 
# iv_ad1 = modelsummary(
#   model = iv_clean_2,
#   star = TRUE,
#   shape = "rbind",
#   gof_map = gof_list,
#   coef_map = coef_name,
#   # output = "latex"
# )
# 
# writeLines(as.character(iv_ad1), "output/regression/robustness/iv_ad1.tex")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# instrument_ad2 = rob[, .(origin, year, county)]
# instrument_ad2[, sum_install := .N, by = .(year, county)]
# instrument_ad2[, sum_install_origin := .N,  by = .(origin, county, year)]
# instrument_ad2[, mkt_share_county_origin:= sum_install_origin/sum_install]
# setorder(instrument_ad2, year)
# instrument_ad2 = unique(na.omit(instrument_ad2[, .(year, mkt_share_county_origin, county, origin)]))
# 
# exposure = instrument_ad2[year %in% 2013 & origin == "china"]
# county_lacking = data.table(county = setdiff(unique(instrument_ad2$county), exposure$county))
# exposure_full = rbind(exposure, county_lacking, fill = TRUE)
# exposure_full[is.na(mkt_share_county_origin), mkt_share_county_origin := 0]
# exposure_full[is.na(origin), origin := "china"]
# 
# export_graph_start(file = "output/regression/robustness/exposure_2013_chinese.pdf", pt = 10)
# plot_distr(
#   ~ mkt_share_county_origin,
#   data = exposure_full,
#   nbins = 40,
#   col = "#2E86AB",
#   border = "white",
#   dict = c(mkt_share_county_origin = "Chinese Market Share (%)"),
#   top = "none",
#   labels.angle = 45,
#   cex.axis = 0.8
# )
# export_graph_end()
# 
# # Merge exposure to full dataset
# rob_post_ad2 = rob[year %in% 2014:2016 & origin != "china",]
# rob_post_ad2 = merge(rob_post_ad2, exposure_full, by = c("county"), all.x = TRUE)
# 
# feols(PV_system_size_DC + I(PV_system_size_DC^2) + elec_price + mean_week_wage +
#         population_density + pct_bachelor_estimate + median_home_value + median_household_income
#       | county + year_quarter + installer_name
#       cluster= ~zip_code, data = rob_post_ad2)
# 
# 
# 
# rob_post_ad2[year == 2013, treated_high := ifelse(mkt_share_county_origin > median(mkt_share_county_origin),1, 0)]
# treated_county = unique(rob_post_ad2[treated_high == 1]$county)
# rob_post_ad2[year %in% 2014:2016, treated_high := ifelse(county %in% treated_county, 1, 0)]
# rob_post_ad2[, post := ifelse(year_quarter >= "2014 Q2", 1,0)]
# rob_post_ad2[, distance_treat := year_quarter -  ]
# 
# did = feols(
#   log(price_w) ~ treated_high*post
#   | year_quarter + installer_name + county,
#   cluster = ~zip_code,
#   data = rob_post_ad2
# )