# We have tariff pass through effect here

library(arrow)
library(data.table)
library(fixest)
library(fplot)
library(modelsummary)
library(tibble)

# Data --------------------------------------------------------------------

tts = setDT(read_parquet(data_final("tts_final.parquet")))

# Pass-Through Estimation ----------------------------------------------------------------
tts[, net_price := price_w - rebate_w]
tts[, year_origin := paste0(year, origin)]
tts[, quarter_origin := paste0(year_quarter, origin)]

tts = tts[rebate_w < price_w]
system_vars <- c("PV_system_size_DC", "I(PV_system_size_DC^2)", "elec_price", "mean_week_wage")
dem_vars <- c("population_density", "pct_bachelor_estimate", "median_home_value", "median_household_income")

# Main interaction term
tts[, ln_tariff := log(tariff)]
base_rhs <- "ln_tariff*premium_panel_overall + ln_tariff*premium_installation"

# Combine everything
rhs <- paste(c(base_rhs, system_vars, dem_vars), collapse = " + ")

for (p in c("log(price_w) ~", "log(net_price) ~")){
  # Changing for net price to gross price influence the results for AD2010-2013
  full_formula_str <- paste(p, rhs) 
  
  # Convert to formula
  full_formula <- as.formula(full_formula_str)
  
  tariff_pt =  list(
    
    "Overall" = list(
      feols(full_formula, fixef = c("year_quarter","county", "installer_name", "origin"), cluster = ~ zip_code, data = tts),
      feols(full_formula, fixef = c("year_quarter","county","installer_name", "quarter_origin"), cluster = ~ zip_code, data = tts)
      ),
    
    "Anti-Dumping : 2010 - 2013" = list(
      feols(full_formula, fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2010:2013]),
      feols(full_formula, fixef = c("year_quarter", "county","installer_name", "quarter_origin"), cluster = ~ zip_code, data = tts[year %in% 2010:2013])
      ),
    
    "Anti-Dumping : 2014 - 2016" = list(
      feols(full_formula, fixef =   c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code,data = tts[year %in% 2013:2016]),
      feols(full_formula, fixef =   c("year_quarter", "county","installer_name", "quarter_origin"), cluster = ~ zip_code,data = tts[year %in% 2013:2016])
      ),
    
    "Trade War 2018" = list(
      feols(full_formula, fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2017:2018]),
      feols(full_formula, fixef = c("year_quarter", "county","installer_name", "quarter_origin"), cluster = ~ zip_code, data = tts[year %in% 2017:2018])
      )
  )
  
  coef_name = c(
    "ln_tariff" = "ln Tariff",
    "ln_tariff:premium_panel_overall" = "ln Tariff x Premium Panel",
    "ln_tariff:premium_installation" = "ln Tariff x Premium Installation"
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
  
  pass_through = modelsummary(
    models = tariff_pt,
    stars = TRUE,
    shape = "cbind",
    coef_map = coef_name,
    gof_omit = "Adj|AIC|BIC|Within|Pseudo|RMSE|Std.",
    gof_map = gof_list,
    output = "latex"
    )
  if (p == "log(net_price) ~"){
    writeLines(as.character(pass_through), "output/regression/pass_through/pass_through_netprice.tex")}
  else {
    writeLines(as.character(pass_through), "output/regression/pass_through/pass_through_grossprice.tex")
  }
}

# Heterogeneity of Price Increase Chinese Tariff ----------------------------------

# We build a metric of competition with Chinese manufacturer : share in a county of Chinese firms
# Relief in competition could also be approximated by HHI variation

firm_share = tts[, .(year, county, origin)]
firm_share[, total_install_year := .N, by = .(year, county)]
firm_share[, install_firm_year := .N, by = .(year, county, origin)]
firm_share = unique(firm_share[, mkt_share := install_firm_year/total_install_year])
setorder(firm_share, year)
firm_share[, hhi := sum(mkt_share^2), by = .(year, county)]

firm_share[, china := ifelse(origin == "china", 1, 0)]
firm_share[, china := NULL]
china_share = firm_share[origin == "china", .(
  tariff_exposure = sum(mkt_share)  # sum of Chinese firms market share in (year, county)
), by = .(year, county)]


tts[, tract := NULL]
tts = merge(tts, firm_share, by = c("year", "county", "origin"))
tts = merge(tts, china_share, by = c("year", "county"))
tts[, post_ad1 := ifelse(year_quarter >= "2012 Q2" & year_quarter <= "2013 Q4", 1, 0)]
tts[, post_ad2 := ifelse(year_quarter >= "2014 Q2", 1, 0)]

# Main interaction term
base_rhs <- "tariff_exposure*post_ad1 + tariff_exposure*post_ad2"  

# Combine everything
rhs <- paste(c(base_rhs, system_vars, dem_vars), collapse = " + ")
full_formula = as.formula(paste0("log(price_w) ~ ", rhs ))

# Model
hetero_pt =  list(
  "Overall" = list(
  "Full Sample" = feols(full_formula, fixef = c("year_quarter","county", "installer_name"), 
                        cluster = ~ zip_code, data = tts),
  "Excluding China"  = feols(full_formula, fixef = c("year_quarter","county","installer_name"), 
                             cluster = ~ zip_code, data = tts[origin != "china"])
  ),
  
  "Anti-Dumping : 2010 - 2013" = list(
  "Full Sample" = feols(full_formula, fixef = c("year_quarter", "county", "installer_name"), 
          cluster = ~ zip_code, data = tts[year %in% 2010:2013]),
  "Excluding China" = feols(full_formula, fixef = c("year_quarter", "county","installer_name"), 
          cluster = ~ zip_code, data = tts[year %in% 2010:2013 & origin != "china"])
  ),
  
  "Anti-Dumping : 2014 - 2016" = list(
    "Full Sample" = feols(full_formula, fixef =   c("year_quarter", "county", "installer_name"), 
          cluster = ~ zip_code,data = tts[year %in% 2013:2016]),
    "Excluding China" = feols(full_formula, fixef =   c("year_quarter", "county","installer_name"), 
          cluster = ~ zip_code,data = tts[year %in% 2013:2016 & origin != "china"])
  ),
  
  "Trade War 2018" = list(
    "Full Sample" = feols(full_formula, fixef = c("year_quarter", "county", "installer_name"), 
          cluster = ~ zip_code, data = tts[year %in% 2017:2018]),
    "Excluding China" = feols(full_formula, fixef = c("year_quarter", "county","installer_name"), 
          cluster = ~ zip_code, data = tts[year %in% 2017:2018 & origin != "china"])
  )
)

coef_name = c(
  "log(tariff)" = "ln Tariff",
  "log(tariff):china" = "ln Tariff x China",
  "china" = "China",
  "tariff_exposure" = "Exposure to Tariff",
  "tariff_exposure:post_ad1" = "Exposure to Tariff x Post AD 1",
  "tariff_exposure:post_ad2" = "Exposure to Tariff x Post AD 2"
)

hetero_pt_table = modelsummary(
  model = hetero_pt,
  star = TRUE,
  shape = "cbind",
  coef_map = coef_name,
  gof_map = gof_list,
  # output = "latex"
)

# Map ---------------------------------------------------------------------
# We take the least exposed county as a reference point
exposure_2011 = tts[year == 2011, .(avg_exposure = mean(tariff_exposure, na.rm = TRUE)), by = county]
least_exposed_county = exposure_2011[which.min(avg_exposure), county]

tts[, county_factor := factor(county)]
tts[, county_factor := relevel(county_factor, ref = least_exposed_county)]

model = feols(
  log(price_w) ~ log(tariff)*county_factor | year_quarter + installer_name + origin,
  cluster = ~zip_code,
  data = tts[year %in% 2010:2013]
)

coefs = broom::tidy(model)  # Tidy output
coefs_county = coefs[grep("log(tariff):county", coefs$term), ]  #

# Main interaction term
base_rhs = "log(tariff)*county"  

# Combine everything
rhs <- paste(c(base_rhs, system_vars, dem_vars), collapse = " + ")
full_formula = as.formula(paste0("log(price_w) ~ ", rhs ))

hetero_map =  list(
  "Overall" = list(
    feols(full_formula, fixef = c("year_quarter", "installer_name", "origin"), cluster = ~ zip_code, data = tts),
    feols(full_formula, fixef = c("year_quarter","installer_name", "quarter_origin"), cluster = ~ zip_code, data = tts)
  ),
  
  "Anti-Dumping : 2010 - 2013" = list(
    feols(full_formula, fixef = c("year_quarter", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2010:2013]),
    feols(full_formula, fixef = c("year_quarter","installer_name", "quarter_origin"), cluster = ~ zip_code, data = tts[year %in% 2010:2013])
  ),
  
  "Anti-Dumping : 2014 - 2016" = list(
    feols(full_formula, fixef =   c("year_quarter", "installer_name", "origin"), cluster = ~ zip_code,data = tts[year %in% 2013:2016]),
    feols(full_formula, fixef =   c("year_quarter","installer_name", "quarter_origin"), cluster = ~ zip_code,data = tts[year %in% 2013:2016])
  ),
  
  "Trade War 2018" = list(
    feols(full_formula, fixef = c("year_quarter",  "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2017:2018]),
    feols(full_formula, fixef = c("year_quarter", "installer_name", "quarter_origin"), cluster = ~ zip_code, data = tts[year %in% 2017:2018])
  )
)

test = feols(full_formula, fixef =   c("year_quarter", "installer_name", "origin"), cluster = ~ zip_code,data = tts[year %in% 2013:2016])
summary(test)
