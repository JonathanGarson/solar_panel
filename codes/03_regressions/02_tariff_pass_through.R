# We have tariff pass through effect here

library(arrow)
library(data.table)
library(fixest)
library(fplot)
library(modelsummary)
library(tibble)
library(zipcodeR)
library(sf)
library(ggplot2)
library(kableExtra)
library(dplyr)

# Data --------------------------------------------------------------------

tts = setDT(read_parquet(data_final("tts_final.parquet")))
# usa_county <- st_read(data_raw("us-county-boundaries/us-county-boundaries.shp"))

# Pass-Through Estimation ----------------------------------------------------------------
tts[, net_price := price_w - rebate_w]
tts[, year_origin := paste0(year, origin)]
tts[, quarter_origin := paste0(year_quarter, origin)]
tts[, year_origin := paste0(year, origin)]
tts[, quarter_installer := paste0(year_quarter, installer_name)]
tts[, year_installer := paste0(year, installer_name)]

tts = tts[rebate_w < price_w]
system_vars <- c("PV_system_size_DC", "I(PV_system_size_DC^2)", "elec_price", "mean_week_wage")
dem_vars <- c("population_density", "pct_bachelor_estimate", "median_home_value", "median_household_income")

# Main interaction term
tts[, ln_tariff := log(tariff)]

base_rhs <- "ln_tariff*premium_panel_overall + ln_tariff*premium_installation"

# Combine everything
rhs <- paste(c(base_rhs, system_vars, dem_vars), collapse = " + ")

for (p in c( "log(net_price) ~", "log(price_w) ~")){
  # Changing for net price to gross price influence the results for AD2010-2013
  # p = "log(price_w) ~"
  full_formula_str <- paste(p, rhs) 
  
  # Convert to formula
  full_formula <- as.formula(full_formula_str)
  
  tariff_pt =  list(
    
    "Overall" = list(
      feols(full_formula, fixef = c("year_quarter","county", "installer_name", "origin"), cluster = ~ zip_code, data = tts),
      feols(full_formula, fixef = c("county", "quarter_installer", "quarter_origin"), cluster = ~ zip_code, data = tts)
      ),
    
    "Anti-Dumping : 2010 - 2013" = list(
      feols(full_formula, fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2010:2013]),
      feols(full_formula, fixef = c("county", "quarter_installer", "quarter_origin"), cluster = ~ zip_code, data = tts[year %in% 2010:2013])
      ),
    
    "Anti-Dumping : 2014 - 2016" = list(
     feols(full_formula, fixef =   c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code,data = tts[year %in% 2013:2016]),
     feols(full_formula, fixef =   c("county", "quarter_installer", "quarter_origin"), cluster = ~ zip_code,data = tts[year %in% 2013:2016])
      ),
    
    "Trade War 2018" = list(
     feols(full_formula, fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2017:2018]),
     feols(full_formula, fixef = c("county", "quarter_installer", "quarter_origin"), cluster = ~ zip_code, data = tts[year %in% 2017:2018])
      )
  )
  
  coef_name = c(
    "ln_tariff" = "ln Tariff",
    "ln_tariff:efficiency_module" = "ln Tariff x Efficiency",
    "ln_tariff:premium_panel_overall" = "ln Tariff x Premium Panel",
    "ln_tariff:premium_installation" = "ln Tariff x Premium Installation",
    "treated" = "Treated",
    "treated:ln_tariff"= "Treated x ln Tariff" 
  )
 
   gof_list <- tribble(
    ~raw,                  ~clean,           ~fmt,
    "nobs",                "Num.Obs",        "%.0f",
    "r.squared",           "R2",             "%.3f",
    "adj.r.squared",       "R2-Adj.",        "%.3f",
    "FE: county",          "FE: County",         "%.0f",
    "FE: year_quarter",    "FE: Year-Quarter",        "%.0f",
    "FE: installer_name",  "FE: Installer",      "%.0f",
    "FE: origin",          "FE: Origin",         "%.0f",
    "FE: year",            "FE: Year",           "%.0f",
    "FE: year_origin",     "FE: Year × Origin",  "%.0f",
    "FE: quarter_origin",  "FE: Year-Quarter × Origin",  "%.0f",
    "FE: quarter_installer",  "FE: Year-Quarter × Installer",  "%.0f",
    "FE: year_origin",  "FE: Year × Origin",  "%.0f",
    "FE: year_installer",  "FE: Year × Installer",  "%.0f"
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

# Second Metrics Quality --------------------------------------------------

base_rhs <- "ln_tariff*efficiency_module + ln_tariff*premium_installation"
rhs <- paste(c(base_rhs, system_vars, dem_vars), collapse = " + ")

for (p in c( "log(net_price) ~","log(price_w) ~")){
  # Changing for net price to gross price influence the results for AD2010-2013
  # p = "log(price_w) ~"
  full_formula_str <- paste(p, rhs) 
  
  # Convert to formula
  full_formula <- as.formula(full_formula_str)
  
  tariff_efficiency_pt =  list(
    
    "Overall" = list(
      feols(full_formula, fixef = c("year_quarter","county", "installer_name", "origin"), cluster = ~ zip_code, data = tts),
      feols(full_formula, fixef = c("county","quarter_installer", "quarter_origin"), cluster = ~ zip_code, data = tts)
    ),
    
    "Anti-Dumping : 2010 - 2013" = list(
    # "2012" = feols(full_formula, fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2012]),
    feols(full_formula, fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2010:2013]),
    feols(full_formula, fixef = c("county","quarter_installer", "quarter_origin"), cluster = ~ zip_code, data = tts[year %in% 2010:2013])
    ),
    
    "Anti-Dumping : 2014 - 2016" = list(
    # "2014" = feols(full_formula, fixef =   c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code,data = tts[year %in% 2014]),
    feols(full_formula, fixef =   c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code,data = tts[year %in% 2013:2016]),
    feols(full_formula, fixef =   c("county","quarter_installer", "quarter_origin"), cluster = ~ zip_code,data = tts[year %in% 2013:2016])
    ),
    
    "Trade War 2018" = list(
     feols(full_formula, fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2017:2018]),
     feols(full_formula, fixef = c("county","quarter_installer", "quarter_origin"), cluster = ~ zip_code, data = tts[year %in% 2017:2018])
    )
  )
  
  model_labels <- c(
    "Overall (1)", "Overall (2)",
    "AD 2010–2013 (1)", "AD 2010–2013 (2)",
    "AD 2014–2016 (1)", "AD 2014–2016 (2)",
    "Trade War 2018 (1)", "Trade War 2018 (2)"
  )
  
  # Periods corresponding to each model
  periods <- list(
    "Overall" = tts,
    "AD1" = tts[year %in% 2010:2013],
    "AD2" = tts[year %in% 2014:2016],
    "TW"  = tts[year %in% 2017:2018]
  )
  
  # Compute min–max efficiency for each period
  minmax <- lapply(periods, function(dt) {
    rng <- range(dt$efficiency_module, na.rm = TRUE)
    sprintf("%.2f–%.2f", rng[1], rng[2])
  })
  
  eff_row <- data.frame(
    term = "Min-Max Efficiency",
    `Overall (1)` = minmax$Overall,
    `Overall (2)` = minmax$Overall,
    `AD 2010–2013 (1)` = minmax$AD1,
    `AD 2010–2013 (2)` = minmax$AD1,
    `AD 2014–2016 (1)` = minmax$AD2,
    `AD 2014–2016 (2)` = minmax$AD2,
    `Trade War 2018 (1)` = minmax$TW,
    `Trade War 2018 (2)` = minmax$TW,
    check.names = FALSE
  )
  
  pass_efficiency_through = modelsummary(
    models = tariff_efficiency_pt,
    stars = TRUE,
    shape = "cbind",
    coef_map = coef_name,
    gof_omit = "Adj|AIC|BIC|Within|Pseudo|RMSE|Std.",
    gof_map = gof_list,
    add_row = eff_row,
    output = "latex"
  )
  
  if (p == "log(net_price) ~"){
    writeLines(as.character(pass_efficiency_through), "output/regression/pass_through/pass_through_efficency_netprice.tex")}
  else {
    writeLines(as.character(pass_efficiency_through), "output/regression/pass_through/pass_through_efficiency_grossprice.tex")
  }
}

# Heterogeneity of Price - Declining Tariff -------------------------------
p = "log(price_w) ~"
base_rhs_1 <- "ln_tariff*premium_panel_overall + ln_tariff*premium_installation"
rhs_1 <- paste(c(base_rhs_1, system_vars, dem_vars), collapse = " + ")
full_formula_str_1 <- paste(p, rhs_1) 
full_formula_1 <- as.formula(full_formula_str_1)

base_rhs_2 <- "ln_tariff*efficiency_module + ln_tariff*premium_installation"
rhs_2 <- paste(c(base_rhs_2, system_vars, dem_vars), collapse = " + ")
full_formula_str_2 <- paste(p, rhs_2) 
full_formula_2 <- as.formula(full_formula_str_2)

tariff_reduc_pt =  list(
  "Trade War Descalation 2018" = list(
    feols(full_formula_1, fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[origin != "china" & year %in% 2018:2020]),
    feols(full_formula_1, fixef = c("county","quarter_installer", "quarter_origin"), cluster = ~ zip_code, data = tts[origin != "china" & year %in% 2018:2020]),
    feols(full_formula_2, fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[origin != "china" & year %in% 2018:2020]),
    feols(full_formula_2, fixef = c("county","quarter_installer", "quarter_origin"), cluster = ~ zip_code, data = tts[origin != "china" & year %in% 2018:2020])
  )
)

tariff_reduc = modelsummary(
  models = tariff_reduc_pt,
  stars = TRUE,
  shape = "cbind",
  coef_map = coef_name,
  gof_omit = "Adj|AIC|BIC|Within|Pseudo|RMSE|Std.",
  gof_map = gof_list,
  output = "latex"
)

writeLines(as.character(tariff_reduc), "output/regression/pass_through/tarif_reduc.tex")

# Heterogeneity - Treatment Intensity -------------------------------------
base_rhs <- "treated*ln_tariff"
rhs <- paste(c(base_rhs, system_vars, dem_vars), collapse = " + ")

for (p in c( "log(net_price) ~","log(price_w) ~")){
  # Changing for net price to gross price influence the results for AD2010-2013
  p = "log(price_w) ~"
  full_formula_str <- paste(p, rhs) 
  
  # Convert to formula
  full_formula <- as.formula(full_formula_str)
  
  tariff_intensity_pt =  list(
    
    "Overall" = list(
      feols(full_formula, fixef = c("year_quarter","county", "installer_name", "origin"), cluster = ~ zip_code, data = tts),
      feols(full_formula, fixef = c("county","year_installer", "year_origin"), cluster = ~ zip_code, data = tts)
    ),
    
    "Anti-Dumping : 2010 - 2013" = list(
      # "2012" = feols(full_formula, fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2012]),
      feols(full_formula, fixef = c("year", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2010:2013]),
      feols(full_formula, fixef = c("county","year_installer", "year_origin"), cluster = ~ zip_code, data = tts[year %in% 2010:2013])
    ),
    
    "Anti-Dumping : 2014 - 2016" = list(
      # "2014" = feols(full_formula, fixef =   c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code,data = tts[year %in% 2014]),
      feols(full_formula, fixef =   c("year", "county", "installer_name", "origin"), cluster = ~ zip_code,data = tts[year %in% 2013:2016]),
      feols(full_formula, fixef =   c("county","year_installer", "year_origin"), cluster = ~ zip_code,data = tts[year %in% 2013:2016])
    ),
    
    "Trade War 2018" = list(
      feols(full_formula, fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2017:2018]),
      feols(full_formula, fixef = c("county","quarter_installer", "quarter_origin"), cluster = ~ zip_code, data = tts[year %in% 2017:2018])
    )
  )
  
  pass_intensity_through = modelsummary(
    models = tariff_intensity_pt,
    stars = TRUE,
    shape = "cbind",
    coef_map = coef_name,
    gof_omit = "Adj|AIC|BIC|Within|Pseudo|RMSE|Std.",
    gof_map = gof_list,
    # output = "latex"
  )
  
  if (p == "log(net_price) ~"){
    writeLines(as.character(pass_efficiency_through), "output/regression/pass_through/pass_through_efficency_netprice.tex")}
  else {
    writeLines(as.character(pass_efficiency_through), "output/regression/pass_through/pass_through_efficiency_grossprice.tex")
  }
}



# Heterogeneity of Price - Firms Transmission ----------------------------------
# FIRM WITH HIGHER MARKET SHARE OR AMERICAN BRAND (HOME BRAND EFFECT)
tts_hetero = copy(tts)
tts_hetero[,module_manufacturer := factor(module_manufacturer)]
levels(tts_hetero$module_manufacturer)
base_rhs <- "i(ln_tariff, module_manufacturer) + ln_tariff*premium_panel_overall + ln_tariff*premium_installation"

# Combine everything
rhs <- paste(c(base_rhs, system_vars, dem_vars), collapse = " + ")

p = "log(price_w) ~"
full_formula_str <- paste(p, rhs) 

full_formula <- as.formula(full_formula_str)
  
all = feols(log(price_w) ~ ln_tariff*module_manufacturer + ln_tariff*premium_panel_overall + ln_tariff*premium_installation, 
            fixef = c("year_quarter","county", "installer_name", "origin"), cluster = ~ zip_code, data = tts_hetero)
ad1 = feols(log(price_w) ~ ln_tariff*module_manufacturer + ln_tariff*premium_panel_overall + ln_tariff*premium_installation
            , fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts_hetero[year %in% 2010:2013])
ad2 = feols(log(price_w) ~ ln_tariff*module_manufacturer + ln_tariff*premium_panel_overall + ln_tariff*premium_installation
  , fixef =   c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code,data = tts_hetero[year %in% 2013:2016])
ad3 = feols(log(price_w) ~ ln_tariff*module_manufacturer + ln_tariff*premium_panel_overall + ln_tariff*premium_installation
  , fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts_hetero[year %in% 2017:2018])
  
summary(ad2)

# Poisson Estimation ------------------------------------------------------
tts[, tariff_pois := (tariff)]
base_rhs <- "tariff_pois*efficiency_module "
rhs <- paste(c(base_rhs, system_vars, dem_vars), collapse = " + ")
p = "price_w ~"
full_formula_str <- paste(p, rhs) 

# Convert to formula
full_formula <- as.formula(full_formula_str)

tariff_pt_pois =  list(
  "Overall" = list(
    fepois(full_formula, fixef = c("year_quarter","county", "installer_name", "origin"), cluster = ~ zip_code, data = tts)
    # feols(full_formula, fixef = c"year_quarter","county", "installer_name", "origin"), cluster = ~ zip_code, data = tts)
  ),
  
  "Anti-Dumping : 2010 - 2013" = list(
    "2012" = fepois(full_formula, fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2012]),
    "All" = fepois(full_formula, fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2010:2013])
  ),
  
  "Anti-Dumping : 2014 - 2016" = list(
    "2014" = fepois(full_formula, fixef =   c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code,data = tts[year %in% 2014]),
    "All" = fepois(full_formula, fixef =   c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code,data = tts[year %in% 2013:2016])
  ),
  
  "Trade War 2018" = list(
    # "2018" = feols(full_formula, fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2017:2018]),
    "All" = fepois(full_formula, fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2017:2018])
  )
)

coef_name = c(
  "ln_tariff" = "ln Tariff",
  "ln_tariff:efficiency_module" = "ln Tariff x Efficiency",
  "ln_tariff:premium_panel_overall" = "ln Tariff x Premium Panel",
  "ln_tariff:premium_installation" = "ln Tariff x Premium Installation",
  "treated" = "Treated",
  "treated:ln_tariff"= "Treated x ln Tariff",
  "tariff" = "Tariff",
  "tariff:efficiency_module" = "Tariff x Efficiency Module",
  "tariff_pois" = "Tariff",
  "tariff_pois:efficiency_module" = "Tariff x Efficiency Module"
)

gof_list <- tribble(
  ~raw,                  ~clean,           ~fmt,
  "nobs",                "Num.Obs",        "%.0f",
  "r.squared",           "R2",             "%.3f",
  "adj.r.squared",       "R2-Adj.",        "%.3f",
  "FE: county",          "FE: County",         "%.0f",
  "FE: year_quarter",    "FE: Year-Quarter",        "%.0f",
  "FE: installer_name",  "FE: Installer",      "%.0f",
  "FE: origin",          "FE: Origin",         "%.0f",
  "FE: year",            "FE: Year",           "%.0f",
  "FE: year_origin",     "FE: Year × Origin",  "%.0f",
  "FE: quarter_origin",  "FE: Year-Quarter × Origin",  "%.0f",
  "AIC", "AIC", "%.1f",
  "pseudo.r.squared", "R2 Pseudo", "%.3f"
)

modelsummary(
  models = tariff_pt_pois,
  stars = TRUE,
  shape = "cbind",
  coef_map = coef_name,
  gof_map = gof_list,
  gof_omit = "Adj|Within|RMSE|Std.",
  exponentiate =TRUE,
  # output = "latex"
)

# Draft -------------------------------------------------------------------

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
base_rhs <- "tariff_exposure"  

# Combine everything
rhs <- paste(c(base_rhs, system_vars, dem_vars), collapse = " + ")
full_formula = as.formula(paste0("log(price_w) ~ ", rhs ))

# Model
hetero_pt =  list(
  "Overall" = list(
  "Full Sample" = feols(full_formula, fixef = c("year_quarter", "installer_name", "origin"), 
                        cluster = ~ zip_code, data = tts),
  "Excluding China"  = feols(full_formula, fixef = c("year_quarter","installer_name", "origin"), 
                             cluster = ~ zip_code, data = tts[origin != "china"])
  ),
  
  "Anti-Dumping : 2010 - 2013" = list(
  "Full Sample" = feols(full_formula, fixef = c("year_quarter", "installer_name", "origin"), 
          cluster = ~ zip_code, data = tts[year %in% 2010:2013]),
  "Excluding China" = feols(full_formula, fixef = c("year_quarter","installer_name", "origin"), 
          cluster = ~ zip_code, data = tts[year %in% 2010:2013 & origin != "china"])
  ),
  
  "Anti-Dumping : 2014 - 2016" = list(
    "Full Sample" = feols(full_formula, fixef =   c("year_quarter", "installer_name", "origin"), 
          cluster = ~ zip_code,data = tts[year %in% 2013:2016]),
    "Excluding China" = feols(full_formula, fixef =   c("year_quarter","installer_name", "origin"), 
          cluster = ~ zip_code,data = tts[year %in% 2013:2016 & origin != "china"])
  ),
  
  "Trade War 2018" = list(
    "Full Sample" = feols(full_formula, fixef = c("year_quarter", "installer_name", "origin"), 
          cluster = ~ zip_code, data = tts[year %in% 2017:2018]),
    "Excluding China" = feols(full_formula, fixef = c("year_quarter","installer_name", "origin"), 
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
  output = "latex"
)
writeLines(as.character(hetero_pt_table), "output/regression/pass_through/hetero_pass_through.tex")

# Mapping Coeff
coef_estimates = tariff_pt$Overall[[1]]$coefficients
beta_log_tariff <- as.numeric(coef_estimates["ln_tariff"])
beta_interaction <- coef_estimates["ln_tariff:efficiency_module"]
vcov_matrix <- vcov(tariff_pt$Overall[[1]])

eff_seq <- seq(0.09, 23, length.out = 100)

marginal_effects <- data.table(
  efficiency = eff_seq,
  elasticity = beta_log_tariff + beta_interaction * eff_seq
)

marginal_effects[, se := sqrt(
  vcov_matrix["ln_tariff", "ln_tariff"] +
    efficiency^2 * vcov_matrix["ln_tariff:efficiency_module", "ln_tariff:efficiency_module"] +
    2 * efficiency * vcov_matrix["ln_tariff", "ln_tariff:efficiency_module"]
)]

marginal_effects[, `:=`(
  upper = elasticity + 1.96 * se,
  lower = elasticity - 1.96 * se
)]

ggplot(marginal_effects, aes(x = efficiency, y = elasticity)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Marginal Effect of Tariff on Price across Efficiency Levels",
    x = "Efficiency",
    y = "Elasticity (Percent Change in Price for 1% increase in Tariff)",
    caption = "Shaded area: 95% confidence interval"
  )


# Chaisemartin ------------------------------------------------------------
tts[, treated := ifelse(tariff > 0, 1,0)]
test = did_multiplegt_dyn(df = tts[year %in% 2010:2013], outcome = "price_w", group = "county", time = 'year_quarter'
                   , treatment = "tariff", continuous = 1, cluster= "zip_code")

tts[, tariff := as.numeric(as.character(tariff))]  # ensure numeric
tts[, year_quarter_num := as.integer(as.factor(year_quarter))]  # fix time
tts[, county := as.factor(county)]
tts[, zip_code := as.factor(zip_code)]

# Step 1: Aggregate to county × year_quarter_num panel
tts_agg <- tts[
  year %in% 2010:2013,
  .(
    price_w = mean(price_w, na.rm = TRUE),
    tariff = mean(tariff, na.rm = TRUE)
  ),
  by = .(county, year_quarter_num, origin)
]

# Step 2: Convert identifiers to proper types
tts_agg[, ln_tariff := log(tariff)]
tts_agg[, tariff_alt := tariff - 1]
tts_agg[, county := as.factor(county)]
tts_agg[, year_quarter_num := as.integer(year_quarter_num)]

# Step 3: Run the dynamic DiD with continuous treatment

model <- did_multiplegt_dyn(
  df = tts_agg,
  outcome = "price_w",
  group = "county",
  time = "year_quarter_num",
  treatment = "tariff_alt",
  continuous = 1,
  effects = 4,
  placebo = 2,
  cluster = "county"
)
