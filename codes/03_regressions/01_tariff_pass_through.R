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
library(quantreg)

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
      feols(full_formula, fixef = c("year_quarter","county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2010:2018]),
      feols(full_formula, fixef = c("county", "installer_name", "quarter_origin"), cluster = ~ zip_code, data = tts[year %in% 2010:2018])
      ),
    
    "Anti-Dumping : 2010 - 2013" = list(
      feols(full_formula, fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2010:2013]),
      feols(full_formula, fixef = c("county", "installer_name", "quarter_origin"), cluster = ~ zip_code, data = tts[year %in% 2010:2013])
      ),
    
    "Anti-Dumping : 2014 - 2016" = list(
     feols(full_formula, fixef =   c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code,data = tts[year %in% 2014:2016]),
     feols(full_formula, fixef =   c("county", "installer_name", "quarter_origin"), cluster = ~ zip_code,data = tts[year %in% 2014:2016])
      ),
    
    "Trade War 2018" = list(
     feols(full_formula, fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2017:2018]),
     feols(full_formula, fixef = c("county", "installer_name", "quarter_origin"), cluster = ~ zip_code, data = tts[year %in% 2017:2018])
      )
  )
  
  coef_name = c(
    "ln_tariff" = "ln Tariff",
    "ln_tariff:efficiency_module" = "ln Tariff x Efficiency",
    "ln_tariff:premium_panel_overall" = "ln Tariff x Premium Panel",
    "ln_tariff:premium_installation" = "ln Tariff x Premium Installation",
    "treated" = "Treated",
    "treated:ln_tariff"= "Treated x ln Tariff",
    "premium_panel_overall" = "Premium Panel",
    "premium_installation" = "Premium Installation",
    "efficiency_module" = "Efficiency"
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
      feols(full_formula, fixef = c("year_quarter","county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2010:2018]),
      feols(full_formula, fixef = c("county","installer_name", "quarter_origin"), cluster = ~ zip_code, data = tts[year %in% 2010:2018])
    ),
    
    "Anti-Dumping : 2010 - 2013" = list(
    # "2012" = feols(full_formula, fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2012]),
    feols(full_formula, fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2010:2013]),
    feols(full_formula, fixef = c("county","installer_name", "quarter_origin"), cluster = ~ zip_code, data = tts[year %in% 2010:2013])
    ),
    
    "Anti-Dumping : 2014 - 2016" = list(
    # "2014" = feols(full_formula, fixef =   c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code,data = tts[year %in% 2014]),
    feols(full_formula, fixef =   c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code,data = tts[year %in% 2014:2016]),
    feols(full_formula, fixef =   c("county","installer_name", "quarter_origin"), cluster = ~ zip_code,data = tts[year %in% 2014:2016])
    ),
    
    "Trade War 2018" = list(
     feols(full_formula, fixef = c("year_quarter", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2017:2018]),
     feols(full_formula, fixef = c("county","installer_name", "year_quarter"), cluster = ~ zip_code, data = tts[year %in% 2017:2018])
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

# Initialize a list to store residuals from the first model in each group
resid_rob_overall = feols(full_formula, fixef = c("year_quarter","county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2010:2018], data.save = TRUE)
data = setDT(resid_rob$data)
data = data[!is.na(mean_week_wage)]
data = data[!is.na(mean_month_emp)]
data = data[!is.na(median_household_income)]
data = data[!is.na(median_home_value)]
data = data[!is.na(pct_bachelor_estimate)]
data[, res := resid(resid_rob)]
res_test_overall = feols(res ~ tariff, data = data)


# Define time periods
periods <- list(
  "Overall" = 2010:2020,
  "AD 2010–2013" = 2010:2013,
  "AD 2014–2016" = 2014:2016,
  "Trade War 2017–2018" = 2017:2018
)

# Store regression outputs
res_tests <- list()

# Loop over each subperiod
for (label in names(periods)) {
  
  # Subset to relevant years
  subdata <- tts[year %in% periods[[label]]]
  
  # Run base regression and extract used dataset
  model <- feols(
    full_formula,
    fixef = c("year_quarter", "county", "installer_name", "origin"),
    cluster = ~ zip_code,
    data = subdata,
    data.save = TRUE
  )
  
  # Extract cleaned dataset and compute residuals
  dt <- setDT(model$data)
  dt <- dt[complete.cases(mean_week_wage, mean_month_emp, median_household_income, median_home_value, pct_bachelor_estimate)]
  dt[, res := resid(model)]
  
  # Regress residuals on tariff
  res_tests[[label]] <- feols(res ~ ln_tariff, cluster = "zip_code", data = dt)
}

# Create coefficient labels (optional)
coef_map <- c("ln_tariff" = "ln Tariff")

# Output summary table
robs = modelsummary(
  res_tests,
  coef_map = coef_map,
  stars = TRUE,
  gof_omit = "Adj|AIC|BIC|RMSE",
  # title = "Test of Correlation Between Tariff and Regression Residuals"
  output = "latex"
)
writeLines(as.character(robs), "output/regression/robustness/weak_test.tex")

# Quantile Regression Efficiency ------------------------------------------
decile_breaks_2011 <- quantile(
  tts[year == 2011, efficiency_module],
  probs = seq(0, 1, 0.20),
  na.rm = TRUE
)

tts[year %in% 2010:2013, qual_qt := cut(efficiency_module,
                                        breaks = decile_breaks_2011,
                                        include.lowest = TRUE,
                                        labels = paste0("Q", 1:5))]

decile_breaks_2013 <- quantile(
  tts[year == 2013, efficiency_module],
  probs = seq(0, 1, 0.20),
  na.rm = TRUE
)

tts[year %in% 2014:2016, qual_qt := cut(efficiency_module,
                                        breaks = decile_breaks_2013,
                                        include.lowest = TRUE,
                                        labels = paste0("Q", 1:5))]

decile_breaks_2017 <- quantile(
  tts[year == 2017, efficiency_module],
  probs = seq(0, 1, 0.20),
  na.rm = TRUE
)

tts[year %in% 2017:2018, qual_qt := cut(efficiency_module,
                                        breaks = decile_breaks_2017,
                                        include.lowest = TRUE,
                                        labels = paste0("Q", 1:5))]

tts[year %in% 2010:2013, qual_qt := cut(efficiency_module,
                     breaks = quantile(efficiency_module,
                                       probs = seq(0,1,0.20),
                                       na.rm=TRUE),
                     include.lowest=TRUE,
                     labels=paste0("Q",1:5)
)]
tts[year %in% 2014:2016, qual_qt := cut(efficiency_module,
                     breaks = quantile(efficiency_module,
                                       probs = seq(0,1,0.20),
                                       na.rm=TRUE),
                     include.lowest=TRUE,
                     labels=paste0("Q",1:5)
)]
tts[year %in% 2017:2018, qual_qt := cut(efficiency_module,
                     breaks = quantile(efficiency_module,
                                       probs = seq(0,1,0.20),
                                       na.rm=TRUE),
                     include.lowest=TRUE,
                     labels=paste0("Q",1:5)
)]

formula_str <- "log(price_w) ~ ln_tariff * qual_qt + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income"
formula_eff_het <- as.formula(formula_str)
tts[, qual_qt := relevel(qual_qt, ref = "Q1")]

heterogeneity_efficiency = list(
  "Overall" = list(
    feols(formula_eff_het, fixef = c("year_quarter", "county", "origin", "installer_name"), 
          cluster = ~zip_code, data = tts[year %in% 2010:2018]),
    feols(formula_eff_het, fixef = c("county", "quarter_origin", "installer_name"), 
          cluster = ~zip_code, data = tts[year %in% 2010:2018])
    ),
  "Anti-Dumping : 2010-2013" = list(
    feols(formula_eff_het, fixef = c("year_quarter", "county", "origin", "installer_name"), 
          cluster = ~zip_code, data = tts[year %in% 2010:2013]),
    feols(formula_eff_het, fixef = c("county", "quarter_origin", "installer_name"), 
          cluster = ~zip_code, data = tts[year %in% 2010:2013])
    ),
  "Anti-Dumping : 2014-2016" = list(
    feols(formula_eff_het, fixef = c("year_quarter", "county", "origin", "installer_name"), 
          cluster = ~zip_code, data = tts[year %in% 2014:2016]),
    feols(formula_eff_het, fixef = c("county", "quarter_origin", "installer_name"), 
          cluster = ~zip_code, data = tts[year %in% 2014:2016])
    ),
  "Trade War 2018" = list(
    feols(formula_eff_het, fixef = c("year_quarter","county","origin","installer_name"), 
          cluster = ~zip_code, data = tts[year %in% 2017:2018]),
    feols(formula_eff_het, fixef = c("county","quarter_origin","installer_name"),
          cluster = ~zip_code, data = tts[year %in% 2017:2018])
    )
)

coef_name = c(
  "ln_tariff" = "ln Tariff", 
  "ln_tariff:qual_qtQ2" = "ln Tariff x Decile D2",
  "ln_tariff:qual_qtQ3" = "ln Tariff x Decile D3",
  "ln_tariff:qual_qtQ4" = "ln Tariff x Decile D4",
  "ln_tariff:qual_qtQ5" = "ln Tariff x Decile D5",
  "ln_tariff:qual_qtQ6" = "ln Tariff x Decile D6",
  "ln_tariff:qual_qtQ7" = "ln Tariff x Decile D7",
  "ln_tariff:qual_qtQ8" = "ln Tariff x Decile D8",
  "ln_tariff:qual_qtQ9" = "ln Tariff x Decile D9",
  "ln_tariff:qual_qtQ10" = "ln Tariff x Decile D10"
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

eff_10 = modelsummary(
  models = heterogeneity_efficiency,
  gof_omit = "Within|AIC|BIC|RMSE|Std.",
  shape = "cbind",
  coef_map = coef_name,
  star = TRUE,
  gof_map = gof_list,
  output = "latex"
)
writeLines(as.character(eff_10), "output/regression/pass_through/hetero_pass_through_efficiency.tex")

coef_list <- list()

for (period in names(heterogeneity_efficiency)) {
  for (i in seq_along(heterogeneity_efficiency[[period]])) {
    model <- heterogeneity_efficiency[[period]][[i]]
    model_label <- paste0(period, " [", i, "]")  # e.g., "2010–2013 [1]"
    
    model_coefs <- as.data.table(coeftable(model), keep.rownames = "term")
    model_coefs[, `:=`(
      model = model_label,
      period = period,
      spec = paste0("Spec ", i)
    )]
    
    coef_list[[length(coef_list) + 1]] <- model_coefs
  }
}

coef_dt <- rbindlist(coef_list)

coef_dt[, label := fcase(
  term == "ln_tariff", "ln Tariff",
  term == "ln_tariff:qual_qtQ2", "ln Tariff x Q2",
  term == "ln_tariff:qual_qtQ3", "ln Tariff x Q3",
  term == "ln_tariff:qual_qtQ4", "ln Tariff x Q4",
  term == "ln_tariff:qual_qtQ5", "ln Tariff x Q5",
  term == "ln_tariff:qual_qtQ6", "ln Tariff x Q6",
  term == "ln_tariff:qual_qtQ7", "ln Tariff x Q7",
  term == "ln_tariff:qual_qtQ8", "ln Tariff x Q8",
  term == "ln_tariff:qual_qtQ9", "ln Tariff x Q9",
  term == "ln_tariff:qual_qtQ10", "ln Tariff x Q10",
  default = NA
)]

plot_dt <- coef_dt[!is.na(label)]
plot_dt[, `:=`(
  ymin = Estimate - 1.96 * `Std. Error`,
  ymax = Estimate + 1.96 * `Std. Error`
)]
plot_dt[, factor := fcase(
  label == "ln Tariff", 1,
  label == "ln Tariff x Q2", 2,
  label == "ln Tariff x Q3", 3,
  label == "ln Tariff x Q4", 4,
  label == "ln Tariff x Q5", 5,
  label == "ln Tariff x Q6", 6,
  label == "ln Tariff x Q7", 7,
  label == "ln Tariff x Q8", 8,
  label == "ln Tariff x Q9", 9,
  label ==  "ln Tariff x Q10", 10,
  default = NA
)]
plot_dt = plot_dt[period != "Overall",]

ordered_labels <- plot_dt[order(factor), unique(label)]
plot_dt[, label := factor(label, levels = ordered_labels)]

ggplot(plot_dt, aes(x = label, y = Estimate, color = model)) +
  geom_point(position = position_dodge(width = 0.6), size = 2.5) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax),
                position = position_dodge(width = 0.6), width = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    x = NULL, y = "Estimate (95% CI)",
    # title = "Tariff Effects by Quality Quintile",
    color = "Model"
  ) +
  theme_classic(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom")
ggsave("output/regression/pass_through/efficiency_quintile_alt.pdf", width = 10, height = 8)

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
      feols(full_formula, fixef = c("year","county", "installer_name", "origin"), cluster = ~ zip_code, data = tts),
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
      feols(full_formula, fixef = c("year", "county", "installer_name", "origin"), cluster = ~ zip_code, data = tts[year %in% 2017:2018]),
      feols(full_formula, fixef = c("county","year_installer", "year_origin"), cluster = ~ zip_code, data = tts[year %in% 2017:2018])
    )
  )
  
  pass_intensity_through = modelsummary(
    models = tariff_intensity_pt,
    stars = TRUE,
    shape = "cbind",
    coef_map = coef_name,
    gof_omit = "Adj|AIC|BIC|Within|Pseudo|RMSE|Std.",
    gof_map = gof_list,
    output = "latex"
  )
  
  if (p == "log(net_price) ~"){
    writeLines(as.character(pass_intensity_through), "output/regression/pass_through/pass_through_intensity_netprice.tex")}
  else {
    writeLines(as.character(pass_intensity_through), "output/regression/pass_through/pass_through_intensity_grossprice.tex")
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
# Sys.setenv(RGL_USE_NULL = TRUE)
# library(DIDmultiplegtDYN)
# 
# treated_firms = tts[origin == "china"]$module_manfucturer
# mkt_share = tts[,.(sum_sales_brand = .N), by = .(year_quarter, county, origin)]
# mkt_share[, sum_sales := sum(sum_sales_brand), by = .(year_quarter, county)]  # fix here
# mkt_share = unique(mkt_share[, .(exposure = sum_sales_brand[origin == "china"] / sum_sales), by = .(year_quarter, county)])
# # mkt_share[, treated := as.integer(module_manufacturer %in% treated_firms)]
# # county_exposure <- mkt_share[, .(exposure = sum(exposure * treated)), by = .(year_quarter, county)]
# tts <- merge(tts, mkt_share, by = c("year_quarter", "county"), all.x = TRUE)
# tts_ad1 = tts[year %in% 2010:2013,]
# all_combos <- CJ(county = unique(tts_ad1$county),
#                  year_quarter = unique(tts_ad1$year_quarter),
#                  sorted = TRUE)
# tts_balanced <- merge(all_combos, tts_ad1, by = c("county", "year_quarter"), all.x = TRUE)
# tts_balanced[, exposure := ifelse(is.na(exposure), 0, exposure)]
# 
# 
# # Convert to ordered factor or numeric time index
# tts_ad1[, year_quarter := as.integer(as.factor(year_quarter))]
# summary(tts_balanced[, .(price_w, county, year_quarter, exposure)])
# 
# test = did_multiplegt_dyn(df = tts_ad1, outcome = "price_w", group = "county", time = 'year_quarter'
#                    , treatment = "exposure", continuous = 1, cluster= "county", bootstrap = 1)
# 
# tts[, tariff := as.numeric(as.character(tariff))]  # ensure numeric
# tts[, year_quarter_num := as.integer(as.factor(year_quarter))]  # fix time
# tts[, county := as.factor(county)]
# tts[, zip_code := as.factor(zip_code)]
# 
# # Step 1: Aggregate to county × year_quarter_num panel
# tts_agg <- tts[
#   year %in% 2010:2013,
#   .(
#     price_w = mean(price_w, na.rm = TRUE),
#     tariff = mean(tariff, na.rm = TRUE)
#   ),
#   by = .(county, year_quarter_num, origin)
# ]
# 
# # Step 2: Convert identifiers to proper types
# tts_agg[, ln_tariff := log(tariff)]
# tts_agg[, tariff_alt := tariff - 1]
# tts_agg[, county := as.factor(county)]
# tts_agg[, year_quarter_num := as.integer(year_quarter_num)]
# 
# # Step 3: Run the dynamic DiD with continuous treatment
# 
# model <- did_multiplegt_dyn(
#   df = tts_agg,
#   outcome = "price_w",
#   group = "county",
#   time = "year_quarter_num",
#   treatment = "tariff_alt",
#   continuous = 1,
#   effects = 4,
#   placebo = 2,
#   cluster = "county"
# )


library(zoo)
# demand = fread(data_final("demand_final_alt.csv"))
tts[, tq := as.yearqtr(gsub("Q", " ", year_quarter), format = "%Y %q")]
tts[, time_to_event_ad1 := as.integer(4 * (tq - as.yearqtr("2012 Q2")))]
tts[, time_to_event_ad2 := as.integer(4 * (tq - as.yearqtr("2014 Q2")))]
tts[, time_to_event_st := as.integer(4 * (tq - as.yearqtr("2018 Q1")))]

sub_ad1 <- tts[origin == "china" & year %in% 2010:2013]
sub_ad2 <- tts[origin == "china" & year %in% 2013:2016]
sub_st <- tts[origin != "china" & year %in% 2017:2018]

# Run the event study with time_to_event = 0 as the event
es_model_ad1 <- feols(
  log(price_w) ~ i(time_to_event_ad1, ref = -4) | zip_code + year, data = sub_ad1
)
png("output/regression/robustness/event_study_plot_ad1.png", width = 800, height = 600)
iplot(es_model_ad1,
      xlab = "Quarter Relative to 2012Q2",
      main = "Event Study: Price Impact of Antidumping Tariff on Chinese",
      ref.line = 0)
dev.off()

es_model_ad2 <- feols(
  log(price_w) ~ i(time_to_event_ad2, ref = -3) |zip_code + year, data = sub_ad2
)

png("output/regression/robustness/event_study_plot_ad2.png", width = 800, height = 600)
iplot(es_model_ad2,
      xlab = "Quarter Relative to 2014Q2",
      main = "Event Study: Price Impact of Antidumping Tariff on Chinese",
      ref.line = 0)
dev.off()


es_model_st <- feols(
  log(price_w) ~ i(time_to_event_st, ref = -2) | zip_code + year, data = sub_st
)
png("output/regression/robustness/event_study_plot_st.png", width = 800, height = 600)
iplot(es_model_st,
      xlab = "Quarter Relative to 2018Q1",
      main = "Event Study: Price Impact of Tariff on China",
      ref.line = 0)
dev.off()


