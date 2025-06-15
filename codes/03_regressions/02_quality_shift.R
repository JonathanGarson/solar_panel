# This code evaluates the change in variety and their quality entering the U.S. territory after tariff

library(arrow)
library(data.table)
library(fixest)
library(modelsummary)
library(tibble)
library(glue)
library(fplot)
library(quantreg)

# Data --------------------------------------------------------------------

tts = setDT(read_parquet(data_final("tts_final.parquet")))

# Merging Data ------------------------------------------------------------

quality_brands = tts[, .(avg_efficiency = mean(efficiency_module, na.rm = TRUE)), by = .(year_quarter, module_manufacturer)]
tts[, tract := NULL]
tts[, demand_zip_code := NULL]
tts = merge(tts, quality_brands, by = c("year_quarter", "module_manufacturer"))
tts[year_quarter >= "2010Q1" & year_quarter <= "2014Q1", post := ifelse(year_quarter >= "2012Q2" & year_quarter <= "2014Q1", 1, 0)]
tts[year_quarter >= "2014Q1" & year_quarter <= "2016Q4", post := ifelse(year_quarter >= "2014Q2" & year_quarter <= "2017Q4", 1, 0)]
tts[year_quarter >= "2017Q1" & year_quarter <= "2020Q4", post := ifelse(year_quarter %in% c("2018Q1","2018Q2"), 1, 0)]
tts[, quarter_origin := paste0(year_quarter, origin)]
tts[, quarter_installer := paste0(year_quarter, installer_name)]


# Quality Effect of Tariff ------------------------------------------------
quality_change = list(
  "Overall" = list(
    feols(log(efficiency_module) ~ log(tariff) + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price 
        + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income
        | year_quarter + county + installer_name + origin,
        cluster = ~zip_code, data = tts[year %in% 2010:2018]),
    feols(log(efficiency_module) ~ log(tariff) + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price 
          + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income
          | county + installer_name + quarter_origin,
          cluster = ~zip_code, data = tts[year %in% 2010:2018])
    ),
  "Anti-Dumping : 2010-2013" = list(
    feols(log(efficiency_module) ~ log(tariff) + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price 
        + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income
        | year_quarter + county + installer_name + origin,
        cluster = ~zip_code, data = tts[year %in% 2010:2013]),
    feols(log(efficiency_module)~ log(tariff) + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price 
        + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income
        | county + installer_name + quarter_origin,
        cluster = ~zip_code, data = tts[year %in% 2010:2013])
    ),
  "Anti-Dumping : 2014-2016" = list(
    feols(log(efficiency_module) ~ log(tariff) + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price 
        + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income
        | year_quarter + county + installer_name + origin,
        cluster = ~zip_code, data = tts[year %in% 2014:2016]),
    feols(log(efficiency_module) ~ log(tariff) + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price 
        + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income
        | county + installer_name + quarter_origin,
        cluster = ~zip_code, data = tts[year %in% 2014:2016])
    ),
  "Trade War 2018" = list(
    feols(log(efficiency_module) ~ log(tariff) + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price 
        + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income
        | year_quarter + county + installer_name + origin,
        cluster = ~zip_code, data = tts[year %in% 2017:2018]),
    feols(log(efficiency_module) ~ log(tariff) + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price 
        + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income
        | county + installer_name + quarter_origin,
        cluster = ~zip_code, data = tts[year %in% 2017:2018])
    )
)

coef_name = c(
  "log(tariff)" = "ln Tariff",
  # "treated" = "Treated",
  "log(tariff):treated" = "ln Tariff x Treated"
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
  "FE: quarter_origin",  "FE: Year Quarter × Origin",  "%.0f",
  "FE: quarter_installer",     "FE: Quarter × Installer",  "%.0f"
)

my_overall_1 = fitstat(quality_change$Overall[[1]], type = "my")$my
my_overall_ad1 = fitstat(quality_change$`Anti-Dumping : 2010-2013`[[1]], type = "my")$my
my_overall_ad2 = fitstat(quality_change$`Anti-Dumping : 2014-2016`[[1]], type = "my")$my
my_overall_st = fitstat(quality_change$`Trade War 2018`[[1]], type = "my")$my

df_eff = data.frame(
  term = "Mean Dep. Var",
  stat_1 = my_overall_1, 
  stat_2 = my_overall_1, 
  stat_3 = my_overall_ad1 ,
  stat_4 = my_overall_ad1,
  stat_5 = my_overall_ad2 ,
  stat_6 = my_overall_ad2 ,
  stat_7 = my_overall_st,
  stat_8 = my_overall_st
)

table = modelsummary(
    models = quality_change,
    stars = TRUE,
    shape = "cbind",
    coef_map = coef_name,
    gof_omit = "Adj|AIC|BIC|Within|Pseudo|RMSE|Std.",
    gof_map = gof_list,
    add_row = df_eff,
    output = "latex"
  )
writeLines(as.character(table), "output/regression/quality_shift/table_quality_change.tex")

# Quantile Regression -----------------------------------------------------
tts[, tariff_scale100 := log((tariff))]
# qnt = rq(efficiency_module ~ tariff_scale100 + origin + year_quarter, tau=c(0.1, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.9), method = "fn", data=tts)
qnt_ad1 = rq(efficiency_module ~ tariff_scale100 + origin + year_quarter, tau=c(0.1, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.9), method = "fn", data=tts[year %in% 2010:2013])
qnt_ad2 = rq(efficiency_module ~ tariff_scale100 + origin + year_quarter, tau=c(0.1, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.9), method = "fn", data=tts[year %in% 2014:2016])
qnt_st = rq(efficiency_module ~ tariff_scale100 + origin + year_quarter, tau=c(0.1, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.9), method = "fn", data=tts[year %in% 2017:2018])

plot_ad1 = summary(qnt_ad1, se= "ker")
plot_ad2 = summary(qnt_ad2, se= "ker")
plot_st = summary(qnt_st, se= "ker")

# Collect estimate for AD1
collec_estimate= function(summary_data){
  taus <- c(0.1, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.9)
  
  # Extract the tariff coefficient and its SE across quantiles
  coef_tau <- sapply(summary_data, function(x) x$coefficients["tariff_scale100", "Value"])
  se_tau   <- sapply(summary_data, function(x) x$coefficients["tariff_scale100", "Std. Error"])
  
  # Combine into a data frame
  df <- data.frame(
    tau = taus,
    coef = coef_tau,
    se = se_tau,
    lower = coef_tau - 1.96 * se_tau,
    upper = coef_tau + 1.96 * se_tau
  )
  return(df)
}

ad1 = collec_estimate(plot_ad1)
ad1 = ad1 %>% mutate(event = "Anti-Dumping 2010-2013")
ad2 = collec_estimate(plot_ad2)
ad2 = ad2 %>% mutate(event = "Anti-Dumping 2014-2016")
st = collec_estimate(plot_st)
st = st %>% mutate(event = "Trade War 2018")

data_qnt = rbind(ad1, ad2, st)

ggplot(data_qnt, aes(x = tau, y = coef, color = event, fill = event)) +
  geom_line(size = 1) +
  geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Quantile",
    y = "Effect of Tariff on Panel Efficiency",
    # title = "Tariff Pass-Through to Efficiency Across Quantiles",
    color = "Event",
    fill = "Event"
  ) +
  theme_classic() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.65, 0.80),  # new argument!
    legend.justification = c("left", "bottom"),
    legend.background = element_rect(fill = "white", color = "grey80"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )
ggsave("output/regression/quality_shift/quantile_regression_efficiency_tariff.pdf", width = 7, height = 7)

# Change in Dispersion ----------------------------------------------------
# CV by year
cv_year <- tts[, .(
  mean_eff = mean(efficiency_module, na.rm = TRUE),
  sd_eff = sd(efficiency_module, na.rm = TRUE)
), by = year_quarter
][, cv := sd_eff / mean_eff]

# CV by year and origin
cv_year_origin <- tts[, .(
  mean_eff = mean(efficiency_module, na.rm = TRUE),
  sd_eff = sd(efficiency_module, na.rm = TRUE)
), by = .(year_quarter, origin)
][, cv_o := sd_eff / mean_eff]

tts = merge(tts, cv_year, by = c("year_quarter"))
tts = merge(tts, cv_year_origin, by = c("origin","year_quarter"))

tts[year %in% 2010:2013, post_ad1 := ifelse(year_quarter >= "2012Q2", 1, 0)]
tts[year %in% 2014:2016, post_ad2 := ifelse(year_quarter >= "2014Q2", 1, 0)]
tts[year %in% 2017:2018, post_st := ifelse(year_quarter >= "2018Q1", 1, 0)]

feols(cv ~ post_ad1| year, data = tts[year %in% 2010:2013])
feols(cv ~ post_ad2| year, data = tts[year %in% 2014:2016])
feols(cv ~ post_st| year, data = tts[year %in% 2017:2018])

ggplot(cv_year, aes(x = year, y = cv))+
  geom_line() +
  theme_minimal()

ggplot(cv_year_origin, aes(x = year, y = cv, group = origin, color = origin))+
  geom_line() +
  theme_minimal()



# RIOS-AVILA - Efficiency Decile -------------------------------------------------------

tts_ad1 = tts[year %in% 2010:2013]
tts_ad2 = tts[year %in% 2014:2016]
tts_st = tts[year %in% 2017:2018]

# 1. « First, for all dependent and independent variables in the model (w = y, x), 
# we partial out the group fixed ejects and obtain the centered-residualized variables: » ([Rios Avila et al., 2024, p. 12]
# In our case y = log_efficiency and x = log_tariff
tts[, log_efficiency := log(efficiency_module)]
tts[, log_tariff := log(tariff)]
tts[, log_efficiency_demean := demean(log_efficiency ~ installer_name + origin + county + year_quarter, data = tts)]
tts[, log_tariff_demean := demean(log_tariff ~ installer_name + origin + county + year_quarter, data = tts)]

# 2. « Afterward, we estimate the location model using the centered-residualized variables: » ([Rios Avila et al., 2024, p. 12]
loc_model = lm(log_efficiency_demean ~ log_tariff_demean, data = tts)
tts[, resid_loc_model := resid(loc_model)]
beta_hat_loc_model = as.numeric(coef(loc_model)["log_tariff_demean"])

# 3.« Because |nuˆi| is the dependent variable for the scale model, 
# we apply the partialing out and recentering to this expression (|nuˆi|rc), 
# and use that to estimate the following model: » ([Rios Avila et al., 2024, p. 12]
tts[, abs_resid_demean := demean(abs(resid_loc_model) ~ installer_name + origin + county + year_quarter, data = tts)]
# We can estimate the scale model now
scale_model = lm(abs_resid_demean ~ log_tariff_demean , data = tts)
gamma_hat_scale_model = as.numeric(coef(scale_model)[["log_tariff_demean"]])

# 4. We recompose the standardized residuals and solve for each quantile
X = model.matrix(~ log_tariff_demean, data = tts)
resid_std <- (tts$log_efficiency_demean - X * beta_hat_loc_model) / as.numeric(X * gamma_hat_scale_model)

# 5. We recover each quantile
tau <- 0.25
moment_function <- function(q) {
  mean(resid_std <= q , na.rm = TRUE) - tau
}
q_tau <- uniroot(moment_function, lower = -10, upper = 10)$root

# Recover coef
beta_tau <- beta_hat_loc_model + q_tau * gamma_hat_scale_model

# We Wrap this in a function
quantile_fe = function(dt, x, y, fe, taus = c(0.25, 0.5, 0.75)){
  library(fixest)
  library(data.table)
  # 1. We demean our dependent and explanatory variables
  # y_demean = paste0(y, "_demean")
  # x_demean = paste0(x, "_demean")
  fe_formula_y = as.formula(paste0(y,"~", paste(fe, collapse = "+")))
  fe_formula_x = as.formula(paste0(x,"~", paste(fe, collapse = "+")))
  
  dt[, y_demean := demean(fe_formula_y, data = dt)]
  dt[, x_demean := demean(fe_formula_x, data = dt)]
  
  # 2. Estimate the location model
  loc_model = lm(y_demean ~ x_demean -1, data = dt)
  dt[, resid_loc_model := resid(loc_model)]
  beta_hat_loc_model = as.numeric(loc_model$coefficients["x_demean"])
  
  # 3. Estimate the scale model 
  scale_fe_formula = as.formula(paste0("abs(resid_loc_model) ~ ", paste(fe, collapse = "+")))
  dt[, abs_resid_demean := demean(scale_fe_formula, data= dt)]
  scale_model = lm(abs_resid_demean ~ x_demean , data = dt)
  gamma_hat_scale_model = as.numeric(coef(scale_model)[["x_demean"]])
  
  # 4. We recompose the standardized residuals
  X = model.matrix(~ x_demean - 1, data = dt)
  resid_std <- (dt$y_demean - X * beta_hat_loc_model) / as.numeric(X * gamma_hat_scale_model)
  resid_std_clean <- resid_std[is.finite(resid_std)]
  
  # Recover quantile
  results <- list()
  for (tau in taus) {
    moment_fun <- function(q) mean(resid_std_clean <= q, na.rm = TRUE) - tau
    root <- uniroot(moment_fun, lower = -20000, upper = 20000)$root
    print(root)
    beta_tau <- beta_hat_loc_model + root * gamma_hat_scale_model
    results[[as.character(tau)]] <- beta_tau
    
    return(results)
    # Return results in tidy format
    do.call(rbind, lapply(names(results), function(t) {
      data.frame(tau = as.numeric(t), term = names(results[[t]]), estimate = results[[t]])
    }))
  }
}

taus_list = seq(0,1, 0.25)
quantile_ad1 = quantile_fe(tts_ad1, x = "log_tariff", y = "log_efficiency",
                           fe = c("installer_name", "county", "origin", "year_quarter"), taus = c(0.00, 0.25, 0.50, 0.75, 1.00))

# Quantile Alt ------------------------------------------------------------

tts[year %in% 2010:2013, tariff_exposure := ifelse(tariff > 1, "high", NA)]
tts[year %in% 2014:2016, tariff_exposure := fcase(tariff < 1.40, "low",
                                                  tariff >1.40 & tariff < 1.70, "mid",
                                                  tariff > 1.70, "high", 
                                                  default = NA)]
tts[year %in% 2017:2018, tariff_exposure := fcase(tariff < 1.40, "low",
                                                  tariff >1.40 & tariff < 1.70, "mid",
                                                  tariff > 1.70, "high", 
                                                  default = NA)]

tts[, tariff_exposure := factor(tariff_exposure,levels = c("low","mid","high"))]

formula_str <- "log(efficiency_module) ~ tariff_exposure  + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income"
formula_eff_tariff <- as.formula(formula_str)

efficiency_jump = list(
  "Overall" = list(
    feols(formula_eff_tariff, fixef = c("year_quarter", "county", "origin", "installer_name"), 
          cluster = ~zip_code, data = tts[year %in% 2010:2018]),
    feols(formula_eff_tariff, fixef = c("county", "quarter_origin", "installer_name"), 
          cluster = ~zip_code, data = tts[year %in% 2010:2018])
  ),
  "Anti-Dumping : 2010-2013" = list(
    feols(formula_eff_tariff, fixef = c("year_quarter", "county", "origin", "installer_name"), 
          cluster = ~zip_code, data = tts[year %in% 2010:2013]),
    feols(formula_eff_tariff, fixef = c("county", "quarter_origin", "installer_name"), 
          cluster = ~zip_code, data = tts[year %in% 2010:2013])
  ),
  "Anti-Dumping : 2014-2016" = list(
    feols(formula_eff_tariff, fixef = c("year_quarter", "county", "origin", "installer_name"), 
          cluster = ~zip_code, data = tts[year %in% 2014:2016]),
    feols(formula_eff_tariff, fixef = c("county", "quarter_origin", "installer_name"), 
          cluster = ~zip_code, data = tts[year %in% 2014:2016])
  ),
  "Trade War 2018" = list(
    feols(formula_eff_tariff, fixef = c("year_quarter","county","origin","installer_name"), 
          cluster = ~zip_code, data = tts[year %in% 2017:2018]),
    feols(formula_eff_tariff, fixef = c("county","quarter_origin","installer_name"),
          cluster = ~zip_code, data = tts[year %in% 2017:2018])
  )
)

coef_name = c(
  "tariff_exposuremid" = "Mid Exposure to Tariff",
  "tariff_exposurehigh" = "High Exposure to Tariff",
  "tariff_exposure" = "Tariff Exposure"
)

modelsummary(
  models= efficiency_jump, 
  shape = "cbind",
  gof_omit = "AIC|BIC|Std.|Within",
  stars = TRUE,
  coef_map = coef_name
)

qr   <- rq(log(efficiency_module) ~ tariff_exposure + origin + county + year,
           data = tts[year %in% 2014:2016],
           tau  = c(0.1,0.2, 0.3,0.4,0.5,0.6, 0.7,0.9))
summary(qr, se = "ker")

# DiD Approach ------------------------------------------------------------
# tts[, treated := fcase(origin == "china", 1,
#                        module_manufacturer == "rec solar", 0,
#                        module_manufacturer == "panasonic", 0)]
# tts[, treated := ifelse(origin == "china", 1, 0)]
# 
# test_ad1 = feols(log(price_w) ~ i(year_quarter, treated, ref= "2011Q4")|county + year + origin,data = tts[year %in% 2010:2013])
# iplot(test_ad1)
# 
# test_ad2 = feols(log(price_w) ~ i(year_quarter, treated, ref= "2017Q4")|county + year + origin ,data = tts[year %in% 2017:2019])
# iplot(test_ad2)

# Alternative Strategy ----------------------------------------------------
# disper_efficiency_pre_ad2 = tts[year == 2013, (var(efficiency_module)/mean(efficiency_module)), by = .(tariff)]
# disper_efficiency_post_ad2 = tts[year == 2015, (var(efficiency_module)/mean(efficiency_module)), by = .(tariff)]
# 
# disper_efficiency_pre = tts[year == 2017, (var(efficiency_module)/mean(efficiency_module)), by = .(tariff)]
# disper_efficiency_post = tts[year == 2019, (var(efficiency_module)/mean(efficiency_module)), by = .(tariff)]

# Better formulation
dispersion_dt <- tts[year %in% 2010:2020, 
                     .(dispersion = var(efficiency_module, na.rm = TRUE) / mean(efficiency_module, na.rm = TRUE)),
                     by = .(module_manufacturer, year_quarter)
]
tariff_brand = unique(tts[, .(module_manufacturer, year_quarter, tariff)])
origin_brand = unique(tts[, .(module_manufacturer, origin)])
dispersion_dt = merge(dispersion_dt, tariff_brand, by = c("module_manufacturer", "year_quarter"), all.x = TRUE)
dispersion_dt = merge(dispersion_dt, origin_brand, by = c("module_manufacturer"), all.x = TRUE)
dispersion_dt[, year := substr(x = year_quarter, 1 , 4)]

overdispersion = list(
  "Overall" = feols(dispersion ~ tariff | year_quarter + origin, vcov = "hetero", data = dispersion_dt),
  "Anti-Dumping Tariff : 2010-2013" = feols(dispersion ~ tariff | year_quarter + origin, vcov = "hetero", data = dispersion_dt[year %in% 2010:2013]),
  "Anti-Dumping Tariff : 2014-2016" = feols(dispersion ~ tariff | year_quarter + origin, vcov = "hetero", data = dispersion_dt[year %in% 2014:2016]),
  "Trade War 2018" = feols(dispersion ~ tariff |year_quarter + origin, vcov = "hetero", data = dispersion_dt[year %in% 2017:2018]))
etable(overdispersion, dict = c(dispersion = "Dispersion",tariff = "Tariff"), fitstat = "f")

# Extract F-statistics
f_stats <- c(
  fitstat(overdispersion$Overall, type = "f")$f$stat,
  fitstat(overdispersion$`Anti-Dumping Tariff : 2010-2013`, type = "f")$f$stat,
  fitstat(overdispersion$`Anti-Dumping Tariff : 2014-2016`, type = "f")$f$stat,
  fitstat(overdispersion$`Trade War 2018`, type = "f")$f$stat
)

# Extract mean of dependent variable
dep_means <- c(
  fitstat(overdispersion$Overall, type = "my")$my,
  fitstat(overdispersion$`Anti-Dumping Tariff : 2010-2013`, type = "my")$my,
  fitstat(overdispersion$`Anti-Dumping Tariff : 2014-2016`, type = "my")$my,
  fitstat(overdispersion$`Trade War 2018`, type = "my")$my
)

# Combine into a 2-row data.frame
test <- data.frame(
  term = c("F-test", "Mean Dep. Var."),
  `Overall` = c(f_stats[1], dep_means[1]),
  `AD 2010–2013` = c(f_stats[2], dep_means[2]),
  `AD 2014–2016` = c(f_stats[3], dep_means[3]),
  `Trade War 2018` = c(f_stats[4], dep_means[4]),
  check.names = FALSE
)

# Display results
overdispersion_change = modelsummary(
  models = overdispersion,
  stars = TRUE,
  coef_map = c(dispersion = "Dispersion",tariff = "Tariff"),
  gof_omit = c("Within|AIC|BIC|RMSE|Std."),
  fmt = fmt_significant(3),
  add_rows = test,
  # output = "latex"
)
writeLines(as.character(overdispersion_change), "output/regression/quality_shift/overdispersion_change.tex")


# Extract R²
ggplot(dispersion_dt, aes(x = tariff, y = dispersion)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  # annotate("text", x = Inf, y = Inf, label = r2_label, 
  #          hjust = 1.1, vjust = 1.5, size = 5) +
  labs(
    # title = "Tariff vs. Dispersion of Module Efficiency",
    x = "Tariff Level",
    y = "Dispersion (Var / Mean)"
  ) +
  theme_minimal()
ggsave("output/figures/tariff/dispersion_shift_linear.pdf", width = 10, height = 8)

# Extract R²
ggplot(dispersion_dt, aes(x = tariff, y = dispersion)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_smooth(method = "glm", color = "black", se = TRUE, method.args = list(family = gaussian(link = "log"))) +
  labs(
    # title = "Relationship Between Tariff and Quality Dispersion",
    x = "Tariff Level",
    y = "Dispersion of Module Efficiency (Var/Mean)"
  ) +
  theme_classic()
ggsave("output/figures/tariff/dispersion_shift_non_linear.pdf", width = 10, height = 8)

# Alternative Strategy - Rank Sales ----------------------------------------------------
eff_module = unique(tts[, .(efficiency_module, module_model)])

sales = tts[, .N, by = .(module_manufacturer, module_model, year)]
sales = merge(sales, eff_module, by = "module_model")
setorder(sales, module_manufacturer, year, -N)
sales[, rank := seq_len(.N), by = .(module_manufacturer, year)]
top2 <- sales[rank <= 2, .(
  efficiency = weighted.mean(efficiency_module, w = N, na.rm = TRUE)
), by = .(module_manufacturer, year, rank)]
eff_wide <- dcast(top2, module_manufacturer + year ~ rank, value.var = "efficiency")
eff_wide[, quality_ratio := `1` / `2`]

