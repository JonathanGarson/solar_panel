# We propose a CF approach to identify the demand function

library(arrow)
library(boot)
library(data.table)
library(fixest)
library(modelsummary)
library(ggplot2)
library(VGAM)

# Data --------------------------------------------------------------------

demand = setDT(read_parquet(data_final("tts_final.parquet")))
# demand = setDT(fread(data_final("demand_final_alt.csv")))


# Function ----------------------------------------------------------------
# Function to collect regression residuals and attach to original model data
collect_cf_resid <- function(model, resid_col_name = "residual") {
  data <- as.data.table(model$model)
  data[, (resid_col_name) := residuals(model)]
  return(data)
}

# Cleaning ----------------------------------------------------------------
demand[,price_sq := price_w^2] 
# demand[year %in% 2010:2013, qual_qt := cut(efficiency_module, 
#                                            breaks = quantile(efficiency_module, 
#                                                              probs = seq(0,1,0.20), 
#                                                              na.rm=TRUE),
#                                            include.lowest=TRUE,
#                                            labels=paste0("Q",1:5)
# )]
# demand[year %in% 2014:2016, qual_qt := cut(efficiency_module, 
#                                            breaks = quantile(efficiency_module, 
#                                                              probs = seq(0,1,0.20), 
#                                                              na.rm=TRUE),
#                                            include.lowest=TRUE,
#                                            labels=paste0("Q",1:5)
# )]
# demand[year %in% 2017:2018, qual_qt := cut(efficiency_module, 
#                                            breaks = quantile(efficiency_module, 
#                                                              probs = seq(0,1,0.20), 
#                                                              na.rm=TRUE),
#                                            include.lowest=TRUE,
#                                            labels=paste0("Q",1:5)
# )]

decile_breaks_2011 <- quantile(
  demand[year == 2011, efficiency_module],
  probs = seq(0, 1, 0.20),
  na.rm = TRUE
)

demand[year %in% 2010:2013, qual_qt := cut(efficiency_module,
                                        breaks = decile_breaks_2011,
                                        include.lowest = TRUE,
                                        labels = paste0("Q", 1:5))]

decile_breaks_2013 <- quantile(
  demand[year == 2013, efficiency_module],
  probs = seq(0, 1, 0.20),
  na.rm = TRUE
)

demand[year %in% 2014:2016, qual_qt := cut(efficiency_module,
                                        breaks = decile_breaks_2013,
                                        include.lowest = TRUE,
                                        labels = paste0("Q", 1:5))]

decile_breaks_2017 <- quantile(
  demand[year == 2017, efficiency_module],
  probs = seq(0, 1, 0.20),
  na.rm = TRUE
)

demand[year %in% 2017:2018, qual_qt := cut(efficiency_module,
                                        breaks = decile_breaks_2017,
                                        include.lowest = TRUE,
                                        labels = paste0("Q", 1:5))]


vars = c("tariff", "population_density", "population", "pct_bachelor_estimate", "median_home_value", "median_household_income",
         "PV_system_size_DC", "efficiency_module", "installer_name", "origin", "year_quarter", "county")

demand <- demand[complete.cases(demand[, ..vars])]

# Estimating Demand -------------------------------------------------------

# Prepare datasets
periods <- list(
  "2010–2013" = demand[year %in% 2010:2013],
  "2014–2016" = demand[year %in% 2014:2016],
  "2017–2018" = demand[year %in% 2017:2018]
)

# Estimate the first-stage control function for each period
for (p in names(periods)) {
  data_p <- periods[[p]]
  cf_model <- feols(log(price_w) ~ population_density + population +
                      pct_bachelor_estimate + median_home_value + median_household_income +
                      PV_system_size_DC + I(PV_system_size_DC^2) + rebate_w + elec_price
                    | year_quarter + origin + installer_name + county,
                    cluster = "zip_code",
                    data = data_p)
  periods[[p]][, res := resid(cf_model)]
}

# Table for first stage
model_1st = list(
  "Anti-Dumping: 2010-2013" = list(
    "Poisson" = feols(log(price_w) ~ log(tariff) + population_density + population + pct_bachelor_estimate + median_home_value + median_household_income + 
                         PV_system_size_DC + I(PV_system_size_DC^2)
                       | year_quarter + origin + installer_name + county,
                       cluster = "zip_code", data = demand[year %in% 2010:2013])),
  
  "Anti-Dumping: 2014-2016" = list(
    "Poisson" = feols(log(price_w) ~ log(tariff) + population_density + population + pct_bachelor_estimate + median_home_value + median_household_income + 
                          PV_system_size_DC + I(PV_system_size_DC^2)
                        | year_quarter + origin + installer_name + county,
                       cluster = "zip_code", data = demand[year %in% 2014:2016])),
  
  "Trade War" = list(
    "Poisson" = feols(log(price_w) ~ log(tariff) + population_density + population + pct_bachelor_estimate + median_home_value + median_household_income + 
                          PV_system_size_DC + I(PV_system_size_DC^2)
                        | year_quarter + origin + installer_name + county,
                       cluster = "zip_code", data = demand[year %in% 2017:2018]))
  )

demand_1stage = modelsummary(
  models = model_1st,
  stars = TRUE,
  shape = "cbind",
  coef_map = c("log(tariff)" = "ln Tariff"),
  gof_omit = "AIC|Within|Std.|BIC|RMSE",
  output = "latex"
  )
writeLines(as.character(demand_1stage), "output/regression/demand_estimation/cf_function.tex")

# Estimate Poisson for first two periods, NB for last
models <- list(
  "Anti-Dumping: 2010-2013" = list(
    "IV" = feols(demand_zip_code ~ price_w + price_w^2  + res + population_density + population + pct_bachelor_estimate + 
             median_home_value + median_household_income + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price + rebate_w
           | year + origin + county,
           cluster = "zip_code", data = periods[["2010–2013"]]),
    "Poisson" = fepois(demand_zip_code ~ price_w + price_w^2  + res + population_density + population + pct_bachelor_estimate + 
             median_home_value + median_household_income + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price + rebate_w
           | year + origin + county,
           cluster = "zip_code", data = periods[["2010–2013"]]),
    "NegBin" = fenegbin(demand_zip_code ~ price_w + price_w^2 + res + population_density + population + pct_bachelor_estimate +
               median_home_value + median_household_income + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price
             | year + origin + county,
             cluster = "zip_code", data = periods[["2010–2013"]])),
  
  "Anti-Dumping: 2014-2016" = list(
    "IV" = feols(demand_zip_code ~ price_w + price_w^2 + res + population_density + population + pct_bachelor_estimate +
             median_home_value + median_household_income + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price + rebate_w
           | year + origin + county,
           cluster = "zip_code", data = periods[["2014–2016"]]),
    "Poisson" = fepois(demand_zip_code ~ price_w + price_w^2 + res + population_density + population + pct_bachelor_estimate +
             median_home_value + median_household_income + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price
           | year + origin + county,
           cluster = "zip_code", data = periods[["2014–2016"]]),
    "NegBin" = fenegbin(demand_zip_code ~ price_w + price_w^2 + res + population_density + population + pct_bachelor_estimate +
                          median_home_value + median_household_income + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price
                        | year + origin + county,
                        cluster = "zip_code", data = periods[["2014–2016"]])),
  
  "Trade War" = list(
    "IV" = feols(demand_zip_code ~ price_w + price_w^2 + res + population_density + population + pct_bachelor_estimate +
                         median_home_value + median_household_income + PV_system_size_DC + I(PV_system_size_DC^2)+ elec_price
                       | year + origin + county,
                       cluster = "zip_code", data = periods[["2017–2018"]]),
    "Poisson" = fepois(demand_zip_code ~ price_w + price_w^2 + res + population_density + population + pct_bachelor_estimate +
                         median_home_value + median_household_income + PV_system_size_DC + I(PV_system_size_DC^2)+ elec_price
                       | year + origin + county,
                       cluster = "zip_code", data = periods[["2017–2018"]]),
    "NegBin" = fenegbin(demand_zip_code ~ price_w + price_w^2 + res + population_density + population + pct_bachelor_estimate +
                          median_home_value + median_household_income + PV_system_size_DC + I(PV_system_size_DC^2)+ elec_price
                        | year + origin + county,
                        cluster = "zip_code", data = periods[["2017–2018"]]))
)

# Extract theta from NB model
theta_nb_ad1 <- fitstat(models$`Anti-Dumping: 2010-2013`[["NegBin"]], type = "theta")
theta_nb_ad2 <- fitstat(models$`Anti-Dumping: 2014-2016`[["NegBin"]], type = "theta")
theta_nb_st <- fitstat(models$`Trade War`[["NegBin"]], type = "theta")

# Extract log-likelihoods and AICs for Poisson
loglik_ad1_pois <- logLik(models$`Anti-Dumping: 2010-2013`[["Poisson"]])
loglik_ad2_pois <- logLik(models$`Anti-Dumping: 2014-2016`[["Poisson"]])
loglik_st_pois  <- logLik(models$`Trade War`[["Poisson"]])

aic_ad1_pois <- AIC(models$`Anti-Dumping: 2010-2013`[["Poisson"]])
aic_ad2_pois <- AIC(models$`Anti-Dumping: 2014-2016`[["Poisson"]])
aic_st_pois  <- AIC(models$`Trade War`[["Poisson"]])

# Extract log-likelihoods and AICs for Negative Binomial
loglik_ad1_nb <- logLik(models$`Anti-Dumping: 2010-2013`[["NegBin"]])
loglik_ad2_nb <- logLik(models$`Anti-Dumping: 2014-2016`[["NegBin"]])
loglik_st_nb  <- logLik(models$`Trade War`[["NegBin"]])

aic_ad1_nb <- AIC(models$`Anti-Dumping: 2010-2013`[["NegBin"]])
aic_ad2_nb <- AIC(models$`Anti-Dumping: 2014-2016`[["NegBin"]])
aic_st_nb  <- AIC(models$`Trade War`[["NegBin"]])

# Extract theta from NB model
my_ad1 <- mean(periods$`2010–2013`$price_w)
my_ad2 <- mean(periods$`2014–2016`$price_w)
my_st  <-  mean(periods$`2017–2018`$price_w, na.rm = TRUE)

# Add theta to modelsummary table as a goodness-of-fit row
add_bottom <- tibble::tibble(
  term = c("Overdispersion (theta)", "Mean Price", "Log-Likelihood", "AIC"),
  
  `Poisson 2010–2013`  = c(NA, my_ad1, round(loglik_ad1_pois[1], 2), round(aic_ad1_pois, 1)),
  `NegBin 2010–2013`   = c(round(theta_nb_ad1$theta, 2), my_ad1, round(loglik_ad1_nb[1], 2), round(aic_ad1_nb, 1)),
  
  `Poisson 2014–2016`  = c(NA, my_ad2, round(loglik_ad2_pois[1], 2), round(aic_ad2_pois, 1)),
  `NegBin 2014–2016`   = c(round(theta_nb_ad2$theta, 2), my_ad2, round(loglik_ad2_nb[1], 2), round(aic_ad2_nb, 1)),
  
  `Poisson 2017–2018`  = c(NA, my_st, round(loglik_st_pois[1], 2), round(aic_st_pois, 1)),
  `NegBin 2017–2018`   = c(round(theta_nb_st$theta, 2), my_st, round(loglik_st_nb[1], 2), round(aic_st_nb, 1))
)

coef_name = c(
  "log(price_w)" = "ln Price",
  "price_w" = "Price",
  "I(price_w^2)" = "Price^2",
  "res" = "\\mu"
)

custom_gof_map <- tibble::tibble(
  raw = c("pseudo.r.squared", "logLik", "AIC", "statistic", "FE: year_quarter", "FE: origin", "FE: county"),
  clean = c("Pseudo R²", "Log Likelihood", "AIC", "Wald χ²", "FE: Year Quarter", "FE: Origin", "FE: County"),
  fmt = c(3, 1, 1, 1, 0, 0, 0),
  omit = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
)

demand_1 = modelsummary(models,
             stars = TRUE,
             shape = "cbind",
             add_rows = add_bottom,
             coef_map = coef_name, 
             gof_map = custom_gof_map,
             output = "latex",
             )
writeLines(as.character(demand_1), "output/regression/demand_estimation/demand_1_robs_elec.tex")

# Elasticity by Quantile --------------------------------------------------

# Prepare datasets
periods <- list(
  "2010–2013" = demand[year %in% 2010:2013],
  "2014–2016" = demand[year %in% 2014:2016],
  "2017–2018" = demand[year %in% 2017:2018]
)

# Estimate the first-stage control function for each period
for (p in names(periods)) {
  data_p <- periods[[p]]
  cf_model <- feols(log(price_w) ~ log(tariff) + population_density + population +
                      pct_bachelor_estimate + median_home_value + median_household_income +
                      PV_system_size_DC + I(PV_system_size_DC^2)
                    | year_quarter + origin + installer_name + county,
                    data = data_p)
  periods[[p]][, res := resid(cf_model)]
}

# Estimate Poisson for first two periods, NB for last
models <- list(
  "Anti-Dumping: 2010-2013" = list(
    "Poisson" = fepois(demand_zip_code ~ price_w + price_w*qual_qt  + res + population_density + population + pct_bachelor_estimate + 
                         median_home_value + median_household_income + PV_system_size_DC + I(PV_system_size_DC^2)
                       | year_quarter + origin + county,
                       cluster = "zip_code", data = periods[["2010–2013"]]),
    "NegBin" = fenegbin(demand_zip_code ~ price_w + price_w*qual_qt+ res + population_density + population + pct_bachelor_estimate +
                          median_home_value + median_household_income + PV_system_size_DC + I(PV_system_size_DC^2)
                        | year_quarter + origin + county,
                        cluster = "zip_code", data = periods[["2010–2013"]])),
  
  "Anti-Dumping: 2014-2016" = list(
    "Poisson" = fepois(demand_zip_code ~ price_w + price_w*qual_qt + res + population_density + population + pct_bachelor_estimate +
                         median_home_value + median_household_income + PV_system_size_DC + I(PV_system_size_DC^2)
                       | year_quarter + origin + county,
                       cluster = "zip_code", data = periods[["2014–2016"]]),
    
    "NegBin" = fenegbin(demand_zip_code ~ price_w + price_w*qual_qt + res + population_density + population + pct_bachelor_estimate +
                          median_home_value + median_household_income + PV_system_size_DC + I(PV_system_size_DC^2)
                        | year_quarter + origin + county,
                        cluster = "zip_code", data = periods[["2014–2016"]])),
  
  "Trade War" = list(
    "Poisson" = fepois(demand_zip_code ~ price_w + price_w*qual_qt + res + population_density + population + pct_bachelor_estimate +
                         median_home_value + median_household_income + PV_system_size_DC + I(PV_system_size_DC^2)
                       | year_quarter + origin + county,
                       cluster = "zip_code", data = periods[["2017–2018"]]),
    "NegBin" = fenegbin(demand_zip_code ~ price_w + price_w*qual_qt + res + population_density + population + pct_bachelor_estimate +
                          median_home_value + median_household_income + PV_system_size_DC + I(PV_system_size_DC^2)
                        | year_quarter + origin + county,
                        cluster = "zip_code", data = periods[["2017–2018"]]))
)

# Extract theta from NB model
theta_nb_ad1 <- fitstat(models$`Anti-Dumping: 2010-2013`[["NegBin"]], type = "theta")
theta_nb_ad2 <- fitstat(models$`Anti-Dumping: 2014-2016`[["NegBin"]], type = "theta")
theta_nb_st <- fitstat(models$`Trade War`[["NegBin"]], type = "theta")

# Extract log-likelihoods and AICs for Poisson
loglik_ad1_pois <- logLik(models$`Anti-Dumping: 2010-2013`[["Poisson"]])
loglik_ad2_pois <- logLik(models$`Anti-Dumping: 2014-2016`[["Poisson"]])
loglik_st_pois  <- logLik(models$`Trade War`[["Poisson"]])

aic_ad1_pois <- AIC(models$`Anti-Dumping: 2010-2013`[["Poisson"]])
aic_ad2_pois <- AIC(models$`Anti-Dumping: 2014-2016`[["Poisson"]])
aic_st_pois  <- AIC(models$`Trade War`[["Poisson"]])

# Extract log-likelihoods and AICs for Negative Binomial
loglik_ad1_nb <- logLik(models$`Anti-Dumping: 2010-2013`[["NegBin"]])
loglik_ad2_nb <- logLik(models$`Anti-Dumping: 2014-2016`[["NegBin"]])
loglik_st_nb  <- logLik(models$`Trade War`[["NegBin"]])

aic_ad1_nb <- AIC(models$`Anti-Dumping: 2010-2013`[["NegBin"]])
aic_ad2_nb <- AIC(models$`Anti-Dumping: 2014-2016`[["NegBin"]])
aic_st_nb  <- AIC(models$`Trade War`[["NegBin"]])

# Extract theta from NB model
my_ad1 <- mean(periods$`2010–2013`$price_w)
my_ad2 <- mean(periods$`2014–2016`$price_w)
my_st  <-  mean(periods$`2017–2018`$price_w, na.rm = TRUE)

# Add theta to modelsummary table as a goodness-of-fit row
add_bottom <- tibble::tibble(
  term = c("Overdispersion (theta)", "Mean Price", "Log-Likelihood", "AIC"),
  
  `Poisson 2010–2013`  = c(NA, my_ad1, round(loglik_ad1_pois[1], 2), round(aic_ad1_pois, 1)),
  `NegBin 2010–2013`   = c(round(theta_nb_ad1$theta, 2), my_ad1, round(loglik_ad1_nb[1], 2), round(aic_ad1_nb, 1)),
  
  `Poisson 2014–2016`  = c(NA, my_ad2, round(loglik_ad2_pois[1], 2), round(aic_ad2_pois, 1)),
  `NegBin 2014–2016`   = c(round(theta_nb_ad2$theta, 2), my_ad2, round(loglik_ad2_nb[1], 2), round(aic_ad2_nb, 1)),
  
  `Poisson 2017–2018`  = c(NA, my_st, round(loglik_st_pois[1], 2), round(aic_st_pois, 1)),
  `NegBin 2017–2018`   = c(round(theta_nb_st$theta, 2), my_st, round(loglik_st_nb[1], 2), round(aic_st_nb, 1))
)

coef_name = c(
  "price_w" = "Price",
  "price_w:qual_qtQ2" = "Price x Q2",
  "price_w:qual_qtQ3" = "Price x Q3",
  "price_w:qual_qtQ4" = "Price x Q4",
  "price_w:qual_qtQ5" = "Price x Q5",
  "price_w:efficiency_module" = "Price x Efficiency",
  "res" = "\\mu"
)

custom_gof_map <- tibble::tibble(
  raw = c("pseudo.r.squared", "logLik", "AIC", "statistic", "FE: year_quarter", "FE: origin", "FE: county"),
  clean = c("Pseudo R²", "Log Likelihood", "AIC", "Wald χ²", "FE: Year Quarter", "FE: Origin", "FE: County"),
  fmt = c(3, 1, 1, 1, 0, 0, 0),
  omit = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
)

demand_2 = modelsummary(models,
                        stars = TRUE,
                        shape = "cbind",
                        add_rows = add_bottom,
                        coef_map = coef_name, 
                        gof_map = custom_gof_map,
                        output = "latex"
)
writeLines(as.character(demand_2), "output/regression/demand_estimation/demand_2.tex")

