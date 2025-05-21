# We estimate the demand

library(arrow)
library(car)
library(data.table)
library(fixest)
library(ggplot2)
library(modelsummary)
library(performance)
library(tibble)
library(pscl)

# Data --------------------------------------------------------------------
# demand = fread(data_final("demand_final.csv"))
demand = fread(data_final("demand_final_alt.csv"))

# Cleaning ----------------------------------------------------------------

demand[, se_zip_county := paste0(county, zip_code)]

# IV ----------------------------------------------------------------------
# POISSON CF --------
linear_ols_net = feols(log(price_w) ~ rebate_w + elec_price + PV_system_size_DC + PV_system_size_DC^2 + population_density + median_home_value + rebate_w 
                       | year + county + origin , cluster = ~zip_code, data = demand[year %in% 2010:2018], data.save = T)
demand_used = setDT(linear_ols_net$data)
demand_used = demand_used[!is.na(median_home_value)]
demand_used[, res_net := linear_ols_net$residuals]

demand_pois_cf_net = fepois(demand ~ log(price_w) + res_net + rebate_w  + PV_system_size_DC + PV_system_size_DC^2 + population_density + median_home_value 
                            | year + county + origin, cluster = ~ zip_code, data = demand_used[year %in% 2010:2018])

demand_pois_cf_net_ng = fenegbin(demand ~ log(price_w) + res_net + rebate_w  + PV_system_size_DC + PV_system_size_DC^2 + population_density + median_home_value + rebate_w
                            | year + county + origin, 
                        cluster = "se_zip_county", data = demand_used[year %in% 2010:2018])

# Log ---------------------------------------------------------------------

log_demand = feglm(nonz ~ net_price + res_net + rebate_w  + PV_system_size_DC + PV_system_size_DC^2 + population_density + median_home_value
                   | year + county + origin, 
                   family = binomial(link = "logit"),cluster = ~ zip_code, data = demand_used)

elas = log_demand$coefficients[["net_price"]]*mean(demand_used$net_price)*(1 - mean(demand_used$demand))

# Elasticity & Delta Method --------------------------------------------------------------
elasticity_p_value = function(data,regression_object, coef_name, net_price = TRUE,linear_estimator = TRUE){
  mean_install = mean(data$demand)
  if (net_price == FALSE){mean_price = mean(data$price_w)}else{mean_price = mean(data$net_price)}
  
  if (linear_estimator == TRUE){
  elas_pv = deltaMethod(object = regression_object$coefficients,
                        g = as.character((regression_object$coefficients[[coef_name]] * mean_price) / mean_install),
                        vcov = vcov(regression_object))
  }
  else {
    elas_pv = deltaMethod(object = regression_object$coefficients,
                          g = as.character(regression_object$coefficients[[coef_name]] * mean_price),
                          vcov = vcov(regression_object))
  }
  
  return(elas_pv)
}

# Linear
elas_iv = elasticity_p_value(demand, demand_iv, coef_name = "fit_net_price")

# Non Linear
elas_poisson_cf_net = elasticity_p_value(demand, demand_pois_cf_net, coef_name = "net_price", linear_estimator = FALSE)

models <- c("IV_net", "Poisson_CF_net")

elasticities <- list(elas_iv,elas_poisson_cf_net)

# build data frame for elasticities
elasticities_df <- data.frame(
  model    = models,
  Estimate = sapply(elasticities, function(x) x$Estimate),
  SE       = sapply(elasticities, function(x) x$SE),
  row.names = NULL,
  stringsAsFactors = FALSE
)

# IV model: only Cragg-Donald
iv_stats <- fitstat(demand_iv, type = "cd")

# Poisson models
overdispersion_pois_cf_net <- check_overdispersion(demand_pois_cf_net)

# 2. Build a clean summary data frame
elasticities_wide <- data.frame(
  Term = "Elasticity",
  # OLS_Net = round(elasticities_df$Estimate[elasticities_df$model == "OLS_net"], 3),
  IV_Net = round(elasticities_df$Estimate[elasticities_df$model == "IV_net"], 3),
  # Poisson_Net = round(elasticities_df$Estimate[elasticities_df$model == "Poisson_net"], 3),
  # NB_Poisson_Net = round(elasticities_df$Estimate[elasticities_df$model == "NB_Poisson_net"], 3),
  Poisson_CF_Net = round(elasticities_df$Estimate[elasticities_df$model == "Poisson_CF_net"], 3)
)

fit_tests_wide <- data.frame(
  Term = c("Cragg-Donald", "Dispersion ratio"),
  # OLS_Net = c(NA, NA),
  IV_Net = c(round(iv_stats$cd, 2), NA),
  # Poisson_Net = c(NA, round(overdispersion_pois_net$dispersion_ratio, 2)),
  # NB_Poisson_Net = c(NA, round(overdispersion_nbpois$dispersion_ratio, 2)),
  Poisson_CF_Net = c(NA, round(overdispersion_pois_cf_net$dispersion_ratio, 2))
)
added_row = rbind(elasticities_wide, fit_tests_wide)

# List of all your models
demand_models <- list(
  # "OLS" = demand_ols_net,
  "IV" = demand_iv,
  # "Poisson" = demand_pois_net,
  # "Negative Binomial" = demand_qpois_net,
  "Poisson CF" = demand_pois_cf_net
)

coef_name = c(
  "net_price" = "Net Price",
  "fit_net_price" = "Net Price",
  "I(net_price^2)" = "(Net Price)^2"
  # "rebate_w" = "Rebate ($/W)",
  # "elec_price" = "Elec. Price ($)"
)

gof_list <- tribble(
  ~raw,                  ~clean,           ~fmt,
  "nobs",                "Num.Obs",        "%.0f",
  "r.squared",           "R2",             "%.3f",
  "adj.r.squared",       "R2-Adj.",        "%.3f",
  "FE: county",          "FE: County",     "%.0f",
  "FE: year",            "FE: Year",       "%.0f",
)

demand_table = modelsummary(
  models = demand_models,
  star = TRUE,
  coef_map = coef_name,
  gof_map = gof_list,
  add_rows = added_row,
  output = "latex"
)
writeLines(as.character(demand_table), "output/regression/demand_estimation/demand_second_stage.tex")

demand_models_first_stage <- list(
  "IV First Stage" = summary(demand_iv, stage = 1),
  "OLS for CF" = summary(linear_ols_net)
)

coef_name = c(
  "net_price" = "Net Price",
  "log(tariff)" = "ln Tariff",
  "tariff" = "Tariff",
  "fit_net_price" = "Net Price",
  "I(net_price^2)" = "(Net Price)^2",
  "rebate_w" = "Rebate ($/W)",
  "elec_price" = "Elec. Price ($)"
)

first_stage = modelsummary(
  models = demand_models_first_stage,
  star = TRUE,
  coef_map = coef_name,
  gof_map = gof_list,
  output = "latex"
)
writeLines(as.character(first_stage), "output/regression/demand_estimation/demand_second_stage.tex")


# Neg Bin Period ------------------------------------------------------

full_formula = as.formula(demand_extensive ~ net_price + rebate_w + elec_price + PV_system_size_DC + PV_system_size_DC^2 + population_density)

# Estimation of Negative Binomial models with feglm
neg_bin_demand = list(
  "Overall" = list(
    fenegbin(full_formula, fixef = c("year", "county"), cluster = ~ zip_code, data = demand[!year %in% 2010:2013])
  ),
  
  # "Anti-Dumping : 2010 - 2013" = list(
  #   fenegbin(full_formula, fixef = c("year", "county"), cluster = ~ zip_code, data = demand[year %in% 2010:2013]),
  #   fenegbin(full_formula, fixef = c("year", "county"), cluster = ~ zip_code, data = demand[year %in% 2010:2013])
  # ),
  
  "Anti-Dumping : 2014 - 2016" = list(
    fenegbin(full_formula, fixef = c("year", "county"), cluster = ~ zip_code, data = demand[year %in% 2013:2016])
  ),
  
  "Trade War 2018" = list(
    fenegbin(full_formula, fixef = c("year", "county"), cluster = ~ zip_code, data = demand[year %in% 2017:2018])
  )
)

elas_neg_overall = mean(demand[!year %in% 2010:2013]$demand_extensive)*neg_bin_demand$Overall[[1]]$coefficients[["net_price"]]
elas_neg_ad2 = mean(demand[!year %in% 2014:2016]$demand_extensive)*neg_bin_demand$`Anti-Dumping : 2014 - 2016`[[1]]$coefficients[["net_price"]]
elas_neg_st = mean(demand[!year %in% 2017:2018]$demand_extensive)*neg_bin_demand$`Trade War 2018`[[1]]$coefficients[["net_price"]]


# GRAPH -------------------------------------------------------------------

demand_shape = demand[, `:=` (predicted_demand_ols = predict(demand_ols_net),
                              predicted_demand_pois = predict(demand_pois_net),
                              predicted_demand_iv = predict(demand_iv))]

demand_shape[, `:=` (p5 = quantile(net_price, prob = c(0.05)),
                              p95 = quantile(net_price, prob = c(0.95)))]

ggplot() +
  geom_ribbon(data = demand_shape,
    aes(x = predicted_demand_ols, ymin = p5, ymax = p95), fill = "lightgrey", alpha = 0.3) +
  geom_point(data = demand_shape, 
    aes(x = predicted_demand_ols, y = net_price), alpha = 0.2) +
  geom_smooth(data = demand_shape, 
    aes(x = predicted_demand_ols, y = net_price),method = "gam", se = TRUE, size = 1.2) +
  scale_x_continuous(limits = c(0, NA)) +
  labs(
    x = "Installation Rate (system per 1000 inhabitants)",
    y = "Price ($/W)"
  ) +
  theme_classic()

ggplot() +
  geom_ribbon(data = demand_shape,
    aes(x = predicted_demand_pois, ymin = p5, ymax = p95), fill = "lightgrey", alpha = 0.3) +
  geom_point(data = demand_shape, 
    aes(x = predicted_demand_pois, y = net_price), alpha = 0.2) +
  geom_smooth(data = demand_shape, 
    aes(x = predicted_demand_pois, y = net_price),method = "gam", se = TRUE, size = 1.2) +
  scale_x_continuous(limits = c(0, NA)) +
  labs(
    x = "Installation Rate (system per 1000 inhabitants)",
    y = "Price ($/W)"
  ) +
  theme_classic()

ggplot() +
  geom_ribbon(data = demand_shape,
    aes(x = predicted_demand_iv, ymin = p5, ymax = p95), fill = "lightgrey", alpha = 0.3) +
  geom_point(data = demand_shape, 
    aes(x = predicted_demand_iv, y = net_price), alpha = 0.2) +
  geom_smooth(data = demand_shape, 
    aes(x = predicted_demand_iv, y = net_price),method = "gam", se = TRUE, size = 1.2) +
  scale_x_continuous(limits = c(0, NA)) +
  labs(
    x = "Installation Rate (system per 1000 inhabitants)",
    y = "Price ($/W)"
  ) +
  theme_classic()

# Hurdle ------------------------------------------------------------------
dt_hurdle = copy(demand)
dt_hurdle = dt_hurdle[!is.na(median_home_value)]
resid = feols(net_price ~ tariff + PV_system_size_DC + PV_system_size_DC^2 + population_density + median_home_value + rebate_w 
                       | year + county + origin, cluster = ~zip_code, data = dt_hurdle,)

dt_hurdle[, res := resid$residuals]

demand_hurdle = hurdle(demand ~ net_price + PV_system_size_DC + PV_system_size_DC^2 + population_density + median_home_value
                       + factor(year) + factor(county) + factor(origin)
                       | net_price + res +  rebate_w + mean_week_wage + PV_system_size_DC + PV_system_size_DC^2 + population_density + median_home_value + elec_price, 
                       data = dt_hurdle)
summary(demand_hurdle)
pR2(demand_hurdle)
compare_performance(
  demand_pois_net,
  demand_pois_cf_net, 
  demand_hurdle,
  metrics = "common"
)
# Confusion Matrix
prob_positive <- predict(demand_hurdle, type = "zero")
pred_positive <- predict(demand_hurdle, type = "zero")
threshold = 0.5
predicted_adoption <- ifelse(pred_positive > threshold, 1, 0)
actual_adoption <- ifelse(demand$demand_extensive > 0, 1, 0)
confusion_matrix = table(predicted_adoption, actual_adoption)
recall_pos = confusion_matrix[2,2]/(confusion_matrix[2,2]+confusion_matrix[1,2])
recall_neg = confusion_matrix[1,1]/(confusion_matrix[2,1] + confusion_matrix[1,1])

# Elasticity Hurdle
# Predicted probability of positive adoption (first hurdle)
prob_positive <- predict(demand_hurdle, type = "zero")
# Predicted expected count, conditional on adoption (second part)
expected_count <- predict(demand_hurdle, type = "count")
# Full expected value: probability * expected count
predicted_total <- prob_positive * expected_count
# Mean predicted demand
mean_predicted_demand <- mean(predicted_total)

# Coefficients for the first stage (zero part)
coef_zero <- coef(summary(demand_hurdle))$zero
# Coefficients for the second stage (positive counts)
coef_count <- coef(summary(demand_hurdle))$count

mean_net_price <- mean(demand$net_price, na.rm = TRUE)
mean_prob_positive <- mean(prob_positive)

# 1. Zero part (adoption probability effect)
beta_zero_net_price <- coef_zero["net_price", "Estimate"]

# Marginal effect on adoption probability
marginal_effect_zero <- beta_zero_net_price * mean_prob_positive * (1 - mean_prob_positive)

# 2. Count part (intensity effect)
beta_count_net_price <- coef_count["net_price", "Estimate"]

# 3. Combine both parts
# Full marginal effect = effect on adoption + effect on conditional positive outcomes
total_marginal_effect = (marginal_effect_zero * mean(expected_count)) + (mean_prob_positive * beta_count_net_price * mean(expected_count))

# 4. Elasticity formula
elasticity_net_price = total_marginal_effect * (mean_net_price / mean_predicted_demand)


# ggplot() +
#   # geom_ribbon(data = demand_shape,
#   #   aes(x = net_price, ymin = p5, ymax = p95), fill = "lightgrey", alpha = 0.3) +
#   geom_point(data = demand_shape, 
#     aes(x = net_price, y = predicted_demand_pois), alpha = 0.2) +
#   geom_smooth(data = demand_shape, 
#     aes(x = net_price, y = predicted_demand_pois),method = "gam", se = TRUE, size = 1.2) +
#   scale_x_continuous(limits = c(min(demand_shape$net_price), NA)) +
#   labs(
#     y = "Installation Rate (system per 1000 inhabitants)",
#     x = "Price ($/W)"
#   ) +
#   theme_classic()


# Hurdle 2: ---------------------------------------------------------------
dt_hurdle = copy(demand)
dt_hurdle = dt_hurdle[!is.na(median_home_value)]
dt_hurdle[, adoption := ifelse(demand > 0, 1, 0)]
feglm(adoption ~ net_price*tariff + rebate_w + mean_week_wage + PV_system_size_DC + PV_system_size_DC^2 
      + population_density + median_home_value + elec_price
      |year + county + origin ,
      family = "logit", data = dt_hurdle)

hurdle_fe = femlm(
  # 1) COUNT part: covariates + FEs
  demand ~ 
    net_price 
  + PV_system_size_DC 
  + I(PV_system_size_DC^2)
  + population_density 
  + median_home_value
  | year + county + origin
  
  # 2) ZERO part: same as your hurdle(...), with CF residual
  | net_price 
  + res 
  + rebate_w 
  + mean_week_wage 
  + PV_system_size_DC 
  + I(PV_system_size_DC^2)
  + population_density 
  + median_home_value 
  + elec_price 
  ~ 0
  
  # 3) Choose distribution & link for the zero‐part
  , family  = "logit"  
  
  # 4) Data and clustering
  , data     = dt_hurdle
  , cluster  = ~ tract
)

summary(hurdle_fe)



# Draft -------------------------------------------------------------------

# Define your subperiods
demand[, period := fcase(year <= 2013, "ad1",
                         year >= 2014 & year <= 2016, "ad2",
                         year >= 2017 & year <= 2018, "st",
                         default = "post_st")]

# Loop over periods
results_list = list()

for (p in unique(demand$period)) {
  
  cat("\n\n--- Estimating for period:", p, "---\n")
  
  # Filter subperiod data
  data_p = demand[period == p & !is.na(median_home_value)]
  
  # First stage: Instrument net_price
  linear_ols_net = feols(
    net_price ~ log(tariff) + PV_system_size_DC + I(PV_system_size_DC^2) +
      population_density + median_home_value + rebate_w |
      year + zip_code + origin + installer_name,
    cluster = ~zip_code,
    data = data_p,
    data.save = TRUE
  )
  
  # Extract data and residuals
  demand_used = setDT(linear_ols_net$data)
  demand_used[, res_net := linear_ols_net$residuals]
  
  # Second stage: Poisson regression with interaction
  demand_pois_cf_net = fepois(
    demand_zip_code ~ net_price * qual_qt + res_net + rebate_w + PV_system_size_DC +
      I(PV_system_size_DC^2) + population_density + median_home_value |
      year + county + origin,
    cluster = ~zip_code,
    data = demand_used
  )
  
  # Store result
  results_list[[p]] <- demand_pois_cf_net
}

# Convexity Calculus ------------------------------------------------------
# ---- Step 1: Estimated coefficients ----
a <- 0.0001     # Coefficient on p^2
b <- -0.2       # Coefficient on p
p_calib <- 6.5  # Calibration price (you choose based on sample)
q_calib <- a * p_calib^2 + b * p_calib  # estimated q at p_calib

# ---- Step 2: Calibrate the constant term c ----
# Assume q = ap^2 + bp + c → c = q - ap^2 - bp
c <- q_calib - a * p_calib^2 - b * p_calib

# ---- Step 3: Define inverse demand function p(q) and its derivatives ----

# p(q): invert q = a p^2 + b p + c ⇒ solve quadratic: a p^2 + b p + (c - q) = 0
inverse_demand <- function(q) {
  discrim <- b^2 - 4 * a * (c - q)
  if (discrim < 0) return(NA)
  p <- (-b - sqrt(discrim)) / (2 * a)  # economically relevant root
  return(p)
}

# First derivative p'(q)
dp_dq <- function(q) {
  sqrt_term <- sqrt(b^2 - 4 * a * (c - q))
  return(1 / sqrt_term)
}

# Second derivative p''(q)
d2p_dq2 <- function(q) {
  base <- b^2 - 4 * a * (c - q)
  return(2 * a / (base^(3/2)))
}

# ---- Step 4: Compute convexity parameter at a range of quantities ----
q_grid <- seq(q_calib * 0.5, q_calib * 1.5, length.out = 100)

convexity_df <- data.frame(
  q = q_grid,
  p = sapply(q_grid, inverse_demand),
  dp = sapply(q_grid, dp_dq),
  d2p = sapply(q_grid, d2p_dq2)
)

# Convexity parameter 1/εms
convexity_df$inv_eps_ms <- with(convexity_df, (d2p / dp) * q)

# ---- Step 5: Plot convexity ----
library(ggplot2)

ggplot(convexity_df, aes(x = q, y = inv_eps_ms)) +
  geom_line(color = "steelblue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = expression("Convexity parameter" ~ (1/epsilon[ms])),
    x = "Quantity (q)",
    y = expression(1 / epsilon[ms])
  ) +
  theme_minimal()

