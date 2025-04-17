# We estimate the demand

library(arrow)
library(data.table)
library(fixest)
library(modelsummary)
library(ggplot2)
library(car)

# Data --------------------------------------------------------------------

demand = fread(data_final("demand_final.csv"))

# OLS - Demand Analysis ---------------------------------------------------
demand[, net_price := price_w-rebate_w]
demand[, net_price_sq := net_price^2]
demand[, price_w_sq := price_w^2]
# demand[, tariff_sq := tariff^2]
# demand = demand[!is.na(price_w) & !is.na(h_median)]
demand = demand[price_w > 1 & price_w < 10,]
demand = demand[rebate_w < price_w]

nrow(demand[population < 10,])
# demand = demand[population > 10,]

# OLS ---------------------------------------------------------------------
demand_ols_net = feols(demand_extensive ~ net_price + net_price^2 + PV_system_size_DC + PV_system_size_DC^2 + elec_price + rebate_w + mean_week_wage + educ +
                     population_density + median_home_value
                   | year + county, cluster = ~zip_code, data = demand)

demand_ols = feols(demand_extensive ~ price_w + price_w^2 + PV_system_size_DC + PV_system_size_DC^2 + elec_price + rebate_w + mean_week_wage + educ +
                     population_density + median_home_value
                   | year + county, cluster = ~zip_code, data = demand)

# IV ----------------------------------------------------------------------
# THINK ABOUT THE SENSE OF INCLUDING ELEC PRICE
demand_iv = feols(demand_extensive ~ PV_system_size_DC + PV_system_size_DC^2 + population_density + median_home_value + educ + elec_price |
                        year + county | price_w ~ mean_week_wage + rebate_w , 
                      cluster = ~zip_code,  data = demand)

demand_iv_net = feols(demand_extensive ~ PV_system_size_DC + PV_system_size_DC^2 + population_density + median_home_value + educ + elec_price |
                    year + county | net_price ~ mean_week_wage + rebate_w , 
                  cluster = ~zip_code,  data = demand)

# POISSON -----------------------------------------------------------------
# Question the presence of elec_price
demand_pois = fepois(demand_extensive ~ price_w + price_w^2 + PV_system_size_DC + PV_system_size_DC^2 + population_density + median_home_value + elec_price 
                     | year + county, cluster = ~zip_code, data = demand)

demand_pois_net = fepois(demand_extensive ~ net_price + net_price^2 + PV_system_size_DC + PV_system_size_DC^2 + population_density + median_home_value + elec_price 
                     | year + county, cluster = ~zip_code, data = demand)

# demand_pois = fepois(demand_extensive ~ price_w  + PV_system_size_DC + PV_system_size_DC^2 + population_density + median_home_value + elec_price
#                      | year + county, cluster = ~zip_code, data = demand)
# 
# ## Close to what Gillingham & Tsevatanov (2019) have estimated
# demand_pois_net = fepois(demand_extensive ~ net_price  + PV_system_size_DC + PV_system_size_DC^2 + population_density + median_home_value + elec_price
#                      | year + county, cluster = ~zip_code, data = demand)

# POISSON CF --------------------------------------------------------------
linear_ols = feols(price_w ~ rebate_w + mean_week_wage + PV_system_size_DC + PV_system_size_DC^2 + population_density + median_home_value + elec_price
                   | year + county, cluster = ~zip_code, data = demand)
demand[, res := linear_ols$residuals]

demand_pois_cf = fepois(demand_extensive ~ price_w + price_w^2 + res + PV_system_size_DC + PV_system_size_DC^2 + population_density + median_home_value 
                     | year + county, cluster = ~zip_code, data = demand)

linear_ols_net = feols(net_price ~ rebate_w + mean_week_wage + PV_system_size_DC + PV_system_size_DC^2 + population_density + median_home_value + elec_price
                   | year + county, cluster = ~zip_code, data = demand)
demand[, res_net := linear_ols_net$residuals]

demand_pois_cf_net = fepois(demand_extensive ~ net_price + net_price^2 + res_net + PV_system_size_DC + PV_system_size_DC^2 + population_density + median_home_value 
                     | year + county, cluster = ~zip_code, data = demand)

# ELASTICITY & ELASTICITY --------------------------------------------------------------
elasticity_p_value = function(data,regression_object, coef_name, net_price = FALSE,linear_estimator = TRUE){
  mean_install = mean(data$demand_extensive)
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
elas_ols = elasticity_p_value(demand, demand_ols, coef_name = "price_w")
elas_ols_net = elasticity_p_value(demand, demand_ols_net, coef_name = "net_price", net_price = TRUE)

elas_iv = elasticity_p_value(demand, demand_iv, coef_name = "fit_price_w")
elas_iv_net = elasticity_p_value(demand, demand_iv_net, coef_name = "fit_net_price", net_price = TRUE)

# Non Linear
elas_poisson = elasticity_p_value(demand, demand_pois, coef_name = "price_w", linear_estimator = FALSE)
elas_poisson_net = elasticity_p_value(demand, demand_pois_net, coef_name = "net_price", net_price = TRUE, linear_estimator = FALSE)

elas_poisson_cf = elasticity_p_value(demand, demand_pois_cf, coef_name = "price_w", linear_estimator = FALSE)
elas_poisson_cf_net = elasticity_p_value(demand, demand_pois_cf_net, coef_name = "net_price", net_price = TRUE, linear_estimator = FALSE)

models <- c("OLS", "OLS_net", 
            "IV", "IV_net", 
            "Poisson", "Poisson_net", 
            "Poisson_CF", "Poisson_CF_net")

elasticities <- list(elas_ols, elas_ols_net,
                     elas_iv, elas_iv_net,
                     elas_poisson, elas_poisson_net,
                     elas_poisson_cf, elas_poisson_cf_net)

# build data frame
elasticities_df <- data.frame(
  model    = models,
  Estimate = sapply(elasticities, function(x) x$Estimate),
  SE       = sapply(elasticities, function(x) x$SE),
  row.names = NULL,
  stringsAsFactors = FALSE
)

# elas_ols = demand_ols$coefficients[["price_w"]] * mean_price/ mean_install
# elas_ols_net = demand_ols_net$coefficients[["net_price"]] * mean_net_price/ mean_install
# elas_iv = demand_iv$coefficients[["fit_price_w"]] * mean_price/ mean_install
# elas_iv_net = demand_iv_net$coefficients[["fit_net_price"]] * mean_net_price/ mean_install

# elas_poisson = demand_pois$coefficients[["price_w"]] *  mean_price # consistent results for demand only if used with gross price
# elas_poisson_net = demand_pois_net$coefficients[["net_price"]] *  mean_net_price # consistent results for demand only if used with gross price
# elas_poisson_cf = demand_pois_cf$coefficients[["price_w"]] *  mean_price # consistent results for demand only if used with gross price
# elas_poisson_cf_net = demand_pois_cf_net$coefficients[["net_price"]] *  mean_net_price # consistent results for demand only if used with gross price


# P-VALUE ELASTICITY ------------------------------------------------------

test_elas = elasticity_p_value(demand_iv, coef_name = "fit_price_w", mean_install = mean_install, mean_price = mean_price)


# GRAPH -------------------------------------------------------------------

demand_shape = demand[, `:=` (predicted_demand_ols = predict(demand_ols),
                              predicted_demand_pois = predict(demand_pois),
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
