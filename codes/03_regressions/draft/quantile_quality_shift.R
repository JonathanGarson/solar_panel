library(arrow)
library(data.table)
library(fixest)
library(modelsummary)
library(tibble)
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------

tts = setDT(read_parquet(data_final("tts_final.parquet")))

# RIOS-AVILA - Efficiency Decile -------------------------------------------------------

tts[, quarter_origin := paste0(year_quarter, origin)]
tts_ad1 = tts[year %in% 2010:2013]
tts_ad2 = tts[year %in% 2014:2016]
tts_st = tts[year %in% 2017:2018]

# AD1 ---------------------------------------------------------------------

# 1. « First, for all dependent and independent variables in the model (w = y, x), 
# we partial out the group fixed ejects and obtain the centered-residualized variables: » ([Rios Avila et al., 2024, p. 12]
# In our case y = log_efficiency and x = log_tariff
tts_ad1[, log_efficiency := efficiency_module]
tts_ad1[, log_tariff := log(tariff)]
tts_ad1[, log_efficiency_demean := demean(log_efficiency ~ installer_name + origin + county + year_quarter, data = tts_ad1)]
tts_ad1[, log_tariff_demean := demean(log_tariff ~ installer_name + origin + county + year_quarter, data = tts_ad1)]

# 2. « Afterward, we estimate the location model using the centered-residualized variables: » ([Rios Avila et al., 2024, p. 12]
loc_model = lm(log_efficiency_demean ~ log_tariff_demean - 1, data = tts_ad1)
tts_ad1[, resid_loc_model := resid(loc_model)]
beta_hat_loc_model = as.numeric(coef(loc_model)["log_tariff_demean"])

# 3.« Because |nuˆi| is the dependent variable for the scale model, 
# we apply the partialing out and recentering to this expression (|nuˆi|rc), 
# and use that to estimate the following model: » ([Rios Avila et al., 2024, p. 12]
tts_ad1[, abs_resid_demean := demean(abs(resid_loc_model) ~ installer_name + origin + county + year_quarter, data = tts_ad1)]
# We can estimate the scale model now
scale_model = lm(abs_resid_demean ~ log_tariff_demean -1 , data = tts_ad1)
gamma_hat_scale_model = as.numeric(coef(scale_model)[["log_tariff_demean"]])

# 4. We recompose the standardized residuals and solve for each quantile
X = model.matrix(~ log_tariff_demean - 1, data = tts_ad1)
resid_std <- (tts_ad1$log_efficiency_demean - X %*% beta_hat_loc_model) / as.numeric(X %*% gamma_hat_scale_model)

# 5. We recover each quantile
results = list()
taus <- c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90)
for (tau in taus){
moment_function <- function(q) {
  mean(resid_std <= q , na.rm = TRUE) - tau
 }
 q_tau <- uniroot(moment_function, lower = -20000, upper = 20000)$root
 beta_tau = beta_hat_loc_model + q_tau * gamma_hat_scale_model
 results[[as.character(tau)]] = beta_tau
}

results_ad1_df = do.call(rbind, lapply(names(results), function(t) {
    data.frame(tau = as.numeric(t), estimate = results[[t]])
  }))
setorder(results_ad1_df, tau)
results_ad1_df = results_ad1_df %>% mutate(period = "2010-2013")

# AD2 ---------------------------------------------------------------------
# 1. « First, for all dependent and independent variables in the model (w = y, x), 
# we partial out the group fixed ejects and obtain the centered-residualized variables: » ([Rios Avila et al., 2024, p. 12]
# In our case y = log_efficiency and x = log_tariff
tts_ad2[, log_efficiency := efficiency_module]
tts_ad2[, log_tariff := log(tariff)]
tts_ad2[, log_efficiency_demean := demean(log_efficiency ~ installer_name + origin + county + year_quarter, data = tts_ad2)]
tts_ad2[, log_tariff_demean := demean(log_tariff ~ installer_name + origin + county + year_quarter, data = tts_ad2)]

# 2. « Afterward, we estimate the location model using the centered-residualized variables: » ([Rios Avila et al., 2024, p. 12]
loc_model = lm(log_efficiency_demean ~ log_tariff_demean - 1, data = tts_ad2)
tts_ad2[, resid_loc_model := resid(loc_model)]
beta_hat_loc_model = as.numeric(coef(loc_model)["log_tariff_demean"])

# 3.« Because |nuˆi| is the dependent variable for the scale model, 
# we apply the partialing out and recentering to this expression (|nuˆi|rc), 
# and use that to estimate the following model: » ([Rios Avila et al., 2024, p. 12]
tts_ad2[, abs_resid_demean := demean(abs(resid_loc_model) ~ installer_name + origin + county + year_quarter, data = tts_ad2)]
# We can estimate the scale model now
scale_model = lm(abs_resid_demean -1 ~ log_tariff_demean , data = tts_ad2)
gamma_hat_scale_model = as.numeric(coef(scale_model)[["log_tariff_demean"]])

# 4. We recompose the standardized residuals and solve for each quantile
X = model.matrix(~ log_tariff_demean -1, data = tts_ad2)
resid_std <- (tts_ad2$log_efficiency_demean - X %*% beta_hat_loc_model) / as.numeric(X %*% gamma_hat_scale_model)

# 5. We recover each quantile
results = list()
taus <- c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90)
for (tau in taus){
  moment_function <- function(q) {
    mean(resid_std <= q , na.rm = TRUE) - tau
  }
  q_tau <- uniroot(moment_function, lower = -20000, upper = 20000)$root
  beta_tau = beta_hat_loc_model + q_tau * gamma_hat_scale_model
  results[[as.character(tau)]] = beta_tau
}

results_ad2_df = do.call(rbind, lapply(names(results), function(t) {
  data.frame(tau = as.numeric(t), estimate = results[[t]])
}))
setorder(results_ad2_df, tau)
results_ad2_df = results_ad2_df %>% mutate(period = "2014-2016")

# ST ----------------------------------------------------------------------

# 1. « First, for all dependent and independent variables in the model (w = y, x), 
# we partial out the group fixed ejects and obtain the centered-residualized variables: » ([Rios Avila et al., 2024, p. 12]
# In our case y = log_efficiency and x = log_tariff
tts_st[, log_efficiency := efficiency_module]
tts_st[, log_tariff := log(tariff)]
tts_st[, log_efficiency_demean := demean(log_efficiency ~ installer_name + origin +  county + year_quarter, data = tts_st)]
tts_st[, log_tariff_demean := demean(log_tariff ~  installer_name + origin + county + year_quarter, data = tts_st)]

# 2. « Afterward, we estimate the location model using the centered-residualized variables: » ([Rios Avila et al., 2024, p. 12]
loc_model = lm(log_efficiency_demean ~ log_tariff_demean -1 , data = tts_st)
tts_st[, resid_loc_model := resid(loc_model)]
beta_hat_loc_model = as.numeric(coef(loc_model)["log_tariff_demean"])

# 3.« Because |nuˆi| is the dependent variable for the scale model, 
# we apply the partialing out and recentering to this expression (|nuˆi|rc), 
# and use that to estimate the following model: » ([Rios Avila et al., 2024, p. 12]
tts_st[, abs_resid_demean := demean(abs(resid_loc_model) ~ installer_name + origin + county + year_quarter, data = tts_st)]
# We can estimate the scale model now
scale_model = lm(abs_resid_demean ~ log_tariff_demean , data = tts_st)
gamma_hat_scale_model = as.numeric(coef(scale_model)[["log_tariff_demean"]])

# 4. We recompose the standardized residuals and solve for each quantile
X = model.matrix(~ log_tariff_demean -1, data = tts_st)
resid_std <- (tts_st$log_efficiency_demean - X %*% beta_hat_loc_model) / as.numeric(X %*% gamma_hat_scale_model)

# 5. We recover each quantile
results = list()
taus <- c(0.10, 0.20, 0.30, 0.4, 0.50, 0.6, 0.7, 0.8, 0.9)
for (tau in taus){
  moment_function <- function(q) {
    mean(resid_std <= q , na.rm = TRUE) - tau
  }
  q_tau <- uniroot(moment_function, lower = -2000, upper = 2000)$root
  beta_tau = beta_hat_loc_model + q_tau * gamma_hat_scale_model
  results[[as.character(tau)]] = beta_tau
}

results_st_df = do.call(rbind, lapply(names(results), function(t) {
  data.frame(tau = as.numeric(t), estimate = results[[t]])
}))
setorder(results_st_df, tau)
results_st_df = results_st_df %>% mutate(period = "2017-2018")

# Plot --------------------------------------------------------------------

results_df = rbind(results_ad1_df, results_ad2_df, fill = TRUE)
results_df = rbind(results_df, results_st_df, fill = TRUE)
results_df = results_df %>% filter(period != TRUE)
results_df = results_df %>% mutate(lower = estimate - 1.96*se_robust,
                                   upper = estimate + 1.96*se_robust)

ggplot(results_df, aes(x = tau, y = estimate, color = period, fill = period)) +
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
    legend.position.inside = c(0.80, 0.80),  # new argument!
    legend.justification = c("left", "bottom"),
    legend.background = element_rect(fill = "white", color = "grey80"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# Bootstrap ---------------------------------------------------------------
library(data.table)
library(purrr)

# Store your original dataset
original_data <- copy(tts_ad2)

# Define function to compute quantile effects
estimate_qeffects <- function(data, taus = seq(0.1, 0.9, 0.1)) {
  data[, log_efficiency := efficiency_module]
  data[, log_tariff := log(tariff)]
  
  # 1. Partial out fixed effects
  data[, log_efficiency_demean := demean(log_efficiency ~ installer_name + origin + county + year_quarter, data = data)]
  data[, log_tariff_demean := demean(log_tariff ~ installer_name + origin + county + year_quarter, data = data)]
  
  # 2. Location model
  loc_model <- lm(log_efficiency_demean ~ log_tariff_demean - 1, data = data)
  beta_hat <- coef(loc_model)[["log_tariff_demean"]]
  data[, resid_loc := resid(loc_model)]
  
  # 3. Scale model
  data[, abs_resid_demean := demean(abs(resid_loc) ~ installer_name + origin + county + year_quarter, data = data)]
  scale_model <- lm(abs_resid_demean -1 ~ log_tariff_demean, data = data)
  gamma_hat <- coef(scale_model)[["log_tariff_demean"]]
  
  # 4. Standardized residuals
  X <- model.matrix(~ log_tariff_demean - 1, data = data)
  resid_std <- (data$log_efficiency_demean - X %*% beta_hat) / as.numeric(X %*% gamma_hat)
  
  # 5. Recover quantile estimates
  map_dfr(taus, function(tau) {
    moment_function <- function(q) mean(resid_std <= q, na.rm = TRUE) - tau
    q_tau <- uniroot(moment_function, lower = -20000, upper = 20000)$root
    beta_tau <- beta_hat + q_tau * gamma_hat
    data.frame(tau = tau, estimate = beta_tau)
  })
}

# Perform bootstrap
set.seed(42)
B <- 200  # Number of replications
taus <- seq(0.1, 0.9, 0.1)

bootstrap_results <- map_dfr(1:B, function(b) {
  sample_ids <- sample(1:nrow(original_data), replace = TRUE)
  data_b <- original_data[sample_ids]
  
  result <- tryCatch({
    df <- estimate_qeffects(copy(data_b), taus = taus)
    if (!is.null(df) && nrow(df) > 0) {
      df$bootstrap <- b
      return(df)
    } else {
      data.frame(tau = numeric(0), estimate = numeric(0), bootstrap = numeric(0))
    }
  }, error = function(e) {
    message(paste("Bootstrap iteration", b, "failed:", e$message))
    data.frame(tau = numeric(0), estimate = numeric(0), bootstrap = numeric(0))
  })
  
  return(result)
})

# Compute SEs from bootstraps
bootstrap_results = setDT(bootstrap_results)
se_df <- bootstrap_results[, .(se = sd(estimate)), by = tau]

# Combine with point estimates from original data
main_result <- estimate_qeffects(original_data, taus = taus)
main_result <- merge(main_result, se_df, by = "tau")
setDT(main_result)
main_result[, period := "2014–2016"]
main_result[, lower := estimate - 1.96*se][, upper := estimate + 1.96*se]

ggplot(main_result, aes(x = tau, y = estimate, color = period, fill = period)) +
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
    legend.position.inside = c(0.80, 0.80),  # new argument!
    legend.justification = c("left", "bottom"),
    legend.background = element_rect(fill = "white", color = "grey80"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )
