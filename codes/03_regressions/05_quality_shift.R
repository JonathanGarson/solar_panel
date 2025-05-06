# This code evaluates the change in variety and their quality entering the U.S. territory after tariff

library(arrow)
library(data.table)
library(fixest)
library(modelsummary)
library(tibble)
library(glue)
library(fplot)

# Data --------------------------------------------------------------------

tts = setDT(read_parquet(data_final("tts_final.parquet")))

# Quality Effect ----------------------------------------------------------

quality_brands = tts[, .(avg_efficiency = mean(efficiency_module, na.rm = TRUE)), by = .(year_quarter, module_manufacturer)]

# Merging Data ------------------------------------------------------------

tts[, tract := NULL]
tts = merge(tts, quality_brands, by = c("year_quarter", "module_manufacturer"))

# Variety Effect of Tariff ------------------------------------------------
# Quantity
quality_change = list(
  "Overall" = list(feols(log(avg_efficiency) ~ treated + log(tariff)*treated + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price 
        + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income
        | year_quarter + county + installer_name + origin,
        cluster = ~zip_code, data = tts)),
  "Anti-Dumping : 2010-2013" = list(feols(log(avg_efficiency) ~ log(tariff) + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price 
        + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income
        | year_quarter + county + installer_name + origin,
        cluster = ~zip_code, data = tts[year %in% 2010:2013])),
  "Anti-Dumping : 2014-2016" = list(feols(log(avg_efficiency) ~ log(tariff) + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price 
        + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income
        | year_quarter + county + installer_name + origin,
        cluster = ~zip_code, data = tts[year %in% 2014:2016])),
  "Trade War 2018" = list(feols(log(avg_efficiency) ~ log(tariff) + PV_system_size_DC + I(PV_system_size_DC^2) + elec_price 
        + mean_week_wage + population_density + pct_bachelor_estimate + median_home_value + median_household_income
        | year_quarter + county + installer_name + origin,
        cluster = ~zip_code, data = tts[year %in% 2017:2018]))
)

coef_name = c(
  "log(tariff)" = "ln Tariff"
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

model_lists = list(
  "quality_change" = quality_change
)

for (t in names(model_lists)){
  # t = "quality_change"
  table = modelsummary(
    models = model_lists[[t]],
    stars = TRUE,
    shape = "cbind",
    coef_map = coef_name,
    gof_omit = "Adj|AIC|BIC|Within|Pseudo|RMSE|Std.",
    gof_map = gof_list,
    # output = "latex"
  )
  writeLines(as.character(table), glue("output/regression/quality_shift/table_{t}.tex"))
}

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


# Alternative Strategy ----------------------------------------------------
# disper_efficiency_pre_ad2 = tts[year == 2013, (var(efficiency_module)/mean(efficiency_module)), by = .(tariff)]
# disper_efficiency_post_ad2 = tts[year == 2015, (var(efficiency_module)/mean(efficiency_module)), by = .(tariff)]
# 
# disper_efficiency_pre = tts[year == 2017, (var(efficiency_module)/mean(efficiency_module)), by = .(tariff)]
# disper_efficiency_post = tts[year == 2019, (var(efficiency_module)/mean(efficiency_module)), by = .(tariff)]

# Better formulation
dispersion_dt <- tts[year %in% 2010:2020, 
                     .(dispersion = var(efficiency_module, na.rm = TRUE) / mean(efficiency_module, na.rm = TRUE)),
                     by = .(tariff, year)
]

overdispersion = list(
  "Overall" = feols(dispersion ~ tariff | year, vcov = "hetero", data = dispersion_dt),
  "Anti-Dumping Tariff : 2010-2013" = feols(dispersion ~ tariff | year, vcov = "hetero", data = dispersion_dt[year %in% 2010:2013]),
  "Anti-Dumping Tariff : 2014-2016" = feols(dispersion ~ tariff | year, vcov = "hetero", data = dispersion_dt[year %in% 2014:2016]),
  "Trade War 2018" = feols(dispersion ~ tariff |year, vcov = "hetero", data = dispersion_dt[year %in% 2017:2018]))
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
  fmt = 4,
  add_rows = test,
  output = "latex"
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

