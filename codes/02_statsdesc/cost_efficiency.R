# This code provide cost efficiency measure of different system depending on the install system

library(arrow)
library(data.table)
library(ggplot2)

# Data --------------------------------------------------------------------

ts = read_parquet(data_temp("TTS_clean_names.parquet"))
top_manufacturer = read_parquet(data_temp('top_manufacturers.parquet'))


# Efficiency Cost by Brand ------------------------------------------------

chinese_mnf = c("suntech power","canadian solar","et solar industry","trina solar", "yingli energy (china)","renesola america")
korea_mnf = c("lg electronics inc.", "hanwha qcells (qidong)", "hanwha qcells", "hyundai energy solutions co., ltd.")
us_mnf = c("maxeon - sunpower", "tesla", "mission solar energy llc")
japan_mnf = top_manufacturer[Country == "Japan",]$Manufacturer

ts_eff_cost = ts[, .(mean_eff_cost = mean(cost_potential_prod_1, na.rm = T),
                     mean_eff_cost_theo = mean(price_w, na.rm = T)), 
                 by = .(module_manufacturer_1, year_quarter)]
ts_eff_cost[, `:=` (log_mean_eff_cost = log(mean_eff_cost),
                    log_mean_eff_cost_theo = log(mean_eff_cost_theo))]

ts_eff_cost[, year := as.numeric(substr(year_quarter, 1, 4))]
ts_eff_cost[, year_quarter := as.yearqtr(year_quarter)]

# Reshape data to long format for facetting
ts_eff_cost_long_log <- melt(ts_eff_cost, 
                         id.vars = c("module_manufacturer_1", "year_quarter", "year"), 
                         measure.vars = c("log_mean_eff_cost", "log_mean_eff_cost_theo"),
                         variable.name = "measure", 
                         value.name = "log_value")

# Relabel measures for clarity
ts_eff_cost_long_log[, measure := factor(measure, 
                                     levels = c("log_mean_eff_cost", "log_mean_eff_cost_theo"),
                                     labels = c("Effective Production Capacity", "Potential Electric Production Capacity"))]

# Define custom linetype mapping (dashed for potential, solid for effective)
ts_eff_cost_long_log[, linetype := "solid"]

# FOR NON LOG
# Reshape data to long format for facetting
ts_eff_cost_long <- melt(ts_eff_cost, 
                         id.vars = c("module_manufacturer_1", "year_quarter", "year"), 
                         measure.vars = c("mean_eff_cost", "mean_eff_cost_theo"),
                         variable.name = "measure", 
                         value.name = "value")

# Relabel measures for clarity
ts_eff_cost_long[, measure := factor(measure, 
                                     levels = c("mean_eff_cost", "mean_eff_cost_theo"),
                                     labels = c("Effective Production Capacity", "Potential Electric Production Capacity"))]

# Define custom linetype mapping (dashed for potential, solid for effective)
ts_eff_cost_long[, linetype := "solid"]

## Chinese Brands ----------------------------------------------------------
### Log ---------------------------------------------------------------------

# Plot with facets
ggplot(ts_eff_cost_long_log[module_manufacturer_1 %in% chinese_mnf & year %in% 2010:2023], 
       aes(x = year_quarter, y = log_value, group = module_manufacturer_1, 
           colour = module_manufacturer_1, linetype = linetype)) +
  geom_vline(xintercept = as.yearqtr("2012 Q4"), color = "black", linetype = "dotted") +  
  geom_vline(xintercept = as.yearqtr("2014 Q4"), color = "black", linetype = "dotted") +  
  geom_vline(xintercept = as.yearqtr("2018 Q1"), color = "black", linetype = "dotted") + 
  geom_line() +
  # Labels, Title & Caption
  labs(
    x = "Year",
    y = "Log Production Capacity (W/$)",
    title = "Comparison of Effective vs. Potential Electric Production Capacity for Chinese firms (2010-2023)",
    color = "Firms",
  ) +
  # Format x-axis for yearly intervals
  scale_x_yearqtr(format = "%Y", 
                  breaks = seq(from = min(ts_eff_cost$year_quarter), 
                               to = max(ts_eff_cost$year_quarter), by = 1)) +
  facet_wrap(~measure, scales = "free_y") +  # Separate facets for clarity
  scale_linetype_identity() +  # Ensure linetype mapping is applied
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    strip.text = element_text(size = 12, face = "bold") # Improve facet label visibility
  )
ggsave("output/figures/statdesc/log_cost_efficiency_chinese_firms.pdf", width = 11, height = 7)

### Non Log -----------------------------------------------------------------

# Plot with facets
ggplot(ts_eff_cost_long[module_manufacturer_1 %in% chinese_mnf & year %in% 2010:2023], 
       aes(x = year_quarter, y = value, group = module_manufacturer_1, 
           colour = module_manufacturer_1, linetype = linetype)) +
  geom_vline(xintercept = as.yearqtr("2012 Q4"), color = "black", linetype = "dotted") +  
  geom_vline(xintercept = as.yearqtr("2014 Q4"), color = "black", linetype = "dotted") +  
  geom_vline(xintercept = as.yearqtr("2018 Q1"), color = "black", linetype = "dotted") + 
  geom_line() +
  # Labels, Title & Caption
  labs(
    x = "Year",
    y = "Production Capacity (W/$)",
    title = "Comparison of Effective vs. Potential Electric Production Capacity for Chinese firms (2010-2023)",
    color = "Firms",
  ) +
  # Format x-axis for yearly intervals
  scale_x_yearqtr(format = "%Y", 
                  breaks = seq(from = min(ts_eff_cost$year_quarter), 
                               to = max(ts_eff_cost$year_quarter), by = 1)) +
  facet_wrap(~measure, scales = "free_y") +  # Separate facets for clarity
  scale_linetype_identity() +  # Ensure linetype mapping is applied
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    strip.text = element_text(size = 12, face = "bold") # Improve facet label visibility
  )
ggsave("output/figures/statdesc/cost_efficiency_chinese_firms.pdf", width = 12, height = 7)

## Korean Brands -----------------------------------------------------------

### Log ---------------------------------------------------------------------

# Plot with facets
ggplot(ts_eff_cost_long_log[module_manufacturer_1 %in% korea_mnf & year %in% 2012:2023], 
       aes(x = year_quarter, y = log_value, group = module_manufacturer_1, 
           colour = module_manufacturer_1, linetype = linetype)) +
  # geom_vline(xintercept = as.yearqtr("2012 Q4"), color = "black", linetype = "dotted") +  
  geom_vline(xintercept = as.yearqtr("2014 Q4"), color = "black", linetype = "dotted") +  
  geom_vline(xintercept = as.yearqtr("2018 Q1"), color = "black", linetype = "dotted") + 
  geom_line() +
  # Labels, Title & Caption
  labs(
    x = "Year",
    y = "Log Production Capacity (W/$)",
    title = "Comparison of Effective vs. Potential Electric Production Capacity for Korean firms (2012-2023)",
    color = "Firms",
  ) +
  # Format x-axis for yearly intervals
  scale_x_yearqtr(format = "%Y", 
                  breaks = seq(from = min(ts_eff_cost$year_quarter), 
                               to = max(ts_eff_cost$year_quarter), by = 1)) +
  facet_wrap(~measure, scales = "free_y") +  # Separate facets for clarity
  scale_linetype_identity() +  # Ensure linetype mapping is applied
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    strip.text = element_text(size = 12, face = "bold") # Improve facet label visibility
  )
ggsave("output/figures/statdesc/log_cost_efficiency_korean_firms.pdf", width = 11, height = 7)


## US ----------------------------------------------------------------------
# Plot with facets
ggplot(ts_eff_cost_long_log[module_manufacturer_1 %in% us_mnf & year %in% 2012:2023], 
       aes(x = year_quarter, y = log_value, group = module_manufacturer_1, 
           colour = module_manufacturer_1, linetype = linetype)) +
  # geom_vline(xintercept = as.yearqtr("2012 Q4"), color = "black", linetype = "dotted") +  
  geom_vline(xintercept = as.yearqtr("2014 Q4"), color = "black", linetype = "dotted") +  
  geom_vline(xintercept = as.yearqtr("2018 Q1"), color = "black", linetype = "dotted") + 
  geom_line() +
  # Labels, Title & Caption
  labs(
    x = "Year",
    y = "Log Production Capacity (W/$)",
    title = "Comparison of Effective vs. Potential Electric Production Capacity for Korean firms (2012-2023)",
    color = "Firms",
  ) +
  # Format x-axis for yearly intervals
  scale_x_yearqtr(format = "%Y", 
                  breaks = seq(from = min(ts_eff_cost$year_quarter), 
                               to = max(ts_eff_cost$year_quarter), by = 1)) +
  facet_wrap(~measure, scales = "free_y") +  # Separate facets for clarity
  scale_linetype_identity() +  # Ensure linetype mapping is applied
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    strip.text = element_text(size = 12, face = "bold") # Improve facet label visibility
  )
ggsave("output/figures/statdesc/log_cost_efficiency_us_firms.pdf", width = 11, height = 7)


# Country -----------------------------------------------------------------
ts_eff_cost[, country := fifelse(module_manufacturer_1 %in% chinese_mnf, "China",
                                 fifelse(module_manufacturer_1 %in% korea_mnf, "Korea",
                                         fifelse(module_manufacturer_1 %in% us_mnf, "United States",
                                                 fifelse(module_manufacturer_1 %in% japan_mnf, "Japan", "Other"))))]

# Aggregate at the country level
ts_eff_cost_country <- ts_eff_cost[, .(
  mean_eff_cost = mean(mean_eff_cost, na.rm = TRUE),
  mean_eff_cost_theo = mean(mean_eff_cost_theo, na.rm = TRUE),
  log_mean_eff_cost = mean(log_mean_eff_cost, na.rm = TRUE),
  log_mean_eff_cost_theo = mean(log_mean_eff_cost_theo, na.rm = TRUE)
), by = .(country, year_quarter, year)]

# Reshape to long format for plotting (Log version)
ts_eff_cost_country_long_log <- melt(ts_eff_cost_country, 
                                     id.vars = c("country", "year_quarter", "year"), 
                                     measure.vars = c("log_mean_eff_cost", "log_mean_eff_cost_theo"),
                                     variable.name = "measure", 
                                     value.name = "log_value")

# Relabel measure names for clarity
ts_eff_cost_country_long_log[, measure := factor(measure, 
                                                 levels = c("log_mean_eff_cost", "log_mean_eff_cost_theo"),
                                                 labels = c("Effective Production Capacity", "Potential Electric Production Capacity"))]

# Plot aggregated country-level trends
ggplot(ts_eff_cost_country_long_log[country %in% c("China", "Korea", "United States") & year %in% 2012:2023], 
       aes(x = year_quarter, y = log_value, group = country, colour = country)) +
  geom_vline(xintercept = as.yearqtr("2012 Q4"), color = "black", linetype = "dotted") +  
  geom_vline(xintercept = as.yearqtr("2014 Q4"), color = "black", linetype = "dotted") +  
  geom_vline(xintercept = as.yearqtr("2018 Q1"), color = "black", linetype = "dotted") + 
  geom_line(linewidth = 1.2) +
  # Labels, Title & Caption
  labs(
    x = "Year",
    y = "Log Production Capacity (W/$)",
    title = "Comparison of Effective vs. Potential Electric Production Capacity by Country (2012-2023)",
    color = "Country",
    caption = "Notes: Potential = total_install_price / (system_size * system_efficiency). Effective is efficiency-adjusted."
  ) +
  # Format x-axis for yearly intervals
  scale_x_yearqtr(format = "%Y", 
                  breaks = seq(from = min(ts_eff_cost$year_quarter), 
                               to = max(ts_eff_cost$year_quarter), by = 1)) +
  facet_wrap(~measure, scales = "free_y") +  # Separate facets for clarity
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    strip.text = element_text(size = 12, face = "bold"),
    text = element_text(size = 14)
  )
ggsave("output/figures/statdesc/log_cost_efficiency_country.pdf", width = 11, height = 7)



