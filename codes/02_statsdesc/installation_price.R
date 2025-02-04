# This code recover the variation in installation price following the tariff implementation

library(arrow)
library(data.table)
library(ggplot2)

# Data --------------------------------------------------------------------

ts_price = as.data.table(read_parquet(data_temp("TTS_clean_names.parquet")))
top_manufacturer = read_parquet(data_temp('top_manufacturers.parquet'))

# chinese_mnf = top_manufacturer[Country == "China",]$Manufacturer
chinese_mnf = c("suntech power","canadian solar","et solar industry","trina solar", "yingli energy (china)","renesola america")
# korea_mnf = top_manufacturer[Country == "South Korea", ]$Manufacturer
korea_mnf = c("lg electronics inc.", "hanwha qcells (qidong)", "hanwha qcells", "hyundai energy solutions co., ltd.")
us_mnf = c(top_manufacturer[Country == "United States",]$Manufacturer, "SunPower - Maxeon")
japan_mnf = top_manufacturer[Country == "Japan",]$Manufacturer

# Overall Price evolution ---------------------------------------------------------------
ts_price[, price_installed := total_installed_price - rebate_or_grant]
ts_price_quarter = ts_price[year %in% 2010:2023, .(mean_install_gross_price_quarter = mean(total_installed_price, na.rm = T),
                                                   mean_install_net_price_quarter = mean(price_installed, na.rm = T)), by = year_quarter]
ts_price_quarter[, `:=`(log_mean_install_gross_price_quarter = log(mean_install_gross_price_quarter),
                        log_mean_install_net_price_quarter = log(mean_install_net_price_quarter))]
ts_price_quarter[, year := as.numeric(substr(year_quarter,1,4))]
ts_price_quarter[, year_quarter := as.yearqtr(year_quarter)]

# Non Log
ggplot(ts_price_quarter, aes(x = year_quarter)) +
  # First line: Net Price (Red, Dashed)
  geom_line(aes(y = mean_install_gross_price_quarter, color = "Gross Price"), linetype = "dashed", size = 1) +
  # Second line: Gross Price (Blue, Solid)
  geom_line(aes(y = mean_install_net_price_quarter, color = "Net Price"), linetype = "solid", size = 1) +
  # Vertical reference lines
  geom_vline(xintercept = as.yearqtr("2012 Q4"), color = "black", linetype = "dotted") +  
  geom_vline(xintercept = as.yearqtr("2014 Q4"), color = "black", linetype = "dotted") +  
  geom_vline(xintercept = as.yearqtr("2018 Q1"), color = "black", linetype = "dotted") +  
  # Labels & Title
  labs(
    x = "Year",
    y = "Total Price ($)",
    title = "Quarterly Evolution of Total Installation Prices in log $",
    color = "Legend"
  ) +
  # Format x-axis to show only yearly labels while keeping quarterly data
  scale_x_yearqtr(format = "%Y", 
                  breaks = seq(from = min(ts_price_quarter$year_quarter), 
                               to = max(ts_price_quarter$year_quarter), by = 1)) +
  # Use the same y-axis for both lines
  scale_y_continuous() +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )
ggsave("output/figures/statdesc/installation_price_evolution.pdf")

# Log
ggplot(ts_price_quarter, aes(x = year_quarter)) +
  # First line: Net Price (Red, Dashed)
  geom_line(aes(y = log_mean_install_gross_price_quarter, color = "Net Price"), linetype = "dashed", size = 1) +
  # Second line: Gross Price (Blue, Solid)
  geom_line(aes(y = mean_install_net_price_quarter, color = "Gross Price"), linetype = "solid", size = 1) +
  # Vertical reference lines
  geom_vline(xintercept = as.yearqtr("2012 Q4"), color = "black", linetype = "dotted") +  
  geom_vline(xintercept = as.yearqtr("2014 Q4"), color = "black", linetype = "dotted") +  
  geom_vline(xintercept = as.yearqtr("2018 Q1"), color = "black", linetype = "dotted") +  
  # Labels & Title
  labs(
    x = "Year",
    y = "Total Price (log $)",
    title = "Quarterly Evolution of Total Installation Prices in log $",
    color = "Legend"
  ) +
  # Format x-axis to show only yearly labels while keeping quarterly data
  scale_x_yearqtr(format = "%Y", 
                  breaks = seq(from = min(ts_price_quarter$year_quarter), 
                               to = max(ts_price_quarter$year_quarter), by = 1)) +
  # Use the same y-axis for both lines
  scale_y_continuous() +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )
ggsave("output/figures/statdesc/installation_price_evolution_log.pdf")

# Price evolution by company and origin ---------------------------------------
## Brands ------------------------------------------------------------------
ts_price_quarter_mnf = ts_price[year %in% 2010:2023, .(mean_install_gross_price_quarter = mean(total_installed_price, na.rm = T),
                                                       mean_install_net_price_quarter = mean(price_installed, na.rm = T)), 
                                by = .(year_quarter, module_manufacturer_1)]
ts_price_quarter_mnf[, `:=`(log_mean_install_gross_price_quarter = log(mean_install_gross_price_quarter),
                        log_mean_install_net_price_quarter = log(mean_install_net_price_quarter))]

ts_price_quarter_mnf[, year := as.numeric(substr(year_quarter,1,4))]
ts_price_quarter_mnf[, year_quarter := as.yearqtr(year_quarter)]

brands = list("china" = chinese_mnf, "korea" = korea_mnf, "usa" = us_mnf, "japan" = japan_mnf)

# Non Log
for (country in names(brands)) {
  # Filter data for the current country's manufacturers
  ts_filtered <- ts_price_quarter_mnf[module_manufacturer_1 %in% brands[[country]]]

  # Generate the plot
  p <- ggplot(ts_filtered, aes(x = year_quarter, y = mean_install_gross_price_quarter, color = module_manufacturer_1)) +
    geom_line(size = 1) +
    labs(
      x = "Year",
      y = "Mean Installation Price ($)",
      title = paste("Quarterly Evolution of Mean Installation Price -", country, "Brands"),
      color = "Brand"
    ) +
    scale_x_yearqtr(format = "%Y",
                    breaks = seq(from = min(ts_price_quarter_mnf$year_quarter),
                                 to = max(ts_price_quarter_mnf$year_quarter), by = 1)) +
    # Vertical reference lines
    geom_vline(xintercept = as.yearqtr("2012 Q4"), color = "black", linetype = "dotted") +
    geom_vline(xintercept = as.yearqtr("2014 Q4"), color = "black", linetype = "dotted") +
    geom_vline(xintercept = as.yearqtr("2018 Q1"), color = "black", linetype = "dotted") +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Save the plot
  ggsave(filename = paste0("output/figures/statdesc/installation_price_evolution_", country, "_brands.pdf"),
         plot = p, width = 10, height = 6)

  print(paste("Saved plot for:", country))  # Print status message
}

# Log
for (country in names(brands)) {
  # Filter data for the current country's manufacturers
  ts_filtered <- ts_price_quarter_mnf[module_manufacturer_1 %in% brands[[country]]]

  # Generate the plot
  p <- ggplot(ts_filtered, aes(x = year_quarter, y = log_mean_install_gross_price_quarter, color = module_manufacturer_1)) +
    geom_line(size = 1) +
    labs(
      x = "Year",
      y = "Mean Installation Price ($)",
      title = paste("Quarterly Evolution of Mean Installation Price -", country, "Brands"),
      color = "Brand"
    ) +
    scale_x_yearqtr(format = "%Y",
                    breaks = seq(from = min(ts_price_quarter_mnf$year_quarter),
                                 to = max(ts_price_quarter_mnf$year_quarter), by = 1)) +
    # Vertical reference lines
    geom_vline(xintercept = as.yearqtr("2012 Q4"), color = "black", linetype = "dotted") +
    geom_vline(xintercept = as.yearqtr("2014 Q4"), color = "black", linetype = "dotted") +
    geom_vline(xintercept = as.yearqtr("2018 Q1"), color = "black", linetype = "dotted") +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Save the plot
  ggsave(filename = paste0("output/figures/statdesc/installation_log_price_evolution_", country, "_brands.pdf"),
         plot = p, width = 10, height = 6)

  print(paste("Saved plot for:", country))  # Print status message
}

## Country -----------------------------------------------------------------

ts_price = merge(ts_price, top_manufacturer, by.x = "module_manufacturer_1", by.y = "Manufacturer")
ts_price[, price_installed := total_installed_price - rebate_or_grant]
ts_price_quarter_mnf= ts_price[year %in% 2010:2023, 
                               .(mean_install_gross_price_quarter = mean(total_installed_price, na.rm = T),
                                 mean_install_net_price_quarter = mean(price_installed, na.rm = T)), 
                               by = .(year_quarter, Country)]
ts_price_quarter_mnf[, `:=` (log_mean_install_gross_price_quarter = log(mean_install_gross_price_quarter),
                             log_mean_install_net_price_quarter = log(mean_install_net_price_quarter))]

ts_price_quarter_mnf[, year := as.numeric(substr(year_quarter,1,4))]
ts_price_quarter_mnf[, year_quarter := as.yearqtr(year_quarter)]

country = c("China", "Germany", "Norway", "Japan", "South Korea", "United States")

ggplot(ts_price_quarter_mnf[Country %in% country & year %in% 2013:2020], aes(x = year_quarter, y = log_mean_install_gross_price_quarter, color = Country)) +
  geom_line(size = 1) +
  labs(
    x = "Year",
    y = "Mean Installation Price (log $)",
    title = paste("Quarterly Evolution of Log Mean Installation Price by Country"),
    color = "Country"
  ) +
  scale_x_yearqtr(format = "%Y",
                  breaks = seq(from = min(ts_price_quarter_mnf$year_quarter),
                               to = max(ts_price_quarter_mnf$year_quarter), by = 1)) +
  # Vertical reference lines
  # geom_vline(xintercept = as.yearqtr("2012 Q4"), color = "black", linetype = "dotted") +
  geom_vline(xintercept = as.yearqtr("2014 Q4"), color = "black", linetype = "dotted") +
  geom_vline(xintercept = as.yearqtr("2018 Q1"), color = "black", linetype = "dotted") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("output/figures/statdesc/quarterly_log_install_price_country_2013_2020.pdf")

ggplot(ts_price_quarter_mnf[Country %in% country & year %in% 2010:2023], aes(x = year_quarter, y = log_mean_install_gross_price_quarter, color = Country)) +
  geom_line(size = 1) +
  labs(
    x = "Year",
    y = "Mean Installation Price (log $)",
    title = paste("Quarterly Evolution of Log Mean Installation Price by Country"),
    color = "Country"
  ) +
  scale_x_yearqtr(format = "%Y",
                  breaks = seq(from = min(ts_price_quarter_mnf$year_quarter),
                               to = max(ts_price_quarter_mnf$year_quarter), by = 1)) +
  # Vertical reference lines
  # geom_vline(xintercept = as.yearqtr("2012 Q4"), color = "black", linetype = "dotted") +
  geom_vline(xintercept = as.yearqtr("2014 Q4"), color = "black", linetype = "dotted") +
  geom_vline(xintercept = as.yearqtr("2018 Q1"), color = "black", linetype = "dotted") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("output/figures/statdesc/quarterly_log_install_price_country_2010_2023.pdf")

