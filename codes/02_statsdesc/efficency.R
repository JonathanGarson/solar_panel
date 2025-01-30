# This code generates figures by country and by brands of efficiency of installed solar panels.

rm(ts, ts_brands, ts_plot, manufacturer_country)
gc()

library(arrow)
library(data.table)
library(ggplot2)
library(gt)
library(glue)
library(scales)

# Set theme ggplot --------------------------------------------------------

theme_set(theme_minimal() +
            theme(
              plot.title = element_text(hjust = 0.5),
              legend.position = "top"
            ))

# Data --------------------------------------------------------------------

top_manufacturer = read_parquet(data_temp("top_manufacturers.parquet"))
ts_brands = data.table(read_parquet(data_final("TTS_brands.parquet")))
ts_plot = data.table(read_parquet(data_temp("TTS_clean_names.parquet")))

# Brand Origin per country --------------------------------------------

chinese_brand = top_manufacturer[Country == "China",]$Manufacturer
korean_brand = top_manufacturer[Country == "South Korea",]$Manufacturer
japanese_brand = top_manufacturer[Country == "Japan",]$Manufacturer
german_brand = top_manufacturer[Country == "Germany",]$Manufacturer
# US_brand = c(top_manufacturer[Country == "United States",]$Manufacturer, "silfab", "maxeon - sunpower") 
US_brand = setdiff(c(top_manufacturer[Country == "United States",]$Manufacturer, "silfab", "maxeon - sunpower"), "tesla") # We exclude Tesla since it does not produce anything
norway_brand = "rec solar"

# Efficiency between Brand Origin ----------------------------------------------

ts_brands[, Country := fifelse(manufacturer %in% chinese_brand, "China",
                               fifelse(manufacturer %in% korean_brand, "South Korea",
                                       fifelse(manufacturer %in% japanese_brand, "Japan",
                                               fifelse(manufacturer %in% german_brand, "Germany",
                                                       fifelse(manufacturer %in% US_brand, "United States",
                                                               fifelse(manufacturer %in% norway_brand, "Norway", NA_character_))))))]

# Aggregate by Country and year to calculate average efficiency
efficiency_by_country <- ts_brands[, .(
  avg_efficiency = mean(avg_eff, na.rm = TRUE)  # Average efficiency per year per country
), by = .(Country, year)]

# Filter out countries with missing values
efficiency_by_country <- efficiency_by_country[!is.na(Country)]

# Plot the trajectory of solar panel efficiency by country
ggplot(efficiency_by_country[year %in% 2010:2022], aes(x = year, y = avg_efficiency, color = Country)) +
  geom_line(size = 1) +
  labs(
    title = "Trajectory of Solar Panel Efficiency between Brand Origin Country",
    x = "Year",
    y = "Average Efficiency (%)",
    color = "Country"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_color_brewer(palette = "Set1")
ggsave("output/figures/statdesc/effiency_by_country.pdf")

# Efficiency within Brand Origin ----------------------------------------------

# Create a list of brands by country
brands_by_country <- list(
  "China" = chinese_brand,
  "South Korea" = korean_brand,
  "Japan" = japanese_brand,
  "Germany" = german_brand,
  "United States" = US_brand,
  "Norway" = norway_brand
)

# Loop over each country and its brands
for (country in names(brands_by_country)) {
  country_brands <- brands_by_country[[country]]  # Get the brands for the country
  
  # Filter the data for the country's brands
  country_data <- ts_brands[manufacturer %in% country_brands]
  
  p <- ggplot(country_data, aes(x = year, y = avg_eff, color = manufacturer)) +
    geom_line(size = 1) +
    labs(
      title = glue("Solar Panel Efficiency Trajectory - {country}"),
      x = "Year",
      y = "Average Efficiency (%)",
      color = "Manufacturer"
    ) +
    geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
    geom_vline(xintercept = 2015, linetype = "dashed", color = "black") +
    geom_vline(xintercept = 2018, linetype = "dashed", color = "black") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom"
    )
  
  ggsave(
    filename = glue("output/figures/statdesc/efficiency_firms_{country}.pdf"),
    plot = p,
    width = 10,
    height = 6
  )
}

# Efficiency by year of installation ----------------------------------------

ts_eff = ts_plot[, .(avg_eff_install = mean(efficiency_module_1)), by = year]
ts_eff[, log_avg_eff_install := log(avg_eff_install*100)]

ggplot(ts_eff, aes(x = year, y = log_avg_eff_install)) +
  geom_line(color = "red")+
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black")+
  geom_vline(xintercept = 2015, linetype = "dashed", color = "black")+
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black")+
  labs(
    x = "Year",
    y = "Average Efficiency (log)",
    title = "Average Efficiency of the Installed Solar Panel"
  )
ggsave("output/figures/statdesc/avg_log_efficiency_install_year.pdf")

ggplot(ts_eff, aes(x = year, y = avg_eff_install)) +
  geom_line(color = "red") + 
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black")+
  geom_vline(xintercept = 2015, linetype = "dashed", color = "black")+
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black")+
  labs(
    x = "Year",
    y = "Average Efficiency",
    title = "Average Efficiency of the Installed Solar Panel"
  )
ggsave("output/figures/statdesc/avg_efficiency_install_year.pdf")

# Most sold panels by brand -----------------------------------------------

csi_models_2014 = unique(ts_plot[year == 2014  & module_manufacturer_1 == "canadian solar", .(module_model_1, efficiency_module_1)])
csi_models_2015 = unique(ts_plot[year == 2015  & module_manufacturer_1 == "canadian solar", .(module_model_1, efficiency_module_1)])
csi_models_2016 = unique(ts_plot[year == 2016  & module_manufacturer_1 == "canadian solar", .(module_model_1, efficiency_module_1)])

ts_plot[, sales_model_year := sum(module_quantity_1), by = .(year, module_model_1, module_manufacturer_1)]
ts_plot[, sales_year_brand := sum(module_quantity_1), by = .(year, module_manufacturer_1)]
ts_plot[, share_sales_model_year := sales_model_year/sales_year_brand, by = .(year, module_manufacturer_1)]
ts_model = unique(ts_plot[, .(module_manufacturer_1, year, share_sales_model_year,sales_model_year, efficiency_module_1, module_model_1)])

ggplot(ts_model[year %in% 2010:2016 & module_manufacturer_1 == "canadian solar" ], 
       aes(x = year, y = share_sales_model_year, group = module_model_1, color = module_model_1, linetype = module_model_1)) +
  geom_line() +
  labs(
    x = "Year",
    y = "Market share (%)", 
    title = "Market share of Chinese and Korean brands"
  ) +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 2014, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black")
ggsave("output/figures/statdesc/market_share_canadian_solar_panels.pdf")


trina_models_2014 = unique(ts_plot[year == 2014  & module_manufacturer_1 == "trina solar", .(module_model_1, efficiency_module_1)])
trina_models_2015 = unique(ts_plot[year == 2015  & module_manufacturer_1 == "trina solar", .(module_model_1, efficiency_module_1)])
trina_models_2016 = unique(ts_plot[year == 2016  & module_manufacturer_1 == "trina solar", .(module_model_1, efficiency_module_1)])
