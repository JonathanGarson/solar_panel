# In this code we create the final TTS dataset for our regression:
# - We provide a clear procedure to identify and figure to justify the firm that we keep for our analysis
# - We build the key quality and adoption variable

library(arrow)
library(data.table)
library(ggplot2)
library(glue)
library(gt)

# Data --------------------------------------------------------------------

tts = setDT(read_parquet(data_temp("TTS_merged.parquet")))
ad_2012 = fread(data_final("ad_2012_final.csv"))
ad_2015 = fread(data_final("ad_2015_final.csv"))

# Selecting firms ---------------------------------------------------------

# Market share of installation by brand approach : we select the 15 biggest firms on the period, their market share is above 3%.

# Displaying the number of installation per model per year
tts[, sales_per_model := sum(module_quantity, na.rm = T) , by = c("year","module_model")]
for (y in c(2011, 2013, 2017, 2019)){
  sales_dt <- unique(tts[year == `y`, .(module_manufacturer,module_model, sales_per_model)])
  sales_dt[, sum_year := sum(sales_per_model)]
  sales_dt[, pct_sales := sales_per_model/sum_year]
  setorder(sales_dt, cols = -pct_sales)
  sales_dt[, cum_sum_pct_sales := cumsum(pct_sales)]
  
  # Compute summary statistics
  n_models    <- nrow(sales_dt)
  mean_sales  <- mean(sales_dt$sales_per_model, na.rm = TRUE)
  median_sales<- median(sales_dt$sales_per_model, na.rm = TRUE)
  max_sales   <- max(sales_dt$sales_per_model, na.rm = TRUE)
  min_sales   <- min(sales_dt$sales_per_model, na.rm = TRUE)
  
  # Create a label for the annotation
  stats_label <- paste0("Models: ", n_models, "\n",
                        "Mean: ", round(mean_sales, 2), "\n",
                        "Median: ", round(median_sales, 2), "\n",
                        "Max: ", max_sales, "\n",
                        "Min: ", min_sales)
 
  # Plot histogram of sales per model for 2010
  ggplot(sales_dt, aes(x = sales_per_model)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black") +
    labs(
      x = "Sales per Model",
      y = "Frequency"
    ) +
    theme_light() +
    annotate("text", x = Inf, y = Inf, label = stats_label, 
             hjust = 1.1, vjust = 1.1, size = 5)
  ggsave(glue("output/figures/statdesc/sales_distribution_{y}.pdf"), width = 10, height = 8)
}

# We select the firm that represented 90% of the market share between 2010 and 2020
tts[, sales_per_brand := sum(module_quantity, na.rm = T) , by = c("module_manufacturer")]
tts[, sales_overall := sum(module_quantity)]
tts[, market_share_period := round(sales_per_brand/sales_overall, 4)]
market_share = unique(tts[, .(module_manufacturer, market_share_period)])
setorder(market_share, -market_share_period)
market_share[, cum_sum_share := cumsum(market_share_period)]
export_dt = market_share[cum_sum_share <= 0.9, .(module_manufacturer, market_share_period)]
list_country = c("USA", "South Korea", "South Korea", "China", "Norway", "Germany", "China", "China", "Japan", "China", "Japan", "South Korea", "China", "USA", "Japan")
export_dt = cbind(export_dt, list_country)

table_final_brands = gt(export_dt) %>% 
  cols_label(
    module_manufacturer = "Manufacturer",
    market_share_period = "Market Share",
    list_country = "Country of Origin",
  ) %>%
  as_latex() %>% 
  as.character()
writeLines(table_final_brands,"output/tables/firms_list/top_firms_us_market.tex")

top_firms = export_dt[, .(module_manufacturer, list_country)]
tts = merge(tts, top_firms, by = "module_manufacturer")
# Note that the merge above does not have the all.x = TRUE argument which implies that non-matched observations are dropped. 

tts[, china := ifelse(list_country == "China", 1, 0)]
tts[, korea := ifelse(list_country == "South Korea", 1, 0)]
tts[, usa := ifelse(list_country == "USA", 1, 0)]
tts[, norway := ifelse(list_country == "Norway", 1, 0)]
tts[, germany := ifelse(list_country == "Germany", 1, 0)]
tts[, japan := ifelse(list_country == "Japan", 1, 0)]

# Setting different quality criteria --------------------------------------
# models_dt = unique(tts[, .(module_model, efficiency_module, year)])
models_dt = unique(tts[, .(module_model, efficiency_module, year, module_manufacturer)])
# Initialize an empty data frame with appropriate column types
pct_eff_dt <- data.frame(year = numeric(), 
                         p50 = numeric(), 
                         p75 = numeric(), 
                         p90 = numeric(), 
                         p95 = numeric())

# Loop over the years of interest
for (y in c(2011, 2013, 2017, 2019)) {
  # Calculate the quantiles for the given year; include na.rm = TRUE to handle missing values.
  pct_eff <- quantile(models_dt[year == y]$efficiency, 
                      probs = c(0.5, 0.75, 0.9, 0.95), 
                      na.rm = TRUE)
  
  # Append the calculated values to the data frame
  pct_eff_dt <- rbind(pct_eff_dt,
                      data.frame(year = y,
                                 p50 = round(as.numeric(pct_eff[1]), 2),
                                 p75 = round(as.numeric(pct_eff[2]), 2),
                                 p90 = round(as.numeric(pct_eff[3]), 2),
                                 p95 = round(as.numeric(pct_eff[4]), 2)))
  
  # Prepare label for the plot annotation
  pct_eff_labs <- paste0(
    "p50: ", round(pct_eff[1], 2), "\n",
    "p75: ", round(pct_eff[2], 2), "\n",
    "p90: ", round(pct_eff[3], 2), "\n",
    "p95: ", round(pct_eff[4], 2)
  )
  
  # Create and save the histogram for the current year
  p <- ggplot(models_dt[year == y], aes(x = efficiency_module)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black") +
    labs(
      x = "Efficiency per model",
      y = "Frequency"
    ) +
    theme_light() +
    annotate("text", x = Inf, y = Inf, label = pct_eff_labs, 
             hjust = 1.1, vjust = 1.1, size = 5)
  
  ggsave(glue("output/figures/statdesc/efficiency_distrib_{y}.pdf"), 
         plot = p, width = 10, height = 8)
}


## Effiency Premium ---------------------------------------------------------
setDT(pct_eff_dt)
# Overall
tts[, premium_panel_overall := ifelse(efficiency_module >= 0.20, 1, 0) ]

# # Relative Premium
# thresholds <- tts[, .(efficiency_threshold = quantile(efficiency_module, 0.90, na.rm = TRUE)), by = year]
# tts <- merge(tts, thresholds, by = "year", all.x = TRUE)
# tts[, premium_panel_relative := as.integer(efficiency_module >= efficiency_threshold), by = year]

# AD 1 : 2010-2013
for (y in c(2010:2013)) {
  tts[year == `y`, premium_panel_ad1 := ifelse(efficiency_module > pct_eff_dt[year == 2011,]$p90, 1, 0) ]
}

# AD 2 : 2013-2016
for (y in c(2013:2016)) {
  tts[year == `y`, premium_panel_ad2 := ifelse(efficiency_module > pct_eff_dt[year == 2013,]$p90, 1, 0) ]
}

# Safeguard : 2017-2020
for (y in c(2017:2020)) {
  tts[year == `y`, premium_panel_st := ifelse(efficiency_module > pct_eff_dt[year == 2017,]$p90, 1, 0) ]
}

list_firms = top_firms$module_manufacturer
tts = tts[module_manufacturer %in% list_firms,]

## Relative Premium --------------------------------------------------------

# ggplot(tts[year %in% 2010:2012,], aes(x = reorder(module_manufacturer, efficiency_module, FUN = mean), 
#                 y = efficiency_module)) +
#   geom_boxplot(fill = "steelblue", color = "black") +
#   labs(
#     x = "Manufacturer",
#     y = "Efficiency Module (%)"
#   ) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggsave("output/figures/firms_list/distrib_efficiency_2010_2012.pdf", width = 10, height = 7)
# 
# ggplot(tts[year %in% 2013:2016,], aes(x = reorder(module_manufacturer, efficiency_module, FUN = mean), 
#                                       y = efficiency_module)) +
#   geom_boxplot(fill = "steelblue", color = "black") +
#   labs(
#     x = "Manufacturer",
#     y = "Efficiency Module (%)"
#   ) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggsave("output/figures/firms_list/distrib_efficiency_2013_2016.pdf", width = 10, height = 7)
# 
# ggplot(tts[year %in% 2017:2020,], aes(x = reorder(module_manufacturer, efficiency_module, FUN = mean), 
#                                       y = efficiency_module)) +
#   geom_boxplot(fill = "steelblue", color = "black") +
#   labs(
#     x = "Manufacturer",
#     y = "Efficiency Module (%)"
#   ) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggsave("output/figures/firms_list/distrib_efficiency_2017_2020.pdf", width = 10, height = 7)

## Combo inverter + high efficiency ----------------------------------------
# Mono cristalyne are categorized as top quality solar panel, more innovative and more efficient
# The presence of micro inverter improve the overall efficiency of the system and makes it more desirable

tts[, premium_installation := ifelse(technology_module == "Mono-c-Si" & micro_inverter_1 == "Y", 1, 0)]
# tts[, premium_installation := ifelse(technology_module == "Mono-c-Si" & (micro_inverter_1 == "Y"|built_in_meter_inverter_1 == "Y"), 1, 0)]
# tts[year %in% 2010:2012, quality_2_ad1 := ifelse(technology_module == "Mono-c-Si" & (micro_inverter_1 == "Y"|built_in_meter_inverter_1 == "Y"), 1, 0)]
# tts[year %in% 2013:2015, quality_2_ad2 := ifelse(technology_module == "Mono-c-Si" & (micro_inverter_1 == "Y"|built_in_meter_inverter_1 == "Y"), 1, 0)]
# tts[year %in% 2016:2020, quality_2_st := ifelse(technology_module == "Mono-c-Si" & (micro_inverter_1 == "Y"|built_in_meter_inverter_1 == "Y"), 1, 0)]

# Grouping Small Observation ----------------------------------------------
install = unique(tts[, .(installer_count = .N), by = installer_name])
install[, sum_installer_count := sum(installer_count)]

setorder(install, installer_count)
install[, cum_N := cumsum(installer_count)]
install[, cum_pct := cum_N / sum_installer_count]

# Identify the installers whose cumulative percentage is <= 5%
list_small_installer = install[cum_pct <= 0.05, installer_name]
tts[installer_name %in% list_small_installer, installer_name := "other"]

# Setting Price and Demand Variables --------------------------------------
setnames(ad_2012, "module_manufacturer_2012", "module_manufacturer")
setnames(ad_2015, "module_manufacturer_2015", "module_manufacturer")

tts = merge(tts, ad_2012, by = c("module_manufacturer"), all.x = TRUE)
tts = merge(tts, ad_2015, by = c("module_manufacturer"), all.x = TRUE)

tts[, tariff_2012 := ad_rate_2012 + cvd_rate_2012]
tts[, tariff_2015 := ad_rate_2015]
tts[, tariff_2015_temp := ad_rate_2015 + cvd_rate_2015]

# Exploiting variation in tariff implementation
tts[year %in% 2010:2013, tariff_2012_treated := ifelse(china == 1 & year_quarter.x > "2012Q2", 1, 0)]
tts[year %in% 2013:2016, tariff_2015_treated := ifelse(china == 1 & year_quarter.x > "2014Q3", 1, 0)]

# We keep zip code with population different from 0 since it would imply that zip code correspond to a commercial area
tts = tts[population > 0,]
tts[, installation_zip_code := .N, by = .(zip_code, year)]
tts[, demand_zip_code := (installation_zip_code/population)*1000]

# Cleaning before export --------------------------------------------------
tts[, year_quarter := NULL]
tts[, year_quarter.y := NULL]
tts[, list_country := NULL]
tts[, installation_zip_code := NULL]
tts[, sales_per_model := NULL]
tts[, sales_per_brand := NULL]
tts[, sales_overall := NULL]
tts[, nb_manufacturer := NULL]
setnames(tts, "year_quarter.x", "year_quarter")

# We only keep HO data
tts = tts[ho == 1,]

# Export Data -------------------------------------------------------------
write_parquet(tts, data_final("tts_final.parquet"))
