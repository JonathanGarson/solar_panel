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

## Overall Premium ---------------------------------------------------------
setDT(pct_eff_dt)
# AD 1 : 2010-2013
for (y in c(2010:2013)) {
  tts[year == `y`, quality_1_ad1 := ifelse(efficiency_module > pct_eff_dt[year == 2011,]$p90, 1, 0) ]
}

# AD 2 : 2013-2016
for (y in c(2013:2016)) {
  tts[year == `y`, quality_1_ad2 := ifelse(efficiency_module > pct_eff_dt[year == 2013,]$p90, 1, 0) ]
}

# Safeguard : 2017-2020
for (y in c(2017:2020)) {
  tts[year == `y`, quality_1_st := ifelse(efficiency_module > pct_eff_dt[year == 2017,]$p90, 1, 0) ]
}

# Cleaning mistakes
tts[module_model == "cs1h-325ms", module_manufacturer := "canadian solar"]
tts[module_model == "spr-225-blk-u", module_manufacturer := "maxeon - sunpower"]
tts[module_model == "spr-e19-320", module_manufacturer := "maxeon - sunpower"]
tts[module_model == "spr-e19-320", module_manufacturer := "maxeon - sunpower"]
tts[module_model == "spr-e19-320", module_manufacturer := "maxeon - sunpower"]
tts[module_model == "spr-x21-345-d-ac", module_manufacturer == "maxeon - sunpower"]
tts[module_model == "spr-a400", module_manufacturer == "maxeon - sunpower"]
tts[module_model == "spr-a400-g-ac", module_manufacturer == "maxeon - sunpower"]
tts[module_model == "d6m310h3a", module_manufacturer := "neo solar power"]
tts[module_model == "lr6-60hpb-310m", module_manufacturer := "longi green energy technology co., ltd."]
tts[module_model == "lr6-60hpb-315m", module_manufacturer := "longi green energy technology co., ltd."]
tts[module_model == "lg305n1c-b3", module_manufacturer := "lg electronics inc."]
tts[module_model == "lg305a1w-b3", module_manufacturer := "lg electronics inc."]
tts[module_model == "lg320n1c-g4", module_manufacturer := "lg electronics inc."]
tts[module_model == "lg335n1c-a5", module_manufacturer := "lg electronics inc."]
tts[module_model == "lg365q1c-a5", module_manufacturer := "lg electronics inc."]
tts[module_model == "lg375a1c-v5", module_manufacturer := "lg electronics inc."]
tts[module_model == "lg360q1c-a5", module_manufacturer := "lg electronics inc."]
tts[module_model == "lg320n1k-a5", module_manufacturer := "lg electronics inc."]
tts[module_model == "sc315b2", module_manufacturer := "tesla"]
tts[module_model == "sc330", module_manufacturer := "tesla"]
tts[module_model == "tsm-335dd06h.05(ii)", module_manufacturer := "trina solar"]
tts[module_model == "tsm-400de15h(ii)", module_manufacturer := "trina solar"]
tts[module_model == "sg310m", module_manufacturer := "peimar"]
tts[module_model == "q.peak duo-g5 325", module_manufacturer := "hanwha qcells"]
tts[module_model == "q.peak duo blk-g5 315", module_manufacturer := "hanwha qcells"]
tts[module_model == "jkm320m-60hbl-q", module_manufacturer := "jinko solar"]

list_firms = top_firms$module_manufacturer
tts = tts[module_manufacturer %in% list_firms,]

## Relative Premium --------------------------------------------------------

ggplot(tts[year %in% 2010:2012,], aes(x = reorder(module_manufacturer, efficiency_module, FUN = mean), 
                y = efficiency_module)) +
  geom_boxplot(fill = "steelblue", color = "black") +
  labs(
    x = "Manufacturer",
    y = "Efficiency Module (%)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("output/figures/firms_list/distrib_efficiency_2010_2012.pdf", width = 10, height = 7)

ggplot(tts[year %in% 2013:2016,], aes(x = reorder(module_manufacturer, efficiency_module, FUN = mean), 
                                      y = efficiency_module)) +
  geom_boxplot(fill = "steelblue", color = "black") +
  labs(
    x = "Manufacturer",
    y = "Efficiency Module (%)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("output/figures/firms_list/distrib_efficiency_2013_2016.pdf", width = 10, height = 7)

ggplot(tts[year %in% 2017:2020,], aes(x = reorder(module_manufacturer, efficiency_module, FUN = mean), 
                                      y = efficiency_module)) +
  geom_boxplot(fill = "steelblue", color = "black") +
  labs(
    x = "Manufacturer",
    y = "Efficiency Module (%)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("output/figures/firms_list/distrib_efficiency_2017_2020.pdf", width = 10, height = 7)

## Combo inverter + high efficiency ----------------------------------------
# Mono cristalyne are categorized as top quality solar panel, more innovative and more efficient
# The presence of micro inverter improve the overall efficiency of the system and makes it more desirable

tts[year %in% 2010:2013, quality_2_ad1 := ifelse(technology_module == "Mono-c-Si" & (micro_inverter_1 == "Y"|built_in_meter_inverter_1 == "Y"), 1, 0)]
tts[year %in% 2013:2016, quality_2_ad2 := ifelse(technology_module == "Mono-c-Si" & (micro_inverter_1 == "Y"|built_in_meter_inverter_1 == "Y"), 1, 0)]
tts[year %in% 2017:2020, quality_2_st := ifelse(technology_module == "Mono-c-Si" & (micro_inverter_1 == "Y"|built_in_meter_inverter_1 == "Y"), 1, 0)]

# Setting Price and Demand Variables --------------------------------------



# Export Data -------------------------------------------------------------
write_parquet(tts, data_final("tts_final.parquet"))
