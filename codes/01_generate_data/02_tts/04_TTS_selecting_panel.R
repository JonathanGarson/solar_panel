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
wages = fread(data_temp("elec_contractor_wage_emp.csv"))
elec = fread(data_temp("elec_price.csv"))

# Selecting firms ---------------------------------------------------------

# Market share of installation by brand approach : we select the 15 biggest firms on the period, their market share is above 3%.

# Displaying the number of installation per model per year
tts[, sales_per_model := sum(module_quantity, na.rm = T) , by = c("year","module_model")]

# We select the firm that represented 90% of the market share between 2010 and 2020
tts[, sales_per_brand := sum(module_quantity, na.rm = T) , by = c("module_manufacturer")]
tts[, sales_overall := sum(module_quantity)]
tts[, market_share_period := round(sales_per_brand/sales_overall, 4)]
market_share = unique(tts[, .(module_manufacturer, market_share_period)])
setorder(market_share, -market_share_period)
market_share[, cum_sum_share := cumsum(market_share_period)]
export_dt = market_share[cum_sum_share <= 0.9, .(module_manufacturer, market_share_period)]
list_country = c("USA", "South Korea", "South Korea", "Germany", "China", "Norway", "China", "China", "Japan", "South Korea", "China", "China", "USA","USA", "Japan", "China")
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
# We get rid of Longi given its unclear position with respect to tariff and only represent 2% sales
tts = tts[module_manufacturer != "longi green energy technology co., ltd."]
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
for (y in c(2010, 2012, 2014, 2017, 2019)) {
  # Calculate the quantiles for the given year; include na.rm = TRUE to handle missing values.
  pct_eff <- quantile(models_dt[year == y]$efficiency, 
                      probs = c(0.5, 0.75, 0.9, 0.95), 
                      na.rm = TRUE)
  
  # # Append the calculated values to the data frame
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

efficiency_year = tts[, .(efficiency_module), by = year]
top_efficiency = unique(efficiency_year[, .(top_efficiency = unique(quantile(efficiency_module, probs =c(0.90)))), by = year])
tts = merge(tts, top_efficiency, by = "year")
tts[, premium_panel_overall := ifelse(efficiency_module > top_efficiency, 1, 0)]

# AD 1 : 2010-2013
tts[year %in% 2010:2013 , premium_panel_ad1 := ifelse(efficiency_module >= pct_eff_dt[year == 2012,]$p90, 1, 0)]

# AD 2 : 2014-2016
tts[year %in% 2014:2016, premium_panel_ad2 := ifelse(efficiency_module >= pct_eff_dt[year == 2014,]$p90, 1, 0)]

# Safeguard : 2017-2020
tts[year %in% 2017:2020, premium_panel_st := ifelse(efficiency_module >= pct_eff_dt[year == 2017,]$p90, 1, 0)]

# Overall
tts[, premium_panel_overall_sec := fcase(premium_panel_ad1 ==1, 1,
                             premium_panel_ad2 ==1, 1,
                             premium_panel_st ==1,  1,
                             default = 0)]

list_firms = top_firms$module_manufacturer
tts = tts[module_manufacturer %in% list_firms,]

## Combo inverter + high efficiency ----------------------------------------
# Mono cristalyne are categorized as top quality solar panel, more innovative and more efficient
# The presence of micro inverter improve the overall efficiency of the system and makes it more desirable

tts[, premium_installation := ifelse(micro_inverter_1 == "Y", 1, 0)]

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
tts[, year_quarter.y := NULL]
tts[, year_quarter:= NULL]
setnames(tts, "year_quarter.x", "year_quarter")

# AD1
tts[year_quarter >= "2012Q2" & year_quarter <= "2014Q1", tariff:= 1 + (ad_rate_2012 + cvd_rate_2012)/100]
tts[year_quarter >= "2012Q2" & year_quarter <= "2014Q1", tariff_temp:= 1+ (ad_rate_2012 + cvd_rate_2012)/100]
tts[year_quarter >= "2010Q1" & year_quarter <= "2014Q1", treated:= ifelse(tariff > 1 & year_quarter >= "2012Q2" & year_quarter <= "2014Q1", 1, 0)]

# AD2
tts[year_quarter >= "2014Q2" & year_quarter <= "2017Q4", tariff:= 1 + (ad_rate_2015)/100]
tts[year_quarter >= "2014Q2" & year_quarter <= "2017Q4", tariff_temp:= 1 + (ad_rate_2015 + cvd_rate_2015)/100] 
tts[year_quarter >= "2014Q1" & year_quarter <= "2016Q4", treated:= ifelse(tariff > 1 & year_quarter >= "2014Q2" & year_quarter <= "2014Q4", 1, 0)]

# ST
tts[year_quarter %in% c("2018Q1","2018Q2"), tariff:= 1 + (ad_rate_2015 + 30)/100]
tts[year_quarter %in% c("2018Q1","2018Q2"), tariff_temp:= 1 + (ad_rate_2015 + cvd_rate_2015 + 30)/100]
tts[year_quarter >= "2017Q1" & year_quarter <= "2020Q4", treated:= ifelse(year_quarter >= "2018Q1" & year_quarter <= "2020Q4", 1, 0)]

# Top up tariff of July 2018
tts[year_quarter > "2018Q2", tariff:= 1 + (ad_rate_2015 + 55)/100]
tts[year_quarter > "2018Q2", tariff_temp:= 1 + (ad_rate_2015 + cvd_rate_2015 + 55)/100]
tts[year %in% 2010:2018, tariff := ifelse(is.na(tariff), 1, tariff)]
tts[!module_manufacturer %in% c("canadian solar", "trina solar", "jinko solar", "yingli energy (china)", "suntech power") & year_quarter >= "2018Q1" & year_quarter <= "2018Q4", 
    tariff := 1.30]
tts[!module_manufacturer %in% c("canadian solar", "trina solar", "jinko solar", "yingli energy (china)", "suntech power") & year_quarter >= "2019Q1" & year_quarter <= "2019Q4", 
    tariff := 1.25]
tts[!module_manufacturer %in% c("canadian solar", "trina solar", "jinko solar", "yingli energy (china)", "suntech power") & year_quarter >= "2020Q1" & year_quarter <= "2020Q4", 
    tariff := 1.20]

# We merge with electricity price and wages 
tts = merge(tts, elec, by = c("state", "year_quarter"), all.x = TRUE)
tts = merge(tts, wages, by = c("county", "year"), all.x = TRUE)

# Cleaning variables
tts[, micro_inverter_1 := fcase(micro_inverter_1 == "Y", 1, 
                                micro_inverter_1 == "N", 0,
                                default = NA)]

tts[, ground_mounted := fcase(ground_mounted == "1", 1, 
                              ground_mounted == "0", 0,
                              default = NA)]

# Cleaning before export --------------------------------------------------
tts[, origin := tolower(list_country)]
tts[, sales_per_model := NULL]
tts[, sales_per_brand := NULL]
tts[, sales_overall := NULL]
tts[, nb_manufacturer := NULL]

# We only keep HO data
tts = tts[ho == 1,]

# We only keep 43 rows
cols_to_keep <- c("county", "zip_code", "tract", "year", "year_quarter", "module_manufacturer", "installer_name", "origin",
                  "PV_system_size_DC", "total_installed_price", "rebate_or_grant", "efficiency_module", "treated", "ground_mounted" ,
                  "module_quantity", "module_model", "premium_panel_ad1", "premium_panel_ad2", "premium_panel_st",
                  "price_w", "rebate_w", "ow_occupied_housing", "self_installed", "population", "population_density",
                  "land_area_in_sqmi", "tract", "geoid", "pct_bachelor_estimate",
                  "median_home_value", "median_household_income", "market_share_period", "premium_panel_overall",
                  "premium_installation", "tariff", "tariff_temp","elec_price",
                  "mean_price_year", "mean_month_emp", "mean_week_wage", "state")

tts = tts[, ..cols_to_keep]

# We only keep California to preserve stability in our data
tts = tts[state == "ca"]
tts = tts[population > 0,]

# Export Data -------------------------------------------------------------
# Export NY State as a Placebo
write_parquet(tts, data_final("tts_final.parquet"))
fwrite(tts, data_final("tts_final.csv"))
