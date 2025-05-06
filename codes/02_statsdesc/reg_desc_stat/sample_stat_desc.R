# In this code we do descriptive statistics of our sample
# We try to recover the number of observation, prices, brands, brand by installer, rate of installations, system size
# Mean price, mean rebate, share of product treated by the tariff, concentration of the market at California level
# Tariff level per origin (weighted by firm share) and the nominal rate per firm
# Demographic data : population density, household median income, median value of the house

library(arrow)
library(data.table)
library(gt)
library(ggplot2)

# Data --------------------------------------------------------------------

tts = setDT(read_parquet(data_final("tts_final.parquet")))
tts_full = setDT(read_parquet(data_temp("tts_clean.parquet")))

# DEMOGRAPHICS -------------------------------------------------------
# THIS PART DEALS WITH THE DEMAND SIDE AND PLACE WHERE THE PANELS ARE INSTALLED
setkey(tts, county, year_quarter)

# Compute both mean and SE by group
summary_dt <- tts[, .(
  price_w_mean = mean(price_w, na.rm = TRUE),
  price_w_se = sd(price_w, na.rm = TRUE) ,
  
  rebate_w_mean = mean(rebate_w, na.rm = TRUE),
  rebate_w_se = sd(rebate_w, na.rm = TRUE) ,
  
  premium_panel_mean = mean(premium_panel_overall, na.rm = TRUE),
  premium_panel_se = sd(premium_panel_overall, na.rm = TRUE),
  
  premium_installation_mean = mean(premium_installation, na.rm = TRUE),
  premium_installation_se = sd(premium_installation, na.rm = TRUE),
  
  efficiency_mean = mean(efficiency_module, na.rm = TRUE),
  efficiency_se = sd(efficiency_module, na.rm = TRUE),
  
  price_panelpre_mean = mean(price_w[premium_panel_overall == 1], na.rm = TRUE),
  price_panelpre_se = sd(price_w[premium_panel_overall == 1], na.rm = TRUE),
  
  price_inspre_mean = mean(price_w[premium_installation == 1], na.rm = TRUE),
  price_inspre_se = sd(price_w[premium_installation == 1], na.rm = TRUE),
  
  PV_system_size_DC_mean = mean(PV_system_size_DC, na.rm = TRUE),
  PV_system_size_DC_se = sd(PV_system_size_DC, na.rm = TRUE),
  
  population_density_mean = mean(population_density, na.rm = TRUE),
  population_density_se = sd(population_density, na.rm = TRUE),
  
  median_household_income_mean = mean(median_household_income, na.rm = TRUE),
  median_household_income_se = sd(median_household_income, na.rm = TRUE),
  
  median_home_value_mean = mean(median_home_value, na.rm = TRUE),
  median_home_value_se = sd(median_home_value, na.rm = TRUE),
  
  pct_bachelor_estimate_mean = mean(pct_bachelor_estimate, na.rm = TRUE),
  pct_bachelor_estimate_se = sd(pct_bachelor_estimate, na.rm = TRUE)
  
), by = .(china)]

# Adding observation share
obs_share <- tts[, .N, by = china][order(china)]
total_obs <- sum(obs_share$N)
obs_share[, share := N / total_obs]
obs_row <- data.table(
  variable = "Share of Observations",
  mean_0 = round(obs_share[china == 0, share], 3),
  mean_1 = round(obs_share[china == 1, share], 3),
  se_0 = NA_real_,
  se_1 = NA_real_,
  group = "Sample Composition"
)

# Adding sum observation
sum_obs <- tts[, .N, by = china][order(china)]
sum_row <- data.table(
  variable = "Num. Observations",
  mean_0 = sum_obs[china == 0, N],
  mean_1 = sum_obs[china == 1, N],
  se_0 = NA_real_,
  se_1 = NA_real_,
  group = "Sample Composition"
)

long_summary <- melt(summary_dt, id.vars = "china", variable.name = "stat", value.name = "value")

long_summary[, type := fifelse(grepl("_mean$", stat), "mean", 
                               fifelse(grepl("_se$", stat), "se", NA_character_))]

long_summary[, variable := gsub("_(mean|se)$", "", stat)]

wide_summary <- dcast(long_summary, variable ~ type + china, value.var = "value")

rank_order <- data.table(
  variable = names(nice_labels),
  rank_order = c(
    # System-related variables
    price_w = 1,
    rebate_w = 2,
    premium_panel = 3,
    premium_installation = 4,
    efficiency = 5,
    price_panelpre = 6,
    price_inspre = 7,
    PV_system_size_DC = 8,
    
    # Socio-demographic variables
    population_density = 9,
    median_household_income = 10,
    median_home_value = 11,
    pct_bachelor_estimate = 12
  )
)

# Reorder lines
wide_summary = merge(wide_summary, rank_order, by = "variable")
setorder(wide_summary, rank_order)
wide_summary[, rank_order := NULL]

nice_labels <- c(
  price_w = "Price ($/W)",
  rebate_w = "Rebate ($/W)",
  premium_panel = "Premium Panel",
  premium_installation = "Premium Installation",
  efficiency = "Efficiency",
  price_panelpre = "Premium Panel Price",
  price_inspre = "Premium Installation Price",
  PV_system_size_DC = "System Size (kW DC)",
  population_density = "Population Density",
  median_household_income = "Median Household Income",
  median_home_value = "Median Home Value",
  pct_bachelor_estimate = "Share with BA Degree (%)"
)


# Apply relabeling
wide_summary[, variable := nice_labels[variable]]
wide_summary[, group := fifelse(
  variable %in% c(
    "Price ($/W)",
    "Rebate ($/W)",
    "Premium Panel",
    "Premium Installation",
    "Efficiency",
    "Premium Panel Price",
    "Premium Installation Price",
    "System Size (kW DC)"
  ),
  "Panel Characteristics",
  "Socio-demographics"
)]
wide_summary = rbind(wide_summary, obs_row, fill = TRUE)
wide_summary = rbind(wide_summary, sum_row, fill = TRUE)

# ADD SUM OBSERVATION/SHARE OF SAMPLE FOR BOTH
gt_table <- wide_summary |>
  gt(rowname_col = "variable", groupname_col = "group") |>
  
  # Column groups
  tab_spanner(label = "Mean", columns = c("mean_0", "mean_1")) |>
  tab_spanner(label = "Standard Error", columns = c("se_0", "se_1")) |>
  
  # Rename columns
  cols_label(
    mean_0 = "Non-Chinese",
    mean_1 = "Chinese",
    se_0 = "Non-Chinese",
    se_1 = "Chinese"
  ) |>
  
  # Format income and home value as currency
  fmt_currency(
    columns = c("mean_0", "mean_1"),
    rows = variable %in% c("Median Household Income", "Median Home Value"),
    currency = "USD"
  ) |>
  
  # Format all other rows as 2-digit numeric
  fmt_number(
    columns = everything(),
    rows = !variable %in% c("Median Household Income", "Median Home Value"),
    decimals = 2
  ) %>% gtsave("output/tables/statdesc/sample_description.tex")

# OFFER SIDE DESCRIPTIVE --------------------------------------------------
# IN THIS SECTION WE SHOW THE NUMBER OF FIRMS OPERATING, HOW MANY BRANDS AN INSTALLER INSTALL ON AVERAGE, 

# ADD NUMBER OF INSTALLER

# Step 1: Count installs by installer and brand
installer <- tts[, .(sum_install_brand = .N), by = .(installer_name, module_manufacturer)]

# Step 2: Total installs per installer
installer[, sum_install := sum(sum_install_brand), by = installer_name]

# Step 3: Compute share per brand
installer[, share := sum_install_brand / sum_install]

# Step 4: Compute final table:
installer <- installer[, .(
  n_brands = uniqueN(module_manufacturer),
  max_brand_share = max(share),
  hhi = sum(share^2)
), by = installer_name]

# Step 5 compute on average
installer_summary = installer[,
                              .(n_brands = round(median(n_brands), 3),
                              max_brand_share = mean(max_brand_share),
                              hhi = mean(hhi))]
setnames(installer_summary, colnames(installer_summary), c("# Brands", "Max. Brand Share", "HHI"))
installer_gt = gt(installer_summary) %>% 
  gtsave(filename = "output/tables/installer/installer_stat.tex")

# TARIFF EXPOSITION -------------------------------------------------------

tts[, origin := ifelse(origin != "china", "Other", origin)]
tts[, origin := ifelse(origin == "china", "China", origin)]
tts[, module_manufacturer := ifelse(origin == "other", "Other", module_manufacturer)]
# Total sales over full period, by brand within origin
brand_sales_total <- tts[, .(brand_sales = sum(module_quantity, na.rm = TRUE)), 
                         by = .(module_manufacturer, origin)]

# Total origin sales (over entire period)
origin_sales_total <- brand_sales_total[, .(origin_sales = sum(brand_sales)), by = origin]

# Merge to get fixed share within origin
brand_sales_total <- merge(brand_sales_total, origin_sales_total, by = "origin")
brand_sales_total[, share_within_origin := brand_sales / origin_sales]

# Brand-level tariff over time
tariff_brand <- unique(tts[, .(module_manufacturer, year_quarter, tariff)])
tariff_brand[, tariff := (tariff - 1) * 100]  # Convert to percentage

# Merge fixed weights with tariff timeline
tariff_brand_share <- merge(brand_sales_total, tariff_brand, 
                            by = "module_manufacturer", allow.cartesian = TRUE)
# Weighted average using fixed weights
tariff_brand_share[, weighted_tariff := share_within_origin * tariff]
origin_weighted_tariff <- tariff_brand_share[, .(
  origin_tariff = sum(weighted_tariff, na.rm = TRUE)
), by = .(origin, year_quarter)]

ggplot(origin_weighted_tariff, aes(x = year_quarter, y = origin_tariff, group = origin, color = origin)) +
  geom_line(size = 1.2) +
  theme_classic() +
  labs(
    # title = "Weighted Tariff by Origin Over Time",
       x = "Quarter", y = "Tariff (%)", color = "Origin") +
  theme(legend.position = "bottom") +
  scale_x_discrete(breaks = origin_weighted_tariff[grepl("Q1$", year_quarter), unique(year_quarter)])
ggsave("output/figures/tariff/weighted_tariff_origin.pdf", width = 10, height = 8)

brand_tariff_plot <- unique(tariff_brand_share[, .(
  module_manufacturer, origin, year_quarter, tariff
)])

ggplot(brand_tariff_plot[origin == "China"], 
       aes(x = year_quarter, y = tariff, group = module_manufacturer,color = module_manufacturer)) +
  geom_line(size = 0.9, alpha = 0.8) +
  theme_classic() +
  labs(
    # title = "Nominal Tariff by Chinese Brand",
       x = "Quarter", y = "Tariff (%)", color = "Brand") +
  theme(legend.position = "bottom") +
  scale_x_discrete(breaks = origin_weighted_tariff[grepl("Q1$", year_quarter), unique(year_quarter)])
ggsave("output/figures/tariff/nominal_tariff_brand_china.pdf", width = 10, height = 8)

# PRICE EVOLUTION ---------------------------------------------------------

price_evol <- tts[, .(
  mean_price_w = mean(price_w, na.rm = TRUE),
  mean_rebate_w = mean(rebate_w, na.rm = TRUE)
), by = year_quarter]

price_evol[, year_quarter_date := as.Date(paste0(substr(year_quarter, 1, 4), "-", 
                                                 as.integer(substr(year_quarter, 6, 6)) * 3 - 2, "-01"))]

ggplot(price_evol, aes(x = year_quarter_date)) +
  # Main price trend line
  geom_line(aes(y = mean_price_w), color = "steelblue", size = 1) +
  geom_line(aes(y = mean_rebate_w), color = "red", size = 1, linetype = "dashed") +
  # geom_point(color = "steelblue", size = 1.5) +
  
  # Quarter labels: only Q1
  scale_x_date(
    breaks = price_evol[grepl("Q1$", year_quarter), unique(year_quarter_date)],
    date_labels = "%YQ1"
  ) +
  
  # Labels and theme
  labs(
    # title = "Average Price per Watt Over Time (with SD Band)",
    x = "Quarter",
    y = "Price ($/W)"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )
ggsave("output/figures/statdesc/price_evol.pdf", width = 10, height = 8)


# PREMIUM PANEL -----------------------------------------------------------
premium_table = tts[, .N, by = .(premium_panel_overall, year)]
premium_wide = dcast(premium_table, year ~premium_panel_overall, value.var = "N")
setnames(premium_wide, colnames(premium_wide), c("Year", "Standard", "Premium"))
premium_wide[, share := round((Premium/Standard)*100, 2)]
premium_wide[, sum := Standard+ Premium]
setnames(premium_wide, c("share", "sum"), c("Share", "Sum"))
premium_wide = premium_wide[, .(Year, Standard, Premium, Sum, Share)]

# premium_chinese = tts[china == 1, .N, by = .(premium_panel_overall, year)]
# premium_installation = tts[china == 1, .N, by = .(premium_installation, year)]


premium_table_gt <- gt(premium_wide) |>
  fmt_percent(
    columns = Share,
    decimals = 2,
    scale_values = FALSE  # Share is already in 0–100 range
  ) %>% 
  gtsave("output/tables/panel_premium_overall.tex")

# All combinations: panel vs china
panel_counts <- tts[, .N, by = .(china, premium_panel_overall, year)]
panel_wide <- dcast(panel_counts, year + china ~ premium_panel_overall, value.var = "N", fill = 0)
setnames(panel_wide, c("0", "1"), c("Standard_panel", "Premium_panel"))
panel_wide[, total := Standard_panel + Premium_panel]
panel_wide[, share_panel := round(100 * Premium_panel / total, 2)]

# All combinations: installation vs china
inst_counts <- tts[, .N, by = .(china, premium_installation, year)]
inst_wide <- dcast(inst_counts, year + china ~ premium_installation, value.var = "N", fill = 0)
setnames(inst_wide, c("0", "1"), c("Standard_inst", "Premium_inst"))
inst_wide[, total := Standard_inst + Premium_inst]
inst_wide[, share_install := round(100 * Premium_inst / total, 2)]

# Merge by year and china
summary_share <- merge(
  panel_wide[, .(year, china, share_panel)],
  inst_wide[, .(year, china, share_install)],
  by = c("year", "china")
)

# Label group
summary_share[, Group := fifelse(china == 1, "Chinese", "Non-Chinese")]
summary_share[, china := NULL]  # remove numeric flag
setcolorder(summary_share, c("year", "Group", "share_panel", "share_install"))

# Wide by metric (Panel vs Installation)
summary_gt <- dcast(summary_share, year ~ Group, 
                    value.var = c("share_panel", "share_install"))

# Rename columns for clarity
setnames(summary_gt, c("share_panel_Chinese", "share_panel_Non-Chinese", 
                       "share_install_Chinese", "share_install_Non-Chinese"),
         c("Panel (Chinese)", "Panel (Non-Chinese)", 
           "Install (Chinese)", "Install (Non-Chinese)"))

gt_share_table <- gt(summary_gt) |>
  tab_spanner(label = "Share of Premium Panels", 
              columns = c("Panel (Chinese)", "Panel (Non-Chinese)")) |>
  tab_spanner(label = "Share of Premium Installation", 
              columns = c("Install (Chinese)", "Install (Non-Chinese)")) |>
  fmt_percent(
    columns = c("Panel (Chinese)","Panel (Non-Chinese)","Install (Chinese)","Install (Non-Chinese)"),
    rows = everything(),
    decimals = 2,
    scale_values = FALSE
  ) |>
  tab_header(title = "Share of Premium Panels and Installation by Origin") |>
  cols_label(year = "Year") %>% 
  gtsave("output/tables/statdesc/premium_panel_install_share_origin.tex")

# INSTALLATION ------------------------------------------------------------
tts_full = tts_full[ho == 1, ]

# Full sample (all installs)
install_summary_full <- tts_full[, .(sum_installation = .N), by = year_quarter]
install_summary_full[, source := "Full sample"]

# Subsample (ho == 1)
install_summary_ca <- tts[, .(sum_installation = .N), by = year_quarter]
install_summary_ca[, source := "California"]

# Combine for plotting
install_plot_data <- rbindlist(list(install_summary_full, install_summary_ca))

# Convert to date (optional for x-axis control)
install_plot_data[, year_quarter_date := as.Date(paste0(substr(year_quarter, 1, 4), "-", 
                                                        as.integer(substr(year_quarter, 6, 6)) * 3 - 2, "-01"))]

ggplot(install_plot_data, aes(x = year_quarter_date, y = sum_installation, fill = source)) +
  geom_col(position = "identity", width = 70, alpha = 0.6) +
  scale_fill_manual(
    values = c("Full sample" = "grey", "California" = "steelblue"),
    name = "Sample"
  ) +
  theme_classic() +
  labs(
    x = "Year",
    y = "Number of Installations",
  ) + 
  theme(legend.position = "bottom")
ggsave("output/figures/statdesc/sample_installation.pdf", width = 10, height = 8)


# Draft -------------------------------------------------------------------

# Compute market shares
market_share <- tts[, .(install_china = sum(module_quantity)), by = .(china, year)][order(year)]
market_share[, share := install_china[china == 1]/install_china[china == 0], by = year]

# Optional: relabel for display
market_share[, group := fifelse(china == 1, "Chinese", "Non-Chinese")]

ggplot(market_share, aes(x = year, y = share, fill = group)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Market Share by Group",
    x = "",
    y = "Share of Observations"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  
