# This code generate the main statsdesc output

rm(ts, ts_brands, ts_eff_1, ts_model, ts_sales)
gc()

library(arrow)
library(data.table)
library(ggplot2)
library(gt)
library(scales)

# Set theme ggplot --------------------------------------------------------

theme_set(theme_minimal() +
            theme(
              plot.title = element_text(hjust = 0.5),
              legend.position = "top"
            ))

# Data --------------------------------------------------------------------

ts_plot = as.data.table(read_parquet(data_temp("TTS_clean_names.parquet")))
ts_brands = as.data.table(read_parquet(data_final("TTS_brands.parquet")))

# installation rate and installation size -------------------------------------------------------
#quick graph to see the number of reported installion per year
ts_plot[, install_peryear := .N, by = c("year")]
install_rate = unique(ts_plot[, .(year, install_peryear)])

ts_plot[, install_pw_year := mean(PV_system_size_DC, na.rm = TRUE), by = (year)]
install_size = unique(ts_plot[, .(year, install_pw_year)])

install = merge(install_rate, install_size, by = c("year"))
install[, log_install_pw_year := log(install_pw_year)]

scale_factor <- max(install$install_peryear, na.rm = TRUE) / max(install$log_install_pw_year, na.rm = TRUE)

ggplot(install, aes(x = year)) +
  geom_col(aes(y = install_peryear), fill = "blue", alpha = 0.7) +  # Bar chart for installations
  geom_line(aes(y = log_install_pw_year * scale_factor), color = "red", size = 1.2, linetype = "dashed") +  # Line for avg power capacity
  scale_y_continuous(
    name = "Installations per year",  # Left y-axis label
    labels = label_comma(),
    sec.axis = sec_axis(
      ~ . / scale_factor,  # Transform back to original scale for avg power capacity
      name = "Average Installed Power Capacity (log(kW))"
    )
  ) +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black") +
  annotate(
    "text", x = 2012, y = max(install$install_peryear, na.rm = TRUE) * 0.65, 
    label = "Tariff", angle = 45, vjust = -1.5, color = "black"
  ) +
  annotate(
    "text", x = 2015, y = max(install$install_peryear, na.rm = TRUE) * 0.65, 
    label = "Tariff", angle = 45, vjust = -1.5, color = "black"
  ) +
  annotate(
    "text", x = 2018, y = max(install$install_peryear, na.rm = TRUE) * 0.65, 
    label = "Tariff", angle = 45, vjust = -1.5, color = "black"
  ) +
  labs(
    x = "Year",
    title = "Number of Installations and Average Installed Power Capacity"
  ) +
  theme(
    axis.title.y.right = element_text(color = "red"),  # Right axis color
    axis.text.y.right = element_text(color = "red"),   # Right axis text color
    axis.title.y.left = element_text(color = "blue"),  # Left axis color
    axis.text.y.left = element_text(color = "blue")    # Left axis text color
  )
ggsave("output/figures/statdesc/install_rate_pw_year.pdf")

rm(install, install_rate, install_size)
gc()

# Top brand per 5 years ---------------------------------------------------

ts_brands[, sum_sales_year := sum(brand_sales_year, na.rm = T), by = year]
ts_brands[, share_sales_brand := round(brand_sales_year/sum_sales_year, 4), by = year]
ts_brands[year %in% 2005:2010, share_sales_brand_05_10 := mean(share_sales_brand), by = manufacturer]
ts_brands[year %in% 2010:2015, share_sales_brand_10_15 := mean(share_sales_brand), by = manufacturer]
ts_brands[year %in% 2015:2020, share_sales_brand_15_20 := mean(share_sales_brand), by = manufacturer]

top_firms <- ts_brands[, .(share_sales_brand_05_10 = unique(share_sales_brand_05_10)), by = manufacturer]
top_firms <- top_firms[order(-share_sales_brand_05_10)]  # Sort in descending order
top_firms <- top_firms[1:10]  # Select top 10 manufacturers

# Generate the table using gt
table_05_10 <- gt(top_firms) %>%
  tab_header(
    title = "Mean Market Share of Top 10 Manufacturers (2005-2010)"
  ) %>%
  fmt_number(
    columns = c(share_sales_brand_05_10),
    decimals = 4
  ) %>%
  cols_label(
    manufacturer = "Manufacturer",
    share_sales_brand_05_10 = "Mean Share (2005-2010)"
  ) %>%
  tab_source_note(
    source_note = "Data: Calculated from Tracking the Sun"
  )

top_firms <- ts_brands[, .(share_sales_brand_10_15 = unique(share_sales_brand_10_15)), by = manufacturer]
top_firms <- top_firms[order(-share_sales_brand_10_15)]  # Sort in descending order
top_firms <- top_firms[1:10]  # Select top 10 manufacturers

# Generate the table using gt
table_10_15 <- gt(top_firms) %>%
  tab_header(
    title = "Mean Market Share of Top 10 Manufacturers (2010-2015)"
  ) %>%
  fmt_number(
    columns = c(share_sales_brand_10_15),
    decimals = 4
  ) %>%
  cols_label(
    manufacturer = "Manufacturer",
    share_sales_brand_10_15 = "Mean Share (2010-2015)"
  ) %>%
  tab_source_note(
    source_note = "Data: Calculated from Tracking the Sun"
  )

top_firms <- ts_brands[, .(share_sales_brand_15_20 = unique(share_sales_brand_15_20)), by = manufacturer]
top_firms <- top_firms[order(-share_sales_brand_15_20)]  # Sort in descending order
top_firms <- top_firms[1:10]  # Select top 10 manufacturers

# Generate the table using gt
table_15_20 <- gt(top_firms) %>%
  tab_header(
    title = "Mean Market Share of Top 10 Manufacturers (2015-2020)"
  ) %>%
  fmt_number(
    columns = c(share_sales_brand_15_20),
    decimals = 4
  ) %>%
  cols_label(
    manufacturer = "Manufacturer",
    share_sales_brand_15_20 = "Mean Share (2015-2020)"
  ) %>%
  tab_source_note(
    source_note = "Data: Calculated from Tracking the Sun"
  ) 

table_05_10 = as_latex(table_05_10)
table_10_15 = as_latex(table_10_15)
table_15_20 = as_latex(table_15_20)
cat(table_05_10, file = "output/tables/statdesc/table_brands_share_05_10.tex")
cat(table_10_15, file = "output/tables/statdesc/table_brands_share_10_15.tex")
cat(table_15_20, file = "output/tables/statdesc/table_brands_share_15_20.tex")

# Effiency Brand Level per year --------------------------------------------

chinese_brand = c("trina solar", "canadian solar", "yingli energy (china)")
concurrent_brand = c("rec solar", "maxeon - sunpower","lg electronics inc.")
ggplot(test_2[module_manufacturer_2 %in% c(chinese_brand, concurrent_brand)], aes(x = year, y = avg_eff_model_2, color = module_manufacturer_2)) +
  geom_line()

# Commercialized solar panel per brand ------------------------------------

chinese_brand = c("trina solar", "canadian solar", "yingli energy (china)")
concurrent_brand = c("rec solar", "maxeon - sunpower","lg electronics inc.")
ggplot(ts_model[manufacturer %in% concurrent_brand & year %in% 2004:2023], aes(x = year, y = brand_model_year, color = manufacturer)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2015) +
  labs(
    title = "Number of Models Sold per Year by Chinese Brands",
    x = "Year",
    y = "Number of Models Sold",
    color = "Brand"
  )
  


