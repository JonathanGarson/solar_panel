# This code generate the tables of main manafucturer in the U.S. over the 2005-2022 periods
# also generate a dataset of the main main manafucturer and their production facilities in the U.S. based on the DOE.

rm(ts, ts_brands, ts_eff_1, ts_model, ts_sales)
gc()

library(arrow)
library(data.table)
library(gt)
library(kableExtra)

# Data --------------------------------------------------------------------

ts_plot = as.data.table(read_parquet(data_temp("TTS_clean_names.parquet")))
ts_brands = data.table(read_parquet(data_final("TTS_brands.parquet")))

# Top brand per 5 years ---------------------------------------------------

ts_brands[, sum_sales_year := sum(brand_sales_year, na.rm = T), by = year]
ts_brands[, share_sales_brand := round(brand_sales_year/sum_sales_year, 4), by = year]
ts_brands[year %in% 2005:2010, share_sales_brand_05_10 := mean(share_sales_brand), by = manufacturer]
ts_brands[year %in% 2010:2015, share_sales_brand_10_15 := mean(share_sales_brand), by = manufacturer]
ts_brands[year %in% 2015:2020, share_sales_brand_15_20 := mean(share_sales_brand), by = manufacturer]
ts_brands[year %in% 2017:2022, share_sales_brand_17_22 := mean(share_sales_brand), by = manufacturer]

## 2005-2010 ---------------------------------------------------------------
top_firms_05_10 <- ts_brands[, .(share_sales_brand_05_10 = unique(share_sales_brand_05_10)), by = manufacturer]
top_firms_05_10 <- top_firms_05_10[order(-share_sales_brand_05_10)]  # Sort in descending order
top_firms_05_10 <- top_firms_05_10[1:15]  # Select top 10 manufacturers

top_mnf_05_10 = top_firms_05_10$manufacturer

# Generate the table using gt
table_05_10 <- gt(top_firms_05_10) %>%
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

## 2010-2015 ---------------------------------------------------------------

top_firms_10_15 <- ts_brands[, .(share_sales_brand_10_15 = unique(share_sales_brand_10_15)), by = manufacturer]
top_firms_10_15 <- top_firms_10_15[order(-share_sales_brand_10_15)]  # Sort in descending order
top_firms_10_15 <- top_firms_10_15[1:15]  # Select top 10 manufacturers

top_mnf_10_15 = top_firms_10_15$manufacturer

# Generate the table using gt
table_10_15 <- gt(top_firms_10_15) %>%
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

setnames(top_firms_10_15, colnames(top_firms_10_15), c("manufacturer_1", "share_sales_brand_10_15"))

## 2015-2020 ---------------------------------------------------------------

top_firms_15_20 <- ts_brands[, .(share_sales_brand_15_20 = unique(share_sales_brand_15_20)), by = manufacturer]
top_firms_15_20 <- top_firms_15_20[order(-share_sales_brand_15_20)]  # Sort in descending order
top_firms_15_20 <- top_firms_15_20[1:15]  # Select top 10 manufacturers

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

top_mnf_15_20 = top_firms$manufacturer

setnames(top_firms_15_20, colnames(top_firms_15_20), c("manufacturer_2", "share_sales_brand_15_20"))

## 2017-2022 ---------------------------------------------------------------

top_firms <- ts_brands[, .(share_sales_brand_17_22 = unique(share_sales_brand_17_22)), by = manufacturer]
top_firms <- top_firms[order(-share_sales_brand_17_22)]  # Sort in descending order
top_firms <- top_firms[1:15]  # Select top 10 manufacturers

# Generate the table using gt
table_17_22 <- gt(top_firms) %>%
  tab_header(
    title = "Mean Market Share of Top 10 Manufacturers (2017-2022)"
  ) %>%
  fmt_number(
    columns = c(share_sales_brand_17_22),
    decimals = 4
  ) %>%
  cols_label(
    manufacturer = "Manufacturer",
    share_sales_brand_17_22 = "Mean Share (2017-2022)"
  ) %>%
  tab_source_note(
    source_note = "Data: Calculated from Tracking the Sun"
  )

top_mnf_17_22 = top_firms$manufacturer


# 2010-2020 ---------------------------------------------------------------
# We merge the two tables around 2015 date
table_10_20 = cbind(top_firms_10_15, top_firms_15_20)
table_10_20 <- gt(table_10_20) %>%
  fmt_number(
    columns = c(share_sales_brand_10_15),
    decimals = 4
  ) %>%
  fmt_number(
    columns = c(share_sales_brand_15_20),
    decimals = 4
  ) %>%
  cols_label(
    manufacturer_1 = "Manufacturer",
    manufacturer_2 = "Manufacturer",
    share_sales_brand_10_15 = "Mean Share (2010-2015)",
    share_sales_brand_15_20 = "Mean Share (2015-2020)"
  )

# Export tables to latex --------------------------------------------------

table_05_10 = as_latex(table_05_10)
table_10_15 = as_latex(table_10_15)
table_15_20 = as_latex(table_15_20)
table_17_22 = as_latex(table_17_22)
table_10_20 = as_latex(table_10_20)
cat(table_05_10, file = "output/tables/statdesc/table_brands_share_05_10.tex")
cat(table_10_15, file = "output/tables/statdesc/table_brands_share_10_15.tex")
cat(table_15_20, file = "output/tables/statdesc/table_brands_share_15_20.tex")
cat(table_17_22, file = "output/tables/statdesc/table_brands_share_17_22.tex")
cat(table_10_20, file = "output/tables/statdesc/table_brands_share_10_20.tex")

# Build a list of top brands ----------------------------------------------
list_manufacturer = unique(c(top_mnf_05_10,top_mnf_10_15, top_mnf_15_20, top_mnf_17_22)) 

manufacturer_country <- data.table(
  Manufacturer = list_manufacturer,
  Country = c(
    "Japan", "United States", "United Kingdom", "Japan", "United States", 
    "Japan", "China", "Germany", "Japan", "China", "United States", "Norway", 
    "China", "Germany", "Germany", "China", "China", "South Korea", "China", 
    "South Korea", "South Korea", "China", "South Korea", "China", "China", 
    "United States", "Canada", "United States"
  ),
  Part_US_prod_2024 = c(0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,1,1,1,0,0,0,0,1,1) #based on the DOE list of manufacturing company in 2024
)

write_parquet(manufacturer_country, data_temp("top_manufacturers.parquet"))
