# We analyse the evolution of market share by brand origin country

library(arrow)
library(data.table)
library(ggplot2)

# Data --------------------------------------------------------------------

ts_brands = as.data.table(read_parquet(data_final("TTS_brands.parquet")))
top_manufacturer = read_parquet(data_temp("top_manufacturers.parquet"))

# Market Share ------------------------------------------------------------

ts_brands = merge(ts_brands, top_manufacturer, by.x = "manufacturer", by.y = "Manufacturer")
ts_brands[, year_sales := sum(brand_sales_year), by = .(year)]
ts_brands[, share_country_brand := sum(brand_sales_year), by = .(year, Country)]
ts_share = ts_brands[, .(country_share = share_country_brand/year_sales), by = .(year, Country)]

ggplot(ts_share[year %in% 2007:2022], aes(x = year, y = country_share, group = Country, color = Country)) +
  geom_line()

#need to get the quarterly information
#need data by firm

ts_brands_alt = merge(ts_brands, top_manufacturer, by.x = "manufacturer", by.y = "Manufacturer")
ts_brands_alt = ts_brands_alt[manufacturer %in% c("ja solar", "jinko solar")]
ts_brands_alt[, year_sales := sum(brand_sales_year), by = .(year)]
ts_brands_alt[, share_country_brand := sum(brand_sales_year), by = .(year, Country)]
ts_share_alt = ts_brands_alt[, .(country_share = share_country_brand/year_sales), by = .(year, Country)]

ggplot(ts_share_alt[year %in% 2007:2022], aes(x = year, y = country_share, group = Country, color = Country)) +
  geom_line()
