# We analyse the evolution of market share by brand origin country

library(arrow)
library(data.table)
library(ggplot2)

# Data --------------------------------------------------------------------

ts_brands = as.data.table(read_parquet(data_final("TTS_brands_year.parquet")))
ts_quarter = as.data.table(read_parquet(data_final("TTS_brands_quarter.parquet")))
top_manufacturer = read_parquet(data_temp("top_manufacturers.parquet"))

# Set ggplot Theme --------------------------------------------------------

theme_set(theme_minimal() +
            theme(
              plot.title = element_text(hjust = 0.5),
              legend.position = "bottom"
            ))
# Market Share by country of origin ------------------------------------------------------------

ts_brands = merge(ts_brands, top_manufacturer, by.x = "manufacturer", by.y = "Manufacturer")
ts_brands[, year_sales := sum(brand_sales_year), by = .(year)]
ts_brands[, share_country_brand := sum(brand_sales_year), by = .(year, Country)]
ts_share = ts_brands[, .(country_share = share_country_brand/year_sales), by = .(year, Country)]

ggplot(ts_share[year %in% 2010:2022], aes(x = year, y = country_share, group = Country, color = Country)) +
  geom_line() +
  labs(
    x = "Year",
    y = "Market Share (%)",
    title = "Market share by brand country of origin"
  ) + 
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 2014, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black") 
ggsave("output/figures/statdesc/market_share_country.pdf", width = 10, height = 8)

# ts_brands_alt = merge(ts_brands, top_manufacturer, by.x = "manufacturer", by.y = "Manufacturer")
# ts_brands_alt = ts_brands_alt[manufacturer %in% c("ja solar", "jinko solar")]
# ts_brands_alt[, year_sales := sum(brand_sales_year), by = .(year)]
# ts_brands_alt[, share_country_brand := sum(brand_sales_year), by = .(year, Country)]
# ts_share_alt = ts_brands_alt[, .(country_share = share_country_brand/year_sales), by = .(year, Country)]
# 
# ggplot(ts_share_alt[year %in% 2007:2022], aes(x = year, y = country_share, group = Country, color = Country)) +
#   geom_line()

# Market Share by Brand ------------------------------------------------------------

ts_brands = merge(ts_brands, top_manufacturer, by.x = "manufacturer", by.y = "Manufacturer")
ts_brands[, year_sales := sum(brand_sales_year), by = .(year)]
ts_brands[, share_brand := sum(brand_sales_year), by = .(year, manufacturer)]
ts_share = ts_brands[, .(country_share = share_brand/year_sales), by = .(year, manufacturer)]

chinese_brand = top_manufacturer[Country == "China",]$Manufacturer
korean_brand = top_manufacturer[Country == "South Korea",]$Manufacturer

ggplot(ts_share[year %in% 2010:2022 & manufacturer %in% c(korean_brand, chinese_brand)], 
       aes(x = year, y = country_share, group = manufacturer, color = manufacturer, linetype = manufacturer)) +
  geom_line() +
  scale_linetype_manual(
    values = setNames(
      c(rep("dashed", length(korean_brand)), rep("solid", length(chinese_brand))), 
      c(korean_brand, chinese_brand)
    )
  ) +
  labs(
    x = "Year",
    y = "Market share (%)", 
    title = "Market share of Chinese and Korean brands"
  ) +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 2014, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black") 
ggsave("output/figures/statdesc/market_share_corean_chinese_brands.pdf", width = 11, height = 8)

# ts_quarter = merge(ts_quarter, top_manufacturer, by.x = "manufacturer", by.y = "Manufacturer")
# ts_quarter[, quarter_sales := sum(brand_sales_quarter), by = .(year_quarter)]
# ts_quarter[, share_brand := sum(brand_sales_quarter), by = .(year_quarter, manufacturer)]
# ts_share_quarter = ts_quarter[, .(brand_share = share_brand/quarter_sales), by = .(year_quarter, manufacturer)]
# ts_share_quarter[, year := as.integer(substr(year_quarter, 1,4))]
# 
# ggplot(ts_share_quarter[year %in% 2007:2022 & manufacturer %in% c(korean_brand, chinese_brand)], aes(x = year, y = brand_share, group = manufacturer, color = manufacturer)) +
#   geom_line()+
#   labs(
#     x = "Year",
#     y = "Market share", 
#     title = "Market share of Chinese and Korean brands"
#   )

