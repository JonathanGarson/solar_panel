# This code panel data at the firm x month level

library(arrow)
library(data.table)

# Data --------------------------------------------------------------------

panel = read_parquet(data_final("/tts_final.parquet"))

# Panel Data --------------------------------------------------------------

panel_clean = panel[year %in% 2013:2016,  .(
  price_w = mean(price_w),
  rebate_w = mean(rebate_w),
  efficiency_module = mean(efficiency_module),
  tariff = mean(tariff)
), by = .(module_manufacturer, year_quarter)]

setorder(panel_clean, year_quarter)