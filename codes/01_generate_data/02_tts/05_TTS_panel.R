# This code reorganizes the final data into a panel : firm * quarter
library(arrow)
library(data.table)

# Data
tts = setDT(read_parquet(data_final("tts_final.parquet")))

# Panel
tts_panel = tts[year %in% 2013:2016,  .(price_w = mean(price_w, na.rm = T),
  tariff = tariff,
  efficiency = mean(efficiency_module, na.rm = T),
  origin = origin), 
  by = .(module_manufacturer, year_quarter)]

# Export
write_parquet(tts_panel, data_final("tts_panel.parquet"))
fwrite(tts_panel, data_final("tts_panel.csv"))