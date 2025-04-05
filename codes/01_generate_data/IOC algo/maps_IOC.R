library(arrow)
library(data.table)
library(dplyr)
library(ggplot2)
library(zipcodeR)
library(viridisLite)
library(viridis)
library(sf)

# Data --------------------------------------------------------------------

market_assignments_ioc = fread(data_temp("market_assignments_ioc.csv"))
usa_zip <- st_read(data_raw("us_ztca_file/tl_2022_us_zcta520.shp"))
tts_small = read_parquet(data_final('TTS_final.parquet'), cols = c("zip_code", "year", "state"))

# Maps --------------------------------------------------------------------
# filter data
tts_small = tts_small[year == 2018]
zcta5 = unique(tts_small[, .(zip_code, state)])
zcta_db = zcta_crosswalk
setnames(zcta_db, "ZCTA5", "zip_code")

zcta5 = merge(zcta5, zcta_db, by = "zip_code", all.x = TRUE)

market_assignments_ioc$zip_code = as.character(market_assignments_ioc$zip_code)
zcta5$zip_code = as.character(zcta5$zip_code)
market_ca = merge(market_assignments_ioc, zcta5, by = "zip_code", all.x = TRUE)

market_ca = market_ca %>% 
  filter(state == "ca")

mapping = merge(usa_zip, market_ca, by.x = "ZCTA5CE20", by.y = "zip_code")

mapping_sf <- st_as_sf(mapping)

plot_ca = 
  ggplot(mapping) +
  geom_sf(aes(fill = factor(market_id)), color = "grey40", size = 0.2) +
  scale_fill_viridis_d(guide = "none", option = "plasma") +
  labs(title = "Installer-Based Market Definition") +
  theme_minimal() +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "none"
  )
ggsave(plot = plot_ca,"output/map/california_ioc_market.pdf", width = 10, height = 7)
