library(arrow)
library(data.table)
library(ggplot2)

# Data --------------------------------------------------------------------

ts = read_parquet(data_final("TTS_clean.parquet"))

# installation rate -------------------------------------------------------
#quick graph to see the number of reported installion per year
ts[, install_peryear := .N, by = c("year")]
install_rate = unique(ts[year %in% 2001:2023, .(year, install_peryear)])
setorder(install_rate)

ggplot(install_rate, aes(x = year, y = install_peryear))+
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
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )

# Effiency Brand Level per year --------------------------------------------


