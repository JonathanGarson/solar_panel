# We derive descriptive graph from proxy install price

library(arrow)
library(data.table)
library(ggplot2)

tts = read_parquet(data_final("tts_final.parquet"))

tts[, avg_proxy_price := mean(proxy_panel_price_w, na.rm = TRUE), by = .(module_manufacturer, year_quarter)]

q1_labels <- unique(tts[grepl("Q1$", year_quarter)]$year_quarter)

ggplot(tts[module_manufacturer %in% c("canadian solar", "trina solar", "jinko solar", "yingli energy (china)")],
       aes(x = year_quarter, y = avg_proxy_price_q, group = module_manufacturer, color = module_manufacturer)
  ) +
  geom_line() +
  geom_vline(xintercept = "2012Q2") +
  geom_vline(xintercept = "2014Q2") + 
  geom_vline(xintercept = "2018Q1") +
  scale_x_discrete(breaks = q1_labels) +
  labs(
    x = "Year",
    y = "Average Proxy Price of Panel ($/W)"
  )+
  theme_classic() 
