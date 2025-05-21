# Share of sales by efficiency decile for a period

library(arrow)
library(data.table)
library(ggplot2)
library(glue)

# Data --------------------------------------------------------------------

tts = read_parquet(data_final("tts_final.parquet"))

# Cleaning ----------------------------------------------------------------

tts[year %in% 2010:2013, qual_qt := cut(efficiency_module, 
                                           breaks = quantile(efficiency_module, 
                                                             probs = seq(0,1,0.20), 
                                                             na.rm=TRUE),
                                           include.lowest=TRUE,
                                           labels=paste0("Q",1:5)
)]
tts[year %in% 2014:2016, qual_qt := cut(efficiency_module, 
                                           breaks = quantile(efficiency_module, 
                                                             probs = seq(0,1,0.20), 
                                                             na.rm=TRUE),
                                           include.lowest=TRUE,
                                           labels=paste0("Q",1:5)
)]
tts[year %in% 2017:2018, qual_qt := cut(efficiency_module, 
                                           breaks = quantile(efficiency_module, 
                                                             probs = seq(0,1,0.20), 
                                                             na.rm=TRUE),
                                           include.lowest=TRUE,
                                           labels=paste0("Q",1:5)
)]

tts[, period := fcase(year %in% 2010:2013, "AD1",
                      year %in% 2014:2016, "AD2",
                      year %in% 2017:2018, "ST",
                      default = NA)]

# Share sales -------------------------------------------------------------

for (p in c("AD1", "AD2", "ST")){
  sum_share = tts[period == `p`, .(sum_sales_efficiency = .N), by = .(year_quarter, qual_qt)]
  sum_share[, sum_sales := sum(sum_sales_efficiency), by = year_quarter]
  sum_share[, mkt_share := sum_sales_efficiency/sum_sales]
  setorder(sum_share, year_quarter, qual_qt)
  sum_share = sum_share[, .(year_quarter, qual_qt, mkt_share)]
  sum_share[, year_quarter := factor(year_quarter, levels = unique(year_quarter), ordered = TRUE)]
  
  ggplot(sum_share, aes(x = year_quarter, y = mkt_share, fill = qual_qt, group = qual_qt)) +
    geom_area(alpha = 0.9, size = 0.5, colour = "white", position = "stack") +
    scale_fill_brewer(palette = "Spectral") +
    labs(
      # title = "Market Share by Quality Quantile Over Time",
      x = "Year Quarter",
      y = "Market Share",
      fill = "Quality Quantile"
    ) +
    geom_vline(xintercept = "2012Q2", color = "black", linetype = "dashed") +
    geom_vline(xintercept = "2014Q2", color = "black", linetype = "dashed") +
    geom_vline(xintercept = "2018Q1", color = "black", linetype = "dashed") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(glue("output/figures/statdesc/mkt_share_quantile_quality_{p}_v2.pdf"), width = 8, height = 7)
}

# 1st Def -----------------------------------------------------------------
decile_breaks_2011 <- quantile(
  tts[year == 2011, efficiency_module],
  probs = seq(0, 1, 0.20),
  na.rm = TRUE
)

tts[year %in% 2010:2013, qual_qt := cut(efficiency_module,
                                        breaks = decile_breaks_2011,
                                        include.lowest = TRUE,
                                        labels = paste0("Q", 1:5))]

decile_breaks_2013 <- quantile(
  tts[year == 2013, efficiency_module],
  probs = seq(0, 1, 0.20),
  na.rm = TRUE
)

tts[year %in% 2014:2016, qual_qt := cut(efficiency_module,
                                        breaks = decile_breaks_2013,
                                        include.lowest = TRUE,
                                        labels = paste0("Q", 1:5))]

decile_breaks_2017 <- quantile(
  tts[year == 2017, efficiency_module],
  probs = seq(0, 1, 0.20),
  na.rm = TRUE
)

tts[year %in% 2017:2018, qual_qt := cut(efficiency_module,
                                        breaks = decile_breaks_2017,
                                        include.lowest = TRUE,
                                        labels = paste0("Q", 1:5))]


for (p in c("AD1", "AD2", "ST")){
  sum_share = tts[period == `p`, .(sum_sales_efficiency = .N), by = .(year_quarter, qual_qt)]
  sum_share[, sum_sales := sum(sum_sales_efficiency), by = year_quarter]
  sum_share[, mkt_share := sum_sales_efficiency/sum_sales]
  setorder(sum_share, year_quarter, qual_qt)
  sum_share = sum_share[, .(year_quarter, qual_qt, mkt_share)]
  sum_share[, year_quarter := factor(year_quarter, levels = unique(year_quarter), ordered = TRUE)]
  
  ggplot(sum_share, aes(x = year_quarter, y = mkt_share, fill = qual_qt, group = qual_qt)) +
    geom_area(alpha = 0.9, size = 0.5, colour = "white", position = "stack") +
    scale_fill_brewer(palette = "Spectral") +
    labs(
      # title = "Market Share by Quality Quantile Over Time",
      x = "Year Quarter",
      y = "Market Share",
      fill = "Quality Quantile"
    ) +
    geom_vline(xintercept = "2012Q2", color = "black", linetype = "dashed") +
    geom_vline(xintercept = "2014Q2", color = "black", linetype = "dashed") +
    geom_vline(xintercept = "2018Q1", color = "black", linetype = "dashed") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(glue("output/figures/statdesc/mkt_share_quantile_quality_{p}_v1.pdf"), width = 8, height = 7)
}
