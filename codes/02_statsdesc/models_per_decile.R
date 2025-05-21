# This code generates a table showing the number of observation per origin per subperiod for each decile

library(arrow)
library(data.table)
library(gt)

# Data --------------------------------------------------------------------

dist = setDT(read_parquet(data_final("tts_final.parquet")))

# Cleaning ----------------------------------------------------------------

dist[year %in% 2010:2013, qual_qt := cut(efficiency_module, 
                                        breaks = quantile(efficiency_module, 
                                                          probs = seq(0,1,0.10), 
                                                          na.rm=TRUE),
                                        include.lowest=TRUE,
                                        labels=paste0("Q",1:10)
)]
dist[year %in% 2014:2016, qual_qt := cut(efficiency_module, 
                                        breaks = quantile(efficiency_module, 
                                                          probs = seq(0,1,0.10), 
                                                          na.rm=TRUE),
                                        include.lowest=TRUE,
                                        labels=paste0("Q",1:10)
)]
dist[year %in% 2017:2018, qual_qt := cut(efficiency_module, 
                                        breaks = quantile(efficiency_module, 
                                                          probs = seq(0,1,0.10), 
                                                          na.rm=TRUE),
                                        include.lowest=TRUE,
                                        labels=paste0("Q",1:10)
)]

dist[, period := fcase(
  year <= 2013, "Anti-Dumping 2010-2013",
  year > 2013 & year <= 2016, "Anti-Dumping 2014-2016",
  year > 2016 & year <= 2018, "Trade War 2018",
  default = "post st"
)]

dist_clean = dist[, .N, by = .(period, qual_qt)]
dist_clean = dcast(data = dist_clean, formula = qual_qt ~ period)
dist_clean = dist_clean[2:11]
dist_clean = dist_clean[, .(qual_qt, `Anti-Dumping 2010-2013`, `Anti-Dumping 2014-2016`, `Trade War 2018`)]
dist_clean[, qual_qt := gsub("^Q", "D", qual_qt)]
setnames(dist_clean, "qual_qt", "Decile")

dist_clean %>%
  gt() %>%
  grand_summary_rows(
    # groups = NULL,
    columns = 2:4,
    fns = list(Total = ~sum(., na.rm = TRUE)),
    formatter = fmt_number,
    decimals = 0
  ) %>% gtsave("output/tables/num_obs_decile_overall.tex")

# For China ---------------------------------------------------------------

dist_clean_china = dist[origin == "china", .N, by = .(period, qual_qt)]
dist_clean_china = dcast(data = dist_clean_china, formula = qual_qt ~ period)
dist_clean_china = dist_clean_china[2:9]
dist_clean_china = dist_clean_china[, .(qual_qt, `Anti-Dumping 2010-2013`, `Anti-Dumping 2014-2016`, `Trade War 2018`)]
dist_clean_china[, qual_qt := gsub("^Q", "D", qual_qt)]
setnames(dist_clean_china, "qual_qt", "Decile")

dist_clean_china %>%
  gt() %>%
  grand_summary_rows(
    columns = 2:4,
    fns = list(Total = ~sum(., na.rm = TRUE)),
    formatter = fmt_number,
    decimals = 0
  ) %>% gtsave("output/tables/num_obs_decile_china.tex")
