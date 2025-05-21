# This code generates a table showing the number of observation per origin per subperiod for each decile

library(arrow)
library(data.table)
library(gt)

# Data --------------------------------------------------------------------

dist = setDT(read_parquet(data_final("tts_final.parquet")))

# 1st Def ----------------------------------------------------------------
decile_breaks_2011 <- quantile(
  dist[year == 2011, efficiency_module],
  probs = seq(0, 1, 0.20),
  na.rm = TRUE
)

dist[year %in% 2010:2013, qual_qt := cut(efficiency_module,
                                        breaks = decile_breaks_2011,
                                        include.lowest = TRUE,
                                        labels = paste0("Q", 1:5))]

decile_breaks_2013 <- quantile(
  dist[year == 2013, efficiency_module],
  probs = seq(0, 1, 0.20),
  na.rm = TRUE
)

dist[year %in% 2014:2016, qual_qt := cut(efficiency_module,
                                        breaks = decile_breaks_2013,
                                        include.lowest = TRUE,
                                        labels = paste0("Q", 1:5))]

decile_breaks_2017 <- quantile(
  dist[year == 2017, efficiency_module],
  probs = seq(0, 1, 0.20),
  na.rm = TRUE
)

dist[year %in% 2017:2018, qual_qt := cut(efficiency_module,
                                        breaks = decile_breaks_2017,
                                        include.lowest = TRUE,
                                        labels = paste0("Q", 1:5))]

dist[, period := fcase(
  year <= 2013, "2010-2013",
  year > 2013 & year <= 2016, "2014-2016",
  year > 2016 & year <= 2018, "2018",
  default = "post st"
)]

dist_clean = dist[, .N, by = .(period, qual_qt)]
dist_clean = dcast(data = dist_clean, formula = qual_qt ~ period)
dist_clean = dist_clean[2:6]
dist_clean = dist_clean[, .(qual_qt, `2010-2013`, `2014-2016`, `2018`)]
setnames(dist_clean, "qual_qt", "Quantile")

dist_clean %>%
  gt() %>%
  grand_summary_rows(
    # groups = NULL,
    columns = 2:4,
    fns = list(Total = ~sum(., na.rm = TRUE)),
    formatter = fmt_number,
    decimals = 0
  ) %>% gtsave("output/tables/num_obs_quintile_overall.tex")

## For China ---------------------------------------------------------------

dist_clean_china = dist[origin == "china", .N, by = .(period, qual_qt)]
dist_clean_china = dcast(data = dist_clean_china, formula = qual_qt ~ period)
dist_clean_china = dist_clean_china[2:6]
dist_clean_china = dist_clean_china[, .(qual_qt, `2010-2013`, `2014-2016`, `2018`)]
setnames(dist_clean_china, "qual_qt", "Quantile")

dist_clean_china %>%
  gt() %>%
  grand_summary_rows(
    columns = 2:4,
    fns = list(Total = ~sum(., na.rm = TRUE)),
    formatter = fmt_number,
    decimals = 0
  ) %>% gtsave("output/tables/num_obs_quintile_china.tex")

# 2nd Def -----------------------------------------------------------------

dist[year %in% 2010:2013, qual_qt := cut(efficiency_module,
                     breaks = quantile(efficiency_module,
                                       probs = seq(0,1,0.20),
                                       na.rm=TRUE),
                     include.lowest=TRUE,
                     labels=paste0("Q",1:5)
)]
dist[year %in% 2014:2016, qual_qt := cut(efficiency_module,
                     breaks = quantile(efficiency_module,
                                       probs = seq(0,1,0.20),
                                       na.rm=TRUE),
                     include.lowest=TRUE,
                     labels=paste0("Q",1:5)
)]
dist[year %in% 2017:2018, qual_qt := cut(efficiency_module,
                     breaks = quantile(efficiency_module,
                                       probs = seq(0,1,0.20),
                                       na.rm=TRUE),
                     include.lowest=TRUE,
                     labels=paste0("Q",1:5)
)]

dist[, period := fcase(
  year <= 2013, "2010-2013",
  year > 2013 & year <= 2016, "2014-2016",
  year > 2016 & year <= 2018, "2018",
  default = "post st"
)]

dist_clean = dist[, .N, by = .(period, qual_qt)]
dist_clean = dcast(data = dist_clean, formula = qual_qt ~ period)
dist_clean = dist_clean[2:6]
dist_clean = dist_clean[, .(qual_qt, `2010-2013`, `2014-2016`, `2018`)]
setnames(dist_clean, "qual_qt", "Quintile")

dist_clean %>%
  gt() %>%
  grand_summary_rows(
    # groups = NULL,
    columns = 2:4,
    fns = list(Total = ~sum(., na.rm = TRUE)),
    formatter = fmt_number,
    decimals = 0
  ) %>% gtsave("output/tables/num_obs_quintile_overall_v2.tex")

## For China ---------------------------------------------------------------

dist_clean_china = dist[origin == "china", .N, by = .(period, qual_qt)]
dist_clean_china = dcast(data = dist_clean_china, formula = qual_qt ~ period)
dist_clean_china = dist_clean_china[2:6]
dist_clean_china = dist_clean_china[, .(qual_qt, `2010-2013`, `2014-2016`, `2018`)]
setnames(dist_clean_china, "qual_qt", "Quintile")

dist_clean_china %>%
  gt() %>%
  grand_summary_rows(
    columns = 2:4,
    fns = list(Total = ~sum(., na.rm = TRUE)),
    formatter = fmt_number,
    decimals = 0
  ) %>% gtsave("output/tables/num_obs_quintile_china_v2.tex")
