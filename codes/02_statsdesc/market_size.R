# This code calculate the market size by states

library(arrow)
library(data.table)
library(ggplot2)
library(kableExtra)
library(lubridate)
library(scales)
library(zoo)

# Data --------------------------------------------------------------------

ts_origin = as.data.table(read_parquet(data_raw("TTS.parquet")))
ts_new = as.data.table(read_parquet(data_temp("TTS_clean_names.parquet")))

# Market size before cleaning -------------------------------------------------------------

ts_origin[, installation_date := dmy(installation_date)]
ts_origin[, year := year(installation_date)] #adding a year column
ts_origin[, year_quarter := paste0(year(installation_date), "Q", quarter(installation_date))]
market_size_origin = ts_origin[year %in% 2010:2023, .N, by = .(year_quarter, state)]
market_size_origin$date <- as.Date(as.yearqtr(market_size_origin$year_quarter, format = "%YQ%q"))

ggplot(market_size_origin, aes(x = date, y = N, color = state, group = state)) +
  geom_line() + 
  labs(
    x = "Year",
    y = "Installation",
    color = "States",
    title = "Number of installations in each state before data cleaning"
  ) +
  # Convert the vertical lines from quarter strings to Date
  geom_vline(xintercept = as.Date(as.yearqtr("2012Q4", format = "%YQ%q")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date(as.yearqtr("2014Q3", format = "%YQ%q")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date(as.yearqtr("2018Q1", format = "%YQ%q")), linetype = "dashed", color = "black") +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal()
ggsave("output/figures/statdesc/market_size_state_us.pdf", width = 10, height = 7)

list_state_suppress_by_cleaning = c("WI","ME","MD","AR","FL","VA","DC","WA","OH","NJ","DE","CO","RI","UT")

market_stock = ts_origin[, .(installed_system = uniqueN(system_ID_1)), by = state]
market_stock[, share_installation := installed_system/sum(installed_system)]
setorder(market_stock, -share_installation)
market_stock[, cumulative_share := cumsum(share_installation)]
market_stock[, state := ifelse(
  state %in% list_state_suppress_by_cleaning,
  cell_spec(state, "latex", color = "blue"),
  state
)]

kable(market_stock[, .(state, installed_system, share_installation, cumulative_share)],
      format = "latex",
      col.names = c("States", "Installed System", "Share Installation", "Cumulative Share"),
      escape = FALSE) %>% 
  kable_classic_2() %>% 
  save_kable("output/tables/statdesc/share_obs_before_cleaning.tex")

# After Cleaning ----------------------------------------------------------

market_stock = ts_new[, .(installed_system = uniqueN(system_ID_1)), by = state]
market_stock[, share_installation := installed_system/3304253]
setorder(market_stock, -share_installation)
market_stock[, cumulative_share := cumsum(share_installation)]
market_stock[, state := toupper(state)]

kable(market_stock[, .(state, installed_system, share_installation, cumulative_share)],
      format = "latex",
      col.names = c("States", "Installed System", "Share Installation", "Cumulative Share"),
      escape = FALSE) %>% 
  kable_classic_2() %>% 
  save_kable("output/tables/statdesc/share_obs_before_cleaning.tex")

