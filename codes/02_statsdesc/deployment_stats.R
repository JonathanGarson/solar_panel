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