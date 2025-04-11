# We use the decline in rebate as an IV variation

library(arrow)
library(data.table)
library(fixest)
library(ggplot2)

# Data --------------------------------------------------------------------

tts = read_parquet(data_final("tts_final.parquet"))

# IV Reduction tariff
