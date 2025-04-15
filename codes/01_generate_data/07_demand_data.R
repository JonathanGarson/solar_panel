# This code generates the demand data set

library(arrow)
library(data.table)

# Data --------------------------------------------------------------------

demand = read_parquet(data_final("tts_final.parquet"))

# 
