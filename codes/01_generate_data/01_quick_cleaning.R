# This dataset propose a quick cleaning method for exploratory purpose only

library(arrow)
library(data.table)
library(ggplot2)

# Load Data ---------------------------------------------------------------

ts = read_parquet("data/1_raw/TTS.parquet")


