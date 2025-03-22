# In this code we create the final TTS dataset for our regression:
# - We provide a clear procedure to identify and figure to justify the firm that we keep for our analysis
# - We build the key quality and adoption variable

library(arrow)
library(data.table)

# Data --------------------------------------------------------------------

tts = read_parquet(data_temp("TTS_merged.parquet"))

# Selecting firms ---------------------------------------------------------

# Market share of installation by brand approach
# -> keeping the biggest brands only

# Displaying the number of installation per model per year
# Displaying the number the quantity of model per year
# -> setting empirically a threshold for biggest sellers

# Setting different quality criteria
# Premium > 20% (maybe do it in function of quality)
# Maybe use a regression to determine the threshold for quality empirically (regress efficiency)
# SolarReview grades of brands
