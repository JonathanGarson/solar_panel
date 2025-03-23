# In this code we create the final TTS dataset for our regression:
# - We provide a clear procedure to identify and figure to justify the firm that we keep for our analysis
# - We build the key quality and adoption variable

library(arrow)
library(data.table)
library(ggplot2)

# Data --------------------------------------------------------------------

tts = setDT(read_parquet(data_temp("TTS_merged.parquet")))

# Selecting firms ---------------------------------------------------------

# Market share of installation by brand approach : we select the 15 biggest firms on the period, their market share is above 3%.

# Displaying the number of installation per model per year
# Displaying the number the quantity of model per year
# -> setting empirically a threshold for biggest sellers
# uniqueN(tts$module_model)
# tts[, sales_per_model := sum(module_quantity, na.rm = T) , by = c("module_model")]
# sales_2010_dt <- unique(tts[year == 2010, .(module_manufacturer,module_model, sales_per_model)])
# sales_2010_dt[, sum_year := sum(sales_per_model)]
# sales_2010_dt[, pct_sales := sales_per_model/sum_year]
# setorder(sales_2010_dt, cols = -pct_sales)
# sales_2010_dt[, cum_sum_pct_sales := cumsum(pct_sales)]
# 
# # Plot histogram of sales per model for 2010
# ggplot(sales_2010_dt, aes(x = sales_per_model)) +
#   geom_histogram(bins = 30, fill = "steelblue", color = "black") +
#   labs(
#     title = "Distribution of Sales per Model in 2010",
#     x = "Sales per Model",
#     y = "Frequency"
#   ) + 
#   theme_classic()

# We obtain a distribution of sells and see that:
# - above 2000 sales over the period we have 110 models that represents 90% of the market
# - above 3000 sales over the period we have  83 models that represent 85% of the market
# - above 10000 sales over the period we have 32 models that represent 65% of the market

# Setting different quality criteria --------------------------------------

# Premium > 20% 

# Combo inverter + high efficiency

# Maybe use a regression to determine the threshold for quality empirically (regress efficiency)
# SolarReview grades of brands
