# This code evaluate the quality variation impact on prices

library(arrow)
library(data.table)
library(fixest)

# Data --------------------------------------------------------------------

tts = setDT(read_parquet(data_final("tts_final.parquet")))

# Set up var --------------------------------------------------------------

tts[, net_price := price_w - rebate_w]
tts[, log_price := log(price_w)]
tts[, log_net_price := log(net_price )]
tts[, ln_tariff := log(tariff)]

# Regression --------------------------------------------------------------

feols(price_w ~ premium_panel_overall*china + premium_panel_overall*usa + premium_panel_overall*korea, fixef = c("year_quarter", "county"), cluster = ~zip_code, data = tts)
