# This code evaluate the subsidy pass-through

library(arrow)
library(data.table)
library(fixest)
library(modelsummary)

# Data --------------------------------------------------------------------

tts= read_parquet(data_final("TTS_final.parquet"))

# The effect of subsidy on price ------------------------------------------

tts[, post_incentive_price_w := price_w - rebate_w]
test = tts[PV_system_size_DC <= 10,]

rebate_pt = feols(post_incentive_price_w ~ rebate_w*quality_1 + rebate_w*quality_2| year + state + module_manufacturer + installer_name, cluster = ~zip_code, data = test[ho == 1])
fitstat(rebate_pt, type = c("f", "wald"))



