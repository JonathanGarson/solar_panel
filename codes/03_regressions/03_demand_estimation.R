# We estimate the demand

library(arrow)
library(data.table)
library(fixest)
library(modelsummary)
library(ggplot2)

# Data --------------------------------------------------------------------

tts = setDT(read_parquet(data_final("tts_final.parquet")))

# OLS - Demand Analysis ---------------------------------------------------
tts[,net_price := 0.7(price_w-rebate_w)]
tts[,net_price_sq := net_price^2]
tts[,price_w_sq := price_w^2]
tts[,tariff_sq := tariff^2]
tts = tts[!is.na(price_w) & !is.na(h_median)]
tts = tts[price_w > 1 & price_w < 10,]
tts = tts[rebate_w < price_w]

demand_ols = feols(demand_zip_code ~ net_price + net_price^2 + PV_system_size_DC + PV_system_size_DC^2 + elec_price | year + county + installer_name + origin, cluster = ~zip_code, data = tts)
demand_pois = fepois(demand_zip_code ~ price_w + price_w^2 + PV_system_size_DC + PV_system_size_DC^2 + elec_price | year + county + installer_name + origin, cluster = ~zip_code, data = tts)
# demand_iv = feols(demand_zip_code ~ PV_system_size_DC + PV_system_size_DC^2 + elec_price | year + county + installer_name + origin | price_w + price_w_sq ~ tariff, cluster = ~zip_code, 
#                   data = tts[year %in% 2012:2018] )
# 
# 
# # First-stage: price_w ~ tariff
# fs1 <- feols(price_w ~ tariff + tariff_sq | year + county + installer_name + origin, 
#              cluster = ~zip_code, data = tts)
# summary(fs1)
# 
# # First-stage: price_w_sq ~ tariff
# fs2 <- feols(price_w_sq ~ tariff + tariff_sq | year + county + installer_name + origin, 
#              cluster = ~zip_code, data = tts)
# summary(fs2)

iv_model <- feols(
  demand_zip_code ~ PV_system_size_DC + I(PV_system_size_DC^2) | 
    year + county + installer_name + origin | 
    net_price + net_price_sq ~ rebate_w + rebate_w^2, 
  cluster = ~zip_code, 
  data = tts[year %in% 2010:2018],
  data.save = TRUE
)

iv_data = setDT(iv_model$data)

# Add fitted values to the dataset
iv_data[, predicted_demand := predict(iv_model)]

# Plot predicted demand vs price_w
# ggplot(iv_data, aes(x = price_w, y = predicted_demand)) +
#   geom_point(size = 0.2) +
#   geom_smooth(method = "loess", se = FALSE, color = "black", size = 1.2) +
#   labs(title = "Predicted Demand vs. Price (Binned Density)",
#        x = "Price (price_w)",
#        y = "Predicted Demand") +
#   theme_minimal()

set.seed(123)  # for reproducibility
iv_sample <- iv_data[sample(.N, 10000)]

ggplot() +
  geom_point(data = iv_sample, aes(x = price_w, y = predicted_demand), alpha = 0.2) +
  geom_smooth(data = iv_data, aes(x = price_w, y = predicted_demand),
              method = "gam", se = TRUE, size = 1.2) +
  labs(title = "Predicted Demand vs. Price (Sampled Points)",
       x = "Price (price_w)",
       y = "Predicted Demand") +
  theme_classic()
