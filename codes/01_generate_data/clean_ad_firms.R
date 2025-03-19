# This document clean the anti-dumping database we built based on the commerce department

library(data.table)

# Data --------------------------------------------------------------------

ad_2012 = fread(data_raw("tariff_ad/ad_china_2012.csv"))
ad_2015 = fread(data_raw("tariff_ad/ad_china_2015.csv"))

# Cleaning ----------------------------------------------------------------

# We want to be sure names match
new_column = c("case_id", "manufacturer", "ad_measure_firm", "case_itc", "notes")
setnames(ad_2015, colnames(ad_2015), new_column)
ad_2015 = ad_2015[, .(manufacturer, ad_measure_firm)]

# We get rid of white tray 
ad_2012[, manufacturer := trimws(manufacturer, which = "both")]
ad_2015[, manufacturer := trimws(manufacturer, which = "both")]

# We merge the dataset
ad_china = merge(ad_2012, ad_2015, by = "manufacturer")
