# This document clean the anti-dumping database we built based on the commerce department

library(data.table)

# Data --------------------------------------------------------------------

ad_2012 = fread(data_raw("tariff_ad/ad_china_2012.csv"))
ad_2015 = fread(data_raw("tariff_ad/ad_china_2015.csv"))

# Cleaning ----------------------------------------------------------------

# We want to be sure names match
new_column = c("case_id", "manufacturer", "ad_measure_firm", "case_itc", "notes", "alt_name_manufacturer")
setnames(ad_2015, colnames(ad_2015), new_column)
ad_2015 = ad_2015[, .(alt_name_manufacturer, ad_measure_firm)]

# We get rid of white tray 
ad_2012[, manufacturer_alt := trimws(alt_name_manufacturer, which = "both")]
ad_2015[, manufacturer_alt := trimws(alt_name_manufacturer, which = "both")]

# We limit our data to the targeted Chinese firms which represent a substantial part of the Chinese presence in the U.S.
ad_2012[, manufacturer_alt := fifelse(trimws(manufacturer_alt) == "", NA_character_, manufacturer_alt)]
ad_2012 = ad_2012[!is.na(manufacturer_alt),]
ad_2015[, manufacturer_alt := fifelse(trimws(manufacturer_alt) == "", NA_character_, manufacturer_alt)]
ad_2015 = ad_2015[!is.na(manufacturer_alt),]

# We add yearly component to variable names
new_names_2012 = paste0(colnames(ad_2012), "_2012")
new_names_2015 = paste0(colnames(ad_2015), "_2015")

setnames(ad_2012, colnames(ad_2012), new_names_2012)
setnames(ad_2015, colnames(ad_2015), new_names_2015)

# We merge the dataset
ad_china = merge(ad_2012, ad_2015, by.x = "manufacturer_alt_2012", by.y = "manufacturer_alt_2015")

# Cleaning the last dataset
new_names = c("manufacturer","manufacturer_2012","dumping_margin_2012","cash_deposit_rate_2012","final_subsidy_2012",
              "alt_name_manufacturer_2012", "alt_name_manufacturer_2015", "ad_measure_firm_2015")
setnames(ad_china, colnames(ad_china), new_names)

ad_china[, manufacturer_low := tolower(manufacturer)]
ad_china = ad_china[, .(manufacturer, manufacturer_low,dumping_margin_2012,cash_deposit_rate_2012,final_subsidy_2012, ad_measure_firm_2015)]

fwrite(ad_china, data_final("ad/ad_china_short.csv"))
