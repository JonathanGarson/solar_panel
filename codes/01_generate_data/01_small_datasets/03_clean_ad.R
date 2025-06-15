# This document clean the anti-dumping database we built based on the commerce department

library(data.table)
library(lubridate)

# Data --------------------------------------------------------------------

ad_2012 = fread(data_raw("tariff_ad/AD_2012.csv"))
ad_2015 = fread(data_raw("tariff_ad/CV_AD_2015.csv"))

# Cleaning ----------------------------------------------------------------
# low cap columns and year flag

ad12_names = colnames(ad_2012)
setnames(ad_2012, ad12_names, tolower(paste0(ad12_names,"_2012")))
setnames(ad_2012, "manufacturer_2012", "module_manufacturer_2012")

ad15_names = colnames(ad_2015)
setnames(ad_2015, ad15_names, tolower(paste0(ad15_names,"_2015")))
setnames(ad_2015, "manufacturer_2015", "module_manufacturer_2015")

# We want to clean the Chinese brand names so that they match with the main dataset
ad_2012[module_manufacturer_2012 == "Changzhou Trina Solar Energy Co. Ltd.", module_manufacturer_2012 := "trina solar"]
ad_2012[module_manufacturer_2012 == "Trina Solar (Changzhou) Science & Technology Co. Ltd", module_manufacturer_2012 := "trina solar"]
ad_2012[module_manufacturer_2012 == "Canadian Solar International Limited", module_manufacturer_2012 := "canadian solar"]
ad_2012[module_manufacturer_2012 == "Canadian Solar Manufacturing (Luoyang) Inc", module_manufacturer_2012 := "canadian solar"]
ad_2012[module_manufacturer_2012 == "Canadian Solar Manufacturing (Changshu) Inc", module_manufacturer_2012 := "canadian solar"]
ad_2012[module_manufacturer_2012 == "JinkoSolar International Limited & Jinko Solar Co. Ltd", module_manufacturer_2012 := "jinko solar"]
ad_2012[module_manufacturer_2012 == "Jinko Solar Import and Export Co. Ltd & Jinko Solar Co. Ltd", module_manufacturer_2012 := "jinko solar"]
ad_2012[module_manufacturer_2012 == "Yingli Energy (China) Company Limited", module_manufacturer_2012 := "yingli energy (china)"]
ad_2012[module_manufacturer_2012 == "Yingli Energy (China) Company Limited", module_manufacturer_2012 := "yingli energy (china)"]
ad_2012[module_manufacturer_2012 == "Baoding Tianwei Yingli New Energy Resources Co. Ltd", module_manufacturer_2012 := "yingli energy (china)"]
ad_2012[module_manufacturer_2012 == "Suntech Power Co. Ltd", module_manufacturer_2012 := "suntech power"]


ad_2015[module_manufacturer_2015 == "Changzhou Trina Solar Energy Co. Ltd.", module_manufacturer_2015 := "trina solar"]
ad_2015[module_manufacturer_2015 == "Trina Solar (Changzhou) Science & Technology Co. Ltd", module_manufacturer_2015 := "trina solar"]
ad_2015[module_manufacturer_2015 == "Canadian Solar International Limited", module_manufacturer_2015 := "canadian solar"]
ad_2015[module_manufacturer_2015 == "Canadian Solar Manufacturing (Luoyang) Inc", module_manufacturer_2015 := "canadian solar"]
ad_2015[module_manufacturer_2015 == "Canadian Solar Manufacturing (Changshu) Inc", module_manufacturer_2015 := "canadian solar"]
ad_2015[module_manufacturer_2015 == "JinkoSolar International Limited & Jinko Solar Co. Ltd", module_manufacturer_2015 := "jinko solar"]
ad_2015[module_manufacturer_2015 == "Jinko Solar Import and Export Co. Ltd & Jinko Solar Co. Ltd", module_manufacturer_2015 := "jinko solar"]
ad_2015[module_manufacturer_2015 == "Renesola Jiangsu Ltd./Renesola Zhejiang Ltd./Jinko Solar Co. Ltd./Jinko Solar Import and Export Co., Ltd", module_manufacturer_2015 := "jinko solar"]
ad_2015[module_manufacturer_2015 == "Yingli Energy (China) Company Limited", module_manufacturer_2015 := "yingli energy (china)"]
ad_2015[module_manufacturer_2015 == "Yingli Energy (China) Company Limited", module_manufacturer_2015 := "yingli energy (china)"]
ad_2015[module_manufacturer_2015 == "Baoding Tianwei Yingli New Energy Resources Co. Ltd", module_manufacturer_2015 := "yingli energy (china)"]
ad_2015[module_manufacturer_2015 == "Baoding Tianwei Yingli New Energy Resources Co. , Ltd", module_manufacturer_2015 := "yingli energy (china)"]
ad_2015[module_manufacturer_2015 == "Wuxi Suntech Power Co., Ltd", module_manufacturer_2015 := "suntech power"]

# We manually add Renesolar
ad_2015 <- rbind(ad_2015, data.table(
    module_manufacturer_2015 = "renesola", ad_rate_2015 = 78.42,cvd_rate_2015 = 38.43, ad_temp_measure_2015 = "31/07/2014", cvd_temp_measure_2015 = "10/06/2014", cvd_temp_end_2015 = "08/10/2014"))

# Format Date
ad_2012[, p_dump_date_2012_fmt := dmy(p_dump_date_2012)]
ad_2012[, year_quarter := paste0(year(p_dump_date_2012_fmt), "Q", quarter(p_dump_date_2012_fmt))]

ad_2015[, ad_temp_measure_2015_fmt := dmy(ad_temp_measure_2015)]
ad_2015[, year_quarter := paste0(year(ad_temp_measure_2015_fmt), "Q", quarter(ad_temp_measure_2015_fmt))]

# Transform tariff to numeric
ad_2012[, ad_rate_2012 := as.numeric(gsub(",", ".", ad_rate_2012))]
ad_2012[, cvd_rate_2012 := as.numeric(gsub(",", ".", cvd_rate_2012))]
ad_2015[, ad_rate_2015 := as.numeric(gsub(",", ".", ad_rate_2015))]

# Export ------------------------------------------------------------------
ad_2012_final = unique(ad_2012)
ad_2015_final = unique(ad_2015)
fwrite(ad_2012_final, data_final("ad_2012_final.csv"))
fwrite(ad_2015_final, data_final("ad_2015_final.csv"))
