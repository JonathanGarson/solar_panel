# This code compares 2012 and 2015 firms that are affected by the tariff for the biggest chinese firms and produce key description

library(arrow)
library(data.table)
library(gt)

# Data --------------------------------------------------------------------

ad_china = fread(data_final("ad/ad_china_short.csv"))
firms = read_parquet(data_temp('sales_year_brand.parquet'))
ad_2015_firms_names = fread(data_raw("tariff_ad/ad_china_2015.csv"), select = c("F_FIRM"))

# Comparing 2012 2015 AD --------------------------------------------------

comparison = ad_china[, .(manufacturer, dumping_margin_2012, ad_measure_firm_2015)]
comparison[, ad_measure_firm_2015 := as.numeric(gsub(",", ".", ad_measure_firm_2015))]
comparison[, difference := dumping_margin_2012 - as.numeric(ad_measure_firm_2015)]

setnames(comparison, colnames(comparison), c("Manufacturer", "Dumping Margin 2012", "Dumping Margin 2015", "Difference"))

comparison_table = gt(comparison) %>%
  as_latex() %>% 
  as.character()
writeLines(comparison_table,"output/tables/statdesc/AD_2012_2015_comparison.tex")

# Share Chinese Firms -----------------------------------------------------
# Copy the original firms dataset
china_firms = copy(firms)

# Compute total yearly sales
china_firms[, sell_year := sum(brand_sales_year, na.rm = TRUE), by = "year"]

# Compute share of each firm's sales in the total yearly sales
china_firms[, share_sell := brand_sales_year / sell_year, by = "year"]

# Define top Chinese firms
top_chinese_firms = c("canadian solar", "ja solar", "jinko solar", "suntech power", "trina solar", "yingli energy (china)")

# Define all Chinese firms affected by AD
list_ad = c("trina solar", "wuxi suntech power co., ltd", "suntech power", "baoding tianwei solarfilms", "yingli energy (china)",
            "tianwei new energy (yangzhou)", "tianwei new energy (chengdu) pv module", "tianwei new energy holdings","canadian solar",
            "hanwha solarone hong kong","ldk solar", "changzhou nesl solartech","china sunergy","chint solar (zhejiang) co., ltd.",
            "suzhou talesun solar technologies co., ltd.","tenksolar","upsolar","jinko solar", "cnpv dongying solar power",
            "csg pvtech","delsolar","eoplly new energy technology","era solar","et solar industry","et solar new energy", "ja solar",
            "jetion solar (china)","jiangsu green power pv","jingao solar", "lightway green new energy", "motech industries",
            "ningbo qixin solar electrical appliance","tianwei new energy holdings", "ningbo qixin solar electrical appliance",
            "ningbo ulica solar science & technology", "perlight solar", "risen energy co., ltd.","byd", "shanghai chaori solar energy science & technology",
            "solarbest energy-tech (zhejiang)","sopray energy", "sun earth solar power", "zhejiang jiutai new energy", "zhejiang sunflower light energy science & technology")

# Compute total sales of all affected Chinese firms by year
china_firms[module_manufacturer %in% list_ad, sum_sell_ad_chinese := sum(brand_sales_year, na.rm = TRUE), by = "year"]

# Compute share of affected Chinese firms in total yearly sales
china_firms[, share_sell_ad_chinese := sum_sell_ad_chinese / sell_year, by = "year"]

# Compute total sales of top Chinese firms among the affected ones by year
china_firms[module_manufacturer %in% top_chinese_firms, sum_sell_top_chinese := sum(brand_sales_year, na.rm = TRUE), by = "year"]

# Compute share of top Chinese firms within the affected Chinese firms' sales
china_firms[, share_top_in_ad := sum_sell_top_chinese / sum_sell_ad_chinese, by = "year"]

# Keep only one row per year for final display
china_firms_summary = china_firms[, .(year, share_sell_ad_chinese, share_top_in_ad)]
china_firms_summary = unique(china_firms_summary)
china_firms_summary = china_firms_summary[year %in% 2010:2020 & !is.na(share_sell_ad_chinese) & !is.na(share_top_in_ad),]

setnames(china_firms_summary, colnames(china_firms_summary), c("Year", "Overall Share Chinese Firms", "Top 5 Chinese Firms"))
setorder(china_firms_summary, cols = "Year")

china_firm_latex = gt(china_firms_summary) %>% 
  as_latex() %>% 
  as.character()
writeLines(china_firm_latex, "output/tables/statdesc/top5_chinese_overall.tex")
