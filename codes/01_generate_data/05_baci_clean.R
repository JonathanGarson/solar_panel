# We generate one parquet file containing all trade year 2010-2020

library(data.table)
library(arrow)
library(glue)
library(stringr)

# Data --------------------------------------------------------------------

country_code = fread(data_raw("trade/BACI/country_codes_V202401b.csv"), select = c("country_code", "country_iso3"))

baci_files = c("trade/BACI/BACI_HS02_Y2010_V202401b.csv",
               "trade/BACI/BACI_HS02_Y2010_V202401b.csv",
               "trade/BACI/BACI_HS02_Y2011_V202401b.csv",
               "trade/BACI/BACI_HS02_Y2012_V202401b.csv",
               "trade/BACI/BACI_HS02_Y2013_V202401b.csv",
               "trade/BACI/BACI_HS02_Y2014_V202401b.csv",
               "trade/BACI/BACI_HS02_Y2015_V202401b.csv",
               "trade/BACI/BACI_HS02_Y2016_V202401b.csv",
               "trade/BACI/BACI_HS02_Y2017_V202401b.csv",
               "trade/BACI/BACI_HS02_Y2018_V202401b.csv",
               "trade/BACI/BACI_HS02_Y2019_V202401b.csv",
               "trade/BACI/BACI_HS02_Y2020_V202401b.csv")

# Cleaning function -------------------------------------------------------

clean_trade_data <- function(trade_file, country_code) {
  # Open Data
  trade = fread(data_raw(trade_file))
  
  # Collect year
  year_value <- str_extract(trade_file, "(?<=_Y)\\d{4}(?=_V)")
    
  # Change column names
  new_names <- c("year", "exporter", "importer", "code", "value", "quantity")
  setnames(trade, colnames(trade), new_names)
  
  # Select only solar panels (codes 854140 and 854150)
  solar_panel <- c(854140, 854150)
  trade <- trade[code %in% solar_panel]
  
  # Merge exporter country name and rename column to exporter_iso
  trade <- merge(trade, country_code, by.x = "exporter", by.y = "country_code", all.x = TRUE)
  setnames(trade, "country_iso3", "exporter_iso")
  
  # Merge importer country name and rename column to importer_iso
  trade <- merge(trade, country_code, by.x = "importer", by.y = "country_code", all.x = TRUE)
  setnames(trade, "country_iso3", "importer_iso")
  
  # Save in Parquet file
  write_parquet(trade, data_final(glue("trade/baci_{year_value}.parquet")))
}

# Cleaning Loop -----------------------------------------------------------
for (file in baci_files){
  clean_trade_data(file, country_code)
}

open_dataset(sources = data_final("trade/"), format = "parquet") %>% 
  write_parquet(data_final("trade/baci_2010_2020.parquet"))

rm(trade, country_code)
gc()
