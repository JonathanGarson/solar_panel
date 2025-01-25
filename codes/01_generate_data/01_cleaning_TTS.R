# This dataset propose a quick cleaning method for exploratory purpose only

library(arrow)
library(data.table)
library(ggplot2)

# Load Data ---------------------------------------------------------------

ts = read_parquet(data_raw("TTS.parquet"))

# Data Cleaning -----------------------------------------------------------
#getting rid of variables concerning the orientation of the solar panel
cons_column = setdiff(colnames(ts), c("azimuth_1","azimuth_2","azimuth_3","tilt_1","tilt_2","tilt_3",
                                      "data_provider_1","data_provider_2","new_construction","tracking"))
ts[, .SD, .SDcols = cons_column]

#getting rid of observations with no information for: installer, zip, manafucturer, module model, customer segment 
string_filter = c("installation_date", "zip_code","installer_name", "module_manufacturer_1",
                  "module_manufacturer_2", "module_manufacturer_3",
                  "module_model_1", "technology_module_1")
num_filter = c("efficiency_module_1", "nameplate_capacity_module_1")

ts_f = ts[apply(ts[, ..string_filter] != "-1", 1, all)]
ts_f = ts_f[apply(ts[, ..num_filter] != -1, 1, all)]
ts_f = ts_f[installation_date != "NaT",]

# cleaning up memory 
rm(ts) 
gc()

#adding a year column
ts_f[, year := as.numeric(substr(installation_date, 8, 11))]
 
unique(ts_f$module_manufacturer_2)
#list of manufacturer to merge
ts_f[module_manufacturer_1 == "BYD (Huizhou) Battery", module_manufacturer_1 := "BYD"]
ts_f[module_manufacturer_1 == "BYD Company Limited", module_manufacturer_1 := "BYD"]
ts_f[module_manufacturer_1 == "CSI Solar Co., Ltd. ", module_manufacturer_1 := "Canadian Solar"]
ts_f[module_manufacturer_1 == "CSI Solar Co., Ltd.", module_manufacturer_1 := "Canadian Solar"]
ts_f[module_manufacturer_1 == "Canadian Solar Inc.", module_manufacturer_1 := "Canadian Solar"]
ts_f[module_manufacturer_1 == "China Sunergy (Nanjing) Co.,Ltd.", module_manufacturer_1 := "China Sunergy"]
ts_f[module_manufacturer_1 == "CSUN Eurasia Energy Systems Industry and Trade", module_manufacturer_1 := "CSUN"]
ts_f[module_manufacturer_1 == "Dehui Solar Power (Vietnam) Co.,Ltd DH-M860W 325W", module_manufacturer_1 := "Dehui"]
ts_f[module_manufacturer_1 == "Dehui Solar Power (Vietnam) Co.,Ltd", module_manufacturer_1 := "Dehui"]
ts_f[module_manufacturer_1 == "DEHUI SOLAR POWER INC.", module_manufacturer_1 := "Dehui"]
ts_f[module_manufacturer_1 == "First Solar, Inc. ", module_manufacturer_1 := "First Solar, Inc."]
ts_f[module_manufacturer_1 == "Guangdong Golden Glass Technologies GG200M2-27/1482x992", module_manufacturer_1 := "Guangdong Golden Glass Technologies"]
ts_f[module_manufacturer_1 == "Heliene 72M-400 G1 Bifacial", module_manufacturer_1 := "Heliene"]
ts_f[module_manufacturer_1 == "JA Solar JAM72D09-370", module_manufacturer_1 := "JA Solar"]
ts_f[module_manufacturer_1 == "Jinko Solar Co., Ltd", module_manufacturer_1 := "Jinko Solar"]
ts_f[module_manufacturer_1 == "MAGE Solar", module_manufacturer_1 := "Mage Solar"]
ts_f[module_manufacturer_1 == "Mage Solar USA", module_manufacturer_1 := "Mage Solar"]
ts_f[module_manufacturer_1 == "Mage Solar USA", module_manufacturer_1 := "Mage Solar"]
ts_f[module_manufacturer_1 == "Maxeon Solar Technologies Ltd.", module_manufacturer_1 := "Maxeon - SunPower"] # non trivial here because Maxeon becomes a Singapore based company in 2020
ts_f[module_manufacturer_1 == "SunPower", module_manufacturer_1 := "Maxeon - SunPower"] 
ts_f[module_manufacturer_1 == "Panasonic Corporation of North America", module_manufacturer_1 := "Panasonic"] 
ts_f[module_manufacturer_1 == "SANYO ELECTRIC CO LTD OF PANASONIC GROUP", module_manufacturer_1 := "Panasonic"]
ts_f[module_manufacturer_1 == "Panasonic Eco Solutions Canada", module_manufacturer_1 := "Panasonic"] 
ts_f[module_manufacturer_1 == "Qcells North America", module_manufacturer_1 := "Hanwha QCells"] #American capacities of Hanwha
ts_f[module_manufacturer_1 == "Q-Cells North America", module_manufacturer_1 := "Hanwha QCells"]
ts_f[module_manufacturer_1 == "Hanwha Qcells (Qidong) Co. Ltd.", module_manufacturer_1 := "Hanwha QCells (Qidong)"]
ts_f[module_manufacturer_1 == "Q-Cells Q.Pro G2 250", module_manufacturer_1 := "Hanwha QCells"]
ts_f[module_manufacturer_1 == "Hanwha Q-Cells", module_manufacturer_1 := "Hanwha QCells"]
ts_f[module_manufacturer_1 == "Hanwha Q-Cells Q.PEAK DUO BLK-G6 330", module_manufacturer_1 := "Hanwha QCells"]
ts_f[module_manufacturer_1 == "Hanwha Qcells (Qidong) Co. Ltd. Q.PRIME L-G5.1.G 340", module_manufacturer_1 := "Hanwha QCells (Quidong)"]
ts_f[module_manufacturer_1 == "Qcells North America Q.PEAK DUO BLK-G5 290", module_manufacturer_1 := "Hanwha QCells"]
ts_f[module_manufacturer_1 == "Hanwha Solar Canada", module_manufacturer_1 := "Hanwha QCells"]
ts_f[module_manufacturer_1 == "Hanwha Solar Canada", module_manufacturer_1 := "Hanwha QCells"]



