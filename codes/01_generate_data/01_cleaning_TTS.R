# This code clean the companies names and avoid to lose observations for mislabelling issues.

gc()

library(arrow)
library(data.table)
library(ggplot2)
library(zoo)

# Load Data ---------------------------------------------------------------

ts = as.data.table(read_parquet(data_raw("TTS.parquet")))

# Data Cleaning -----------------------------------------------------------

## Getting rid of data on physical orientation -----------------------------
#getting rid of variables concerning the orientation of the solar panel
cons_column = setdiff(colnames(ts), c("azimuth_1","azimuth_2","azimuth_3","tilt_1","tilt_2","tilt_3",
                                      "data_provider_1","data_provider_2","new_construction","tracking"))
ts[, .SD, .SDcols = cons_column]


## Getting rid of missing values -------------------------------------------
#getting rid of observations with no information for: installer, zip, manafucturer, module model, customer segment
NA_char_cols = c("installation_date", "zip_code","installer_name", 
                "module_manufacturer_1", "module_manufacturer_2", "module_manufacturer_3",
                "module_model_1", "module_model_2", "module_model_3", 
                "technology_module_1", "technology_module_2", "technology_module_3")
ts[, (NA_char_cols) := lapply(.SD, function(x) fifelse(x == "-1", NA_character_, x)), .SDcols = NA_char_cols]

NA_int_cols = c("efficiency_module_1", "efficiency_module_2", "efficiency_module_3",
                "nameplate_capacity_module_1", "nameplate_capacity_module_2", "nameplate_capacity_module_3",
                "module_quantity_1", "module_quantity_2", "module_quantity_3",
                "PV_system_size_DC")
ts[, (NA_int_cols) := lapply(.SD, function(x) fifelse(x == -1, NA_integer_, x)), .SDcols = NA_int_cols]

#dropping na
na_cols = c("installation_date", "zip_code","installer_name", "module_manufacturer_1", 
            "technology_module_1","efficiency_module_1","nameplate_capacity_module_1",
            "module_quantity_1", "PV_system_size_DC", "rebate_or_grant")
ts = na.omit(ts, cols = na_cols)

## Adding time columns -----------------------------------------------------
ts[, year := as.numeric(substr(installation_date, 8, 11))] #adding a year column
ts[, installation_date := as.Date(installation_date, format = "%d-%b-%Y")]
ts[, year_quarter := as.yearqtr(installation_date, format = "%Y-%m-%d")]

# ts[, year_quarter := fcase(
#   substr(installation_date, 4, 6) %in% c("Jan","Feb", "Mar"), paste0(substr(installation_date, 8, 11), "q1"),
#   substr(installation_date, 4, 6) %in% c("Apr","May", "Jun"), paste0(substr(installation_date, 8, 11), "q2"),
#   substr(installation_date, 4, 6) %in% c("Jul","Aug", "Sep"), paste0(substr(installation_date, 8, 11), "q3"),
#   substr(installation_date, 4, 6) %in% c("Oct","Nov", "Dec"), paste0(substr(installation_date, 8, 11), "q4"),
#   default = NA_character_
# )]

## Merging data for module 1 manufacturer ----------------------------------
#list of manufacturer to merge for module 1
ts[module_manufacturer_1 == "Atlantis Energy Systems", module_manufacturer_1 := "Atlantis Energy"]
ts[module_manufacturer_1 == "BYD (Huizhou) Battery", module_manufacturer_1 := "BYD"]
ts[module_manufacturer_1 == "BYD Company Limited", module_manufacturer_1 := "BYD"]
ts[module_manufacturer_1 == "CSI Solar Co., Ltd. ", module_manufacturer_1 := "Canadian Solar"]
ts[module_manufacturer_1 == "CSI Solar Co., Ltd.", module_manufacturer_1 := "Canadian Solar"]
ts[module_manufacturer_1 == "Canadian Solar Inc.", module_manufacturer_1 := "Canadian Solar"]
ts[module_manufacturer_1 == "Canadian Solar Inc. CS5A-160MX", module_manufacturer_1 := "Canadian Solar"]
ts[module_manufacturer_1 == "China Sunergy (Nanjing) Co.,Ltd.", module_manufacturer_1 := "China Sunergy"]
ts[module_manufacturer_1 == "China Sunergy (Nanjing)", module_manufacturer_1 := "China Sunergy"]
ts[module_manufacturer_1 == "CSUN Eurasia Energy Systems Industry and Trade", module_manufacturer_1 := "CSUN"]
ts[module_manufacturer_1 == "Chint Solar (Zhejiang) Co., Ltd. ", module_manufacturer_1 := "Chint Solar (Zhejiang) Co., Ltd."]
ts[module_manufacturer_1 == "Dehui Solar Power (Vietnam) Co.,Ltd DH-M860W 325W", module_manufacturer_1 := "Dehui"]
ts[module_manufacturer_1 == "Dehui Solar Power (Vietnam) Co.,Ltd", module_manufacturer_1 := "Dehui"]
ts[module_manufacturer_1 == "Dehui Solar Power (Vietnam) Co.,Ltd ", module_manufacturer_1 := "Dehui"]
ts[module_manufacturer_1 == "DEHUI SOLAR POWER INC.", module_manufacturer_1 := "Dehui"]
ts[module_manufacturer_1 == "First Solar, Inc. ", module_manufacturer_1 := "First Solar, Inc."]
ts[module_manufacturer_1 == "Guangdong Golden Glass Technologies GG200M2-27/1482x992", module_manufacturer_1 := "Guangdong Golden Glass Technologies"]
ts[module_manufacturer_1 == "Heliene 72M-400 G1 Bifacial", module_manufacturer_1 := "Heliene"]
ts[module_manufacturer_1 == "JA Solar JAM72D09-370", module_manufacturer_1 := "JA Solar"]
ts[module_manufacturer_1 == "Jinko Solar Co., Ltd", module_manufacturer_1 := "Jinko Solar"]
ts[module_manufacturer_1 == "Japan Solar (Infini Co., Ltd.)", module_manufacturer_1 := "Japan Solar"]
ts[module_manufacturer_1 == "Japan Solar (Infini Co., Ltd.) ", module_manufacturer_1 := "Japan Solar"]
ts[module_manufacturer_1 == "LONGi Green Energy Technology Co., Ltd. ", module_manufacturer_1 := "LONGi Green Energy Technology Co., Ltd."]
ts[module_manufacturer_1 == "LG", module_manufacturer_1 := "LG Electronics Inc."]
ts[module_manufacturer_1 == "LG Electronics", module_manufacturer_1 := "LG Electronics Inc."]
ts[module_manufacturer_1 == "MAGE Solar", module_manufacturer_1 := "Mage Solar"]
ts[module_manufacturer_1 == "Mage Solar USA", module_manufacturer_1 := "Mage Solar"]
ts[module_manufacturer_1 == "Mage Solar USA", module_manufacturer_1 := "Mage Solar"]
ts[module_manufacturer_1 == "Maxeon Solar Technologies Ltd.", module_manufacturer_1 := "Maxeon - SunPower"] # non trivial here because Maxeon becomes a Singapore based company in 2020
ts[module_manufacturer_1 == "SunPower", module_manufacturer_1 := "Maxeon - SunPower"] 
ts[module_manufacturer_1 == "Motech Americas", module_manufacturer_1 := "Motech Industries"] 
ts[module_manufacturer_1 == "Panasonic Corporation of North America", module_manufacturer_1 := "Panasonic"] 
ts[module_manufacturer_1 == "SANYO ELECTRIC CO LTD OF PANASONIC GROUP", module_manufacturer_1 := "Panasonic"]
ts[module_manufacturer_1 == "SANYO ELECTRIC CO LTD OF PANASONIC GROUP HIT-N210A01", module_manufacturer_1 := "Panasonic"]
ts[module_manufacturer_1 == "Sanyo Electric Co Ltd of Panasonic Group", module_manufacturer_1 := "Panasonic"]
ts[module_manufacturer_1 == "SANYO ELECTRIC CO LTD OF PANASONIC GROUP HIT-N190A03", module_manufacturer_1 := "Panasonic"]
ts[module_manufacturer_1 == "SANYO ELECTRIC CO LTD OF PANASONIC GROUP VBHN210AA01", module_manufacturer_1 := "Panasonic"]
ts[module_manufacturer_1 == "SANYO ELECTRIC CO LTD OF PANASONIC GROUP HIT-N214A01", module_manufacturer_1 := "Panasonic"]
ts[module_manufacturer_1 == "SANYO ELECTRIC CO LTD OF PANASONIC GROUP HIT-N215A01", module_manufacturer_1 := "Panasonic"]
ts[module_manufacturer_1 == "SANYO ELECTRIC CO LTD OF PANASONIC GROUP HIT-N220A01", module_manufacturer_1 := "Panasonic"]
ts[module_manufacturer_1 == "Panasonic Eco Solutions Canada", module_manufacturer_1 := "Panasonic"]
ts[module_manufacturer_1 == "Phono Solar Technology Co., Ltd.", module_manufacturer_1 := "Phono Solar Technology Co., Ltd."]
ts[module_manufacturer_1 == "Qcells North America", module_manufacturer_1 := "Hanwha QCells"] #American capacities of Hanwha
ts[module_manufacturer_1 == "Q-Cells North America", module_manufacturer_1 := "Hanwha QCells"]
ts[module_manufacturer_1 == "Hanwha Qcells (Qidong) Co. Ltd.", module_manufacturer_1 := "Hanwha QCells"]
ts[module_manufacturer_1 == "Hanwha Qcells (Qidong) Co. Ltd. Q.PRIME L-G5.1.G 335", module_manufacturer_1 := "Hanwha QCells"]
ts[module_manufacturer_1 == "Q-Cells Q.Pro G2 250", module_manufacturer_1 := "Hanwha QCells"]
ts[module_manufacturer_1 == "Hanwha Q-Cells Q.PEAK BLK G4.1 295", module_manufacturer_1 := "Hanwha QCells"]
ts[module_manufacturer_1 == "Hanwha Q-Cells", module_manufacturer_1 := "Hanwha QCells"]
ts[module_manufacturer_1 == "Hanwha Q-Cells Q.PEAK DUO BLK-G6 330", module_manufacturer_1 := "Hanwha QCells"]
ts[module_manufacturer_1 == "Hanwha SolarOne (Qidong)", module_manufacturer_1 := "Hanwha QCells"]
ts[module_manufacturer_1 == "Hanwha Qcells (Qidong) Co. Ltd. Q.PRIME L-G5.1.G 340", module_manufacturer_1 := "Hanwha QCells"]
ts[module_manufacturer_1 == "Qcells North America Q.PEAK DUO BLK-G5 290", module_manufacturer_1 := "Hanwha QCells"]
ts[module_manufacturer_1 == "Hanwha Solar Canada", module_manufacturer_1 := "Hanwha QCells"]
ts[module_manufacturer_1 == "Hyundai Heavy Industries", module_manufacturer_1 := "HYUNDAI ENERGY SOLUTIONS CO., LTD."]
ts[module_manufacturer_1 == "HD HYUNDAI ENERGY SOLUTIONS CO., LTD. ", module_manufacturer_1 := "HYUNDAI ENERGY SOLUTIONS CO., LTD."]
ts[module_manufacturer_1 == "REC", module_manufacturer_1 := "REC Solar"]
ts[module_manufacturer_1 == "REC ScanModule", module_manufacturer_1 := "REC Solar"]
ts[module_manufacturer_1 == "ReneSola", module_manufacturer_1 := "Renesola America"]
ts[module_manufacturer_1 == "Renesola Jiangsu", module_manufacturer_1 := "Renesola America"]
ts[module_manufacturer_1 == "Sharp Electronics", module_manufacturer_1 := "Sharp"]
ts[module_manufacturer_1 == "Sharp ND-123UJF", module_manufacturer_1 := "Sharp"]
ts[module_manufacturer_1 == "SolarWorld Industries GmbH", module_manufacturer_1 := "SolarWorld"]
ts[module_manufacturer_1 == "SolarWorld Americas Inc", module_manufacturer_1 := "SolarWorld"]
ts[module_manufacturer_1 == "SolarWorld SW250 Mono", module_manufacturer_1 := "SolarWorld"]
ts[module_manufacturer_1 == "Silfab Solar Inc.", module_manufacturer_1 := "Silfab"]
ts[module_manufacturer_1 == "Solaria Corporation", module_manufacturer_1 := "Solaria"]
ts[module_manufacturer_1 == "Siliken Manufacturing USA", module_manufacturer_1 := "Siliken"]
ts[module_manufacturer_1 == "Siliken Canada", module_manufacturer_1 := "Siliken"]
ts[module_manufacturer_1 == "Suntech", module_manufacturer_1 := "Suntech Power"]
ts[module_manufacturer_1 == "Tesla Inc.", module_manufacturer_1 := "Tesla"]
ts[year >= 2016 & module_manufacturer_1 == "SolarCity Corp", module_manufacturer_1 := "Tesla"]
ts[module_manufacturer_1 == "SolarJuice Technology Inc.", module_manufacturer_1 := "SolarJuice"]
ts[module_manufacturer_1 == "SolarJuice American Inc.", module_manufacturer_1 := "SolarJuice"]
ts[module_manufacturer_1 == "Znshine PV-Tech", module_manufacturer_1 := "Znshine"]
ts[module_manufacturer_1 == "ZNSHINE PV-TECH Co., Ltd.", module_manufacturer_1 := "Znshine"]

## Merging data for module 2 manufacturer ----------------------------------
#list of manufacturer to merge for module 2
ts[module_manufacturer_2 == "Atlantis Energy Systems", module_manufacturer_2 := "Atlantis Energy"]
ts[module_manufacturer_2 == "BYD (Huizhou) Battery", module_manufacturer_2 := "BYD"]
ts[module_manufacturer_2 == "BYD Company Limited", module_manufacturer_2 := "BYD"]
ts[module_manufacturer_2 == "CSI Solar Co., Ltd. ", module_manufacturer_2 := "Canadian Solar"]
ts[module_manufacturer_2 == "CSI Solar Co., Ltd.", module_manufacturer_2 := "Canadian Solar"]
ts[module_manufacturer_2 == "Canadian Solar Inc.", module_manufacturer_2 := "Canadian Solar"]
ts[module_manufacturer_2 == "Canadian Solar Inc. CS5A-160MX", module_manufacturer_2 := "Canadian Solar"]
ts[module_manufacturer_2 == "China Sunergy (Nanjing) Co.,Ltd.", module_manufacturer_2 := "China Sunergy"]
ts[module_manufacturer_2 == "China Sunergy (Nanjing)", module_manufacturer_2 := "China Sunergy"]
ts[module_manufacturer_2 == "CSUN Eurasia Energy Systems Industry and Trade", module_manufacturer_2 := "CSUN"]
ts[module_manufacturer_2 == "Chint Solar (Zhejiang) Co., Ltd. ", module_manufacturer_2 := "Chint Solar (Zhejiang) Co., Ltd."]
ts[module_manufacturer_2 == "Dehui Solar Power (Vietnam) Co.,Ltd DH-M860W 325W", module_manufacturer_2 := "Dehui"]
ts[module_manufacturer_2 == "Dehui Solar Power (Vietnam) Co.,Ltd", module_manufacturer_2 := "Dehui"]
ts[module_manufacturer_2 == "DEHUI SOLAR POWER INC.", module_manufacturer_2 := "Dehui"]
ts[module_manufacturer_2 == "Dehui Solar Power (Vietnam) Co.,Ltd ", module_manufacturer_2 := "Dehui"]
ts[module_manufacturer_2 == "First Solar, Inc. ", module_manufacturer_2 := "First Solar, Inc."]
ts[module_manufacturer_2 == "Guangdong Golden Glass Technologies GG200M2-27/1482x992", module_manufacturer_2 := "Guangdong Golden Glass Technologies"]
ts[module_manufacturer_2 == "Heliene 72M-400 G1 Bifacial", module_manufacturer_2 := "Heliene"]
ts[module_manufacturer_2 == "JA Solar JAM72D09-370", module_manufacturer_2 := "JA Solar"]
ts[module_manufacturer_2 == "Jinko Solar Co., Ltd", module_manufacturer_2 := "Jinko Solar"]
ts[module_manufacturer_2 == "Japan Solar (Infini Co., Ltd.)", module_manufacturer_2 := "Japan Solar"]
ts[module_manufacturer_2 == "Japan Solar (Infini Co., Ltd.) ", module_manufacturer_2 := "Japan Solar"]
ts[module_manufacturer_2 == "LONGi Green Energy Technology Co., Ltd. ", module_manufacturer_2 := "LONGi Green Energy Technology Co., Ltd."]
ts[module_manufacturer_2 == "LG", module_manufacturer_2 := "LG Electronics Inc."]
ts[module_manufacturer_2 == "LG Electronics", module_manufacturer_2 := "LG Electronics Inc."]
ts[module_manufacturer_2 == "MAGE Solar", module_manufacturer_2 := "Mage Solar"]
ts[module_manufacturer_2 == "Mage Solar USA", module_manufacturer_2 := "Mage Solar"]
ts[module_manufacturer_2 == "Mage Solar USA", module_manufacturer_2 := "Mage Solar"]
ts[module_manufacturer_2 == "Maxeon Solar Technologies Ltd.", module_manufacturer_2 := "Maxeon - SunPower"] # non trivial here because Maxeon becomes a Singapore based company in 2020
ts[module_manufacturer_2 == "SunPower", module_manufacturer_2 := "Maxeon - SunPower"]
ts[module_manufacturer_2 == "Motech Americas", module_manufacturer_2 := "Motech Industries"] 
ts[module_manufacturer_2 == "Panasonic Corporation of North America", module_manufacturer_2 := "Panasonic"] 
ts[module_manufacturer_2 == "SANYO ELECTRIC CO LTD OF PANASONIC GROUP", module_manufacturer_2 := "Panasonic"]
ts[module_manufacturer_2 == "SANYO ELECTRIC CO LTD OF PANASONIC GROUP HIT-N210A01", module_manufacturer_2 := "Panasonic"]
ts[module_manufacturer_2 == "Sanyo Electric Co Ltd of Panasonic Group", module_manufacturer_2 := "Panasonic"]
ts[module_manufacturer_2 == "SANYO ELECTRIC CO LTD OF PANASONIC GROUP HIT-N190A03", module_manufacturer_2 := "Panasonic"]
ts[module_manufacturer_2 == "SANYO ELECTRIC CO LTD OF PANASONIC GROUP VBHN210AA01", module_manufacturer_2 := "Panasonic"]
ts[module_manufacturer_2 == "SANYO ELECTRIC CO LTD OF PANASONIC GROUP HIT-N214A01", module_manufacturer_2 := "Panasonic"]
ts[module_manufacturer_2 == "SANYO ELECTRIC CO LTD OF PANASONIC GROUP HIT-N215A01", module_manufacturer_2 := "Panasonic"]
ts[module_manufacturer_2 == "SANYO ELECTRIC CO LTD OF PANASONIC GROUP HIT-N220A01", module_manufacturer_2 := "Panasonic"]
ts[module_manufacturer_2 == "Panasonic Eco Solutions Canada", module_manufacturer_2 := "Panasonic"]
ts[module_manufacturer_2 == "Phono Solar Technology Co., Ltd.", module_manufacturer_2 := "Phono Solar Technology Co., Ltd."]
ts[module_manufacturer_2 == "Qcells North America", module_manufacturer_2 := "Hanwha QCells"] #American capacities of Hanwha
ts[module_manufacturer_2 == "Q-Cells North America", module_manufacturer_2 := "Hanwha QCells"]
ts[module_manufacturer_2 == "Hanwha Qcells (Qidong) Co. Ltd.", module_manufacturer_2 := "Hanwha QCells"]
ts[module_manufacturer_2 == "Q-Cells Q.Pro G2 250", module_manufacturer_2 := "Hanwha QCells"]
ts[module_manufacturer_2 == "Hanwha Q-Cells", module_manufacturer_2 := "Hanwha QCells"]
ts[module_manufacturer_2 == "Hanwha Q-Cells Q.PEAK BLK G4.1 295", module_manufacturer_2 := "Hanwha QCells"]
ts[module_manufacturer_2 == "Hanwha Q-Cells Q.PEAK DUO BLK-G6 330", module_manufacturer_2 := "Hanwha QCells"]
ts[module_manufacturer_2 == "Hanwha Qcells (Qidong) Co. Ltd. Q.PRIME L-G5.1.G 340", module_manufacturer_2 := "Hanwha QCells"]
ts[module_manufacturer_2 == "Hanwha Qcells (Qidong) Co. Ltd. Q.PRIME L-G5.1.G 335", module_manufacturer_2 := "Hanwha QCells"]
ts[module_manufacturer_2 == "Qcells North America Q.PEAK DUO BLK-G5 290", module_manufacturer_2 := "Hanwha QCells"]
ts[module_manufacturer_2 == "Hanwha Solar Canada", module_manufacturer_2 := "Hanwha QCells"]
ts[module_manufacturer_2 == "Hanwha Solar Canada", module_manufacturer_2 := "Hanwha QCells"]
ts[module_manufacturer_2 == "REC", module_manufacturer_2 := "REC Solar"]
ts[module_manufacturer_2 == "REC ScanModule", module_manufacturer_2 := "REC Solar"]
ts[module_manufacturer_2 == "ReneSola", module_manufacturer_2 := "Renesola America"]
ts[module_manufacturer_2 == "Renesola Jiangsu", module_manufacturer_2 := "Renesola America"]
ts[module_manufacturer_2 == "Sharp Electronics", module_manufacturer_2 := "Sharp"]
ts[module_manufacturer_2 == "Sharp ND-123UJF", module_manufacturer_2 := "Sharp"]
ts[module_manufacturer_2 == "SolarWorld Industries GmbH", module_manufacturer_2 := "SolarWorld"]
ts[module_manufacturer_2 == "SolarWorld Americas Inc", module_manufacturer_2 := "SolarWorld"]
ts[module_manufacturer_2 == "SolarWorld SW250 Mono", module_manufacturer_2 := "SolarWorld"]
ts[module_manufacturer_2 == "Silfab Solar Inc.", module_manufacturer_2 := "Silfab"]
ts[module_manufacturer_2 == "Solaria Corporation", module_manufacturer_2 := "Solaria"]
ts[module_manufacturer_2 == "Siliken Manufacturing USA", module_manufacturer_2 := "Siliken"]
ts[module_manufacturer_2 == "Siliken Canada", module_manufacturer_2 := "Siliken"]
ts[module_manufacturer_2 == "Suntech", module_manufacturer_2 := "Suntech Power"]
ts[module_manufacturer_2 == "Tesla Inc.", module_manufacturer_2 := "Tesla"]
ts[year >= 2016 & module_manufacturer_2 == "SolarCity Corp", module_manufacturer_2 := "Tesla"]
ts[module_manufacturer_2 == "SolarJuice Technology Inc.", module_manufacturer_2 := "SolarJuice"]
ts[module_manufacturer_2 == "SolarJuice American Inc.", module_manufacturer_2 := "SolarJuice"]
ts[module_manufacturer_2 == "Znshine PV-Tech", module_manufacturer_2 := "Znshine"]
ts[module_manufacturer_2 == "ZNSHINE PV-TECH Co., Ltd.", module_manufacturer_2 := "Znshine"]

## Merging data for module 3 manufacturer ----------------------------------
#list of manufacturer to merge for module 3
ts[module_manufacturer_3 == "Atlantis Energy Systems", module_manufacturer_3 := "Atlantis Energy"]
ts[module_manufacturer_3 == "BYD (Huizhou) Battery", module_manufacturer_3 := "BYD"]
ts[module_manufacturer_3 == "BYD Company Limited", module_manufacturer_3 := "BYD"]
ts[module_manufacturer_3 == "CSI Solar Co., Ltd. ", module_manufacturer_3 := "Canadian Solar"]
ts[module_manufacturer_3 == "CSI Solar Co., Ltd.", module_manufacturer_3 := "Canadian Solar"]
ts[module_manufacturer_3 == "Canadian Solar Inc.", module_manufacturer_3 := "Canadian Solar"]
ts[module_manufacturer_3 == "Canadian Solar Inc. CS5A-160MX", module_manufacturer_3 := "Canadian Solar"]
ts[module_manufacturer_3 == "China Sunergy (Nanjing) Co.,Ltd.", module_manufacturer_3 := "China Sunergy"]
ts[module_manufacturer_3 == "China Sunergy (Nanjing)", module_manufacturer_3 := "China Sunergy"]
ts[module_manufacturer_3 == "Chint Solar (Zhejiang) Co., Ltd. ", module_manufacturer_3 := "Chint Solar (Zhejiang) Co., Ltd."]
ts[module_manufacturer_3 == "CSUN Eurasia Energy Systems Industry and Trade", module_manufacturer_3 := "CSUN"]
ts[module_manufacturer_3 == "Dehui Solar Power (Vietnam) Co.,Ltd DH-M860W 325W", module_manufacturer_3 := "Dehui"]
ts[module_manufacturer_3 == "Dehui Solar Power (Vietnam) Co.,Ltd", module_manufacturer_3 := "Dehui"]
ts[module_manufacturer_3 == "DEHUI SOLAR POWER INC.", module_manufacturer_3 := "Dehui"]
ts[module_manufacturer_3 == "Dehui Solar Power (Vietnam) Co.,Ltd ", module_manufacturer_3 := "Dehui"]
ts[module_manufacturer_3 == "First Solar, Inc. ", module_manufacturer_3 := "First Solar, Inc."]
ts[module_manufacturer_3 == "Guangdong Golden Glass Technologies GG200M2-27/1482x992", module_manufacturer_3 := "Guangdong Golden Glass Technologies"]
ts[module_manufacturer_3 == "Heliene 72M-400 G1 Bifacial", module_manufacturer_3 := "Heliene"]
ts[module_manufacturer_3 == "JA Solar JAM72D09-370", module_manufacturer_3 := "JA Solar"]
ts[module_manufacturer_3 == "Jinko Solar Co., Ltd", module_manufacturer_3 := "Jinko Solar"]
ts[module_manufacturer_3 == "Japan Solar (Infini Co., Ltd.)", module_manufacturer_3 := "Japan Solar"]
ts[module_manufacturer_3 == "Japan Solar (Infini Co., Ltd.) ", module_manufacturer_3 := "Japan Solar"]
ts[module_manufacturer_3 == "LONGi Green Energy Technology Co., Ltd. ", module_manufacturer_3 := "LONGi Green Energy Technology Co., Ltd."]
ts[module_manufacturer_3 == "LG", module_manufacturer_3 := "LG Electronics Inc."]
ts[module_manufacturer_3 == "LG Electronics", module_manufacturer_3 := "LG Electronics Inc."]
ts[module_manufacturer_3 == "MAGE Solar", module_manufacturer_3 := "Mage Solar"]
ts[module_manufacturer_3 == "Mage Solar USA", module_manufacturer_3 := "Mage Solar"]
ts[module_manufacturer_3 == "Mage Solar USA", module_manufacturer_3 := "Mage Solar"]
ts[module_manufacturer_3 == "Maxeon Solar Technologies Ltd.", module_manufacturer_3 := "Maxeon - SunPower"] # non trivial here because Maxeon becomes a Singapore based company in 2020
ts[module_manufacturer_3 == "SunPower", module_manufacturer_3 := "Maxeon - SunPower"]
ts[module_manufacturer_3 == "Motech Americas", module_manufacturer_3 := "Motech Industries"] 
ts[module_manufacturer_3 == "Panasonic Corporation of North America", module_manufacturer_3 := "Panasonic"] 
ts[module_manufacturer_3 == "SANYO ELECTRIC CO LTD OF PANASONIC GROUP", module_manufacturer_3 := "Panasonic"]
ts[module_manufacturer_3 == "SANYO ELECTRIC CO LTD OF PANASONIC GROUP HIT-N210A01", module_manufacturer_3 := "Panasonic"]
ts[module_manufacturer_3 == "Sanyo Electric Co Ltd of Panasonic Group", module_manufacturer_3 := "Panasonic"]
ts[module_manufacturer_3 == "SANYO ELECTRIC CO LTD OF PANASONIC GROUP HIT-N190A03", module_manufacturer_3 := "Panasonic"]
ts[module_manufacturer_3 == "SANYO ELECTRIC CO LTD OF PANASONIC GROUP VBHN210AA01", module_manufacturer_3 := "Panasonic"]
ts[module_manufacturer_3 == "SANYO ELECTRIC CO LTD OF PANASONIC GROUP HIT-N214A01", module_manufacturer_3 := "Panasonic"]
ts[module_manufacturer_3 == "SANYO ELECTRIC CO LTD OF PANASONIC GROUP HIT-N215A01", module_manufacturer_3 := "Panasonic"]
ts[module_manufacturer_3 == "SANYO ELECTRIC CO LTD OF PANASONIC GROUP HIT-N220A01", module_manufacturer_3 := "Panasonic"]
ts[module_manufacturer_3 == "Panasonic Eco Solutions Canada", module_manufacturer_3 := "Panasonic"]
ts[module_manufacturer_3 == "Phono Solar Technology Co., Ltd.", module_manufacturer_3 := "Phono Solar Technology Co., Ltd."]
ts[module_manufacturer_3 == "Qcells North America", module_manufacturer_3 := "Hanwha QCells"] #American capacities of Hanwha
ts[module_manufacturer_3 == "Q-Cells North America", module_manufacturer_3 := "Hanwha QCells"]
ts[module_manufacturer_3 == "Hanwha Qcells (Qidong) Co. Ltd.", module_manufacturer_3 := "Hanwha QCells"]
ts[module_manufacturer_3 == "Q-Cells Q.Pro G2 250", module_manufacturer_3 := "Hanwha QCells"]
ts[module_manufacturer_3 == "Hanwha Q-Cells Q.PEAK BLK G4.1 295", module_manufacturer_3 := "Hanwha QCells"]
ts[module_manufacturer_3 == "Hanwha Q-Cells", module_manufacturer_3 := "Hanwha QCells"]
ts[module_manufacturer_3 == "Hanwha Q-Cells Q.PEAK DUO BLK-G6 330", module_manufacturer_3 := "Hanwha QCells"]
ts[module_manufacturer_3 == "Hanwha Qcells (Qidong) Co. Ltd. Q.PRIME L-G5.1.G 340", module_manufacturer_3 := "Hanwha QCells"]
ts[module_manufacturer_3 == "Hanwha Qcells (Qidong) Co. Ltd. Q.PRIME L-G5.1.G 335", module_manufacturer_3 := "Hanwha QCells"]
ts[module_manufacturer_3 == "Qcells North America Q.PEAK DUO BLK-G5 290", module_manufacturer_3 := "Hanwha QCells"]
ts[module_manufacturer_3 == "Hanwha Solar Canada", module_manufacturer_3 := "Hanwha QCells"]
ts[module_manufacturer_3 == "Hanwha Solar Canada", module_manufacturer_3 := "Hanwha QCells"]
ts[module_manufacturer_3 == "REC", module_manufacturer_3 := "REC Solar"]
ts[module_manufacturer_3 == "REC ScanModule", module_manufacturer_3 := "REC Solar"]
ts[module_manufacturer_3 == "ReneSola", module_manufacturer_3 := "Renesola America"]
ts[module_manufacturer_3 == "Renesola Jiangsu", module_manufacturer_3 := "Renesola America"]
ts[module_manufacturer_3 == "Sharp Electronics", module_manufacturer_3 := "Sharp"]
ts[module_manufacturer_3 == "Sharp ND-123UJF", module_manufacturer_3 := "Sharp"]
ts[module_manufacturer_3 == "SolarWorld Industries GmbH", module_manufacturer_3 := "SolarWorld"]
ts[module_manufacturer_3 == "SolarWorld Americas Inc", module_manufacturer_3 := "SolarWorld"]
ts[module_manufacturer_3 == "SolarWorld SW250 Mono", module_manufacturer_3 := "SolarWorld"]
ts[module_manufacturer_3 == "Silfab Solar Inc.", module_manufacturer_3 := "Silfab"]
ts[module_manufacturer_3 == "Solaria Corporation", module_manufacturer_3 := "Solaria"]
ts[module_manufacturer_3 == "Siliken Manufacturing USA", module_manufacturer_3 := "Siliken"]
ts[module_manufacturer_3 == "Siliken Canada", module_manufacturer_3 := "Siliken"]
ts[module_manufacturer_3 == "Suntech", module_manufacturer_3 := "Suntech Power"]
ts[module_manufacturer_3 == "Tesla Inc.", module_manufacturer_3 := "Tesla"]
ts[year >= 2016 & module_manufacturer_3 == "SolarCity Corp", module_manufacturer_3 := "Tesla"]
ts[module_manufacturer_3 == "SolarJuice Technology Inc.", module_manufacturer_3 := "SolarJuice"]
ts[module_manufacturer_3 == "SolarJuice American Inc.", module_manufacturer_3 := "SolarJuice"]
ts[module_manufacturer_3 == "Znshine PV-Tech", module_manufacturer_3 := "Znshine"]
ts[module_manufacturer_3 == "ZNSHINE PV-TECH Co., Ltd.", module_manufacturer_3 := "Znshine"]

# to lower case and getting rid of white trays
ts[, module_manufacturer_1 := trimws(tolower(module_manufacturer_1))]
ts[, module_manufacturer_2 := trimws(tolower(module_manufacturer_2))]
ts[, module_manufacturer_3 := trimws(tolower(module_manufacturer_3))]
ts[, module_model_1 := trimws(tolower(module_model_1))]
ts[, module_model_2 := trimws(tolower(module_model_2))]
ts[, module_model_3 := trimws(tolower(module_model_3))]

# Reorganizing Tables -----------------------------------------------------
cols = c("year","year_quarter", "zip", "city", "state", "system_ID_1", "system_ID_2", "installation_date",
         "PV_system_size_DC", "total_installed_price", "rebate_or_grant", "customer_segment",
         "expansion_system", "third_party_owned", "installer_name", 
         "module_manufacturer_1", "module_model_1", "module_quantity_1", "technology_module_1",
         "module_manufacturer_2", "module_model_2", "module_quantity_2", "technology_module_2",
         "module_manufacturer_3", "module_model_3", "module_quantity_3", "technology_module_3",
         "additional_modules", 
         "nameplate_capacity_module_1","nameplate_capacity_module_2","nameplate_capacity_module_3",
         "efficiency_module_1", "efficiency_module_2", "efficiency_module_3",
         "inverter_manufacturer_2", "inverter_model_2","inverter_quantity_2",
         "inverter_manufacturer_3", "inverter_model_3","inverter_quantity_3",
         "additional_inverters", "micro_inverter_1","micro_inverter_2",
         "micro_inverter_3", "built_in_meter_inverter_1","built_in_meter_inverter_2",
         "built_in_meter_inverter_3", "output_capacity_inverter_1", "output_capacity_inverter_2",
         "output_capacity_inverter_3", 
         "DC_optimizer", "inverter_loading_ratio", "battery_manufacturer",
         "battery_model","battery_rated_capacity_kW", "battery_rated_capacity_kWh",
         "battery_price","technology_type")

ts = ts[year %in% 2002:2023, .SDcols = cols]

write_parquet(ts,data_temp("TTS_clean_names.parquet"))