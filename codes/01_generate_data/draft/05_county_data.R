# This code cleans county data

library(arrow)
library(data.table)
library(zoo)

# Data --------------------------------------------------------------------

county = as.data.table(fread(data_raw("census/county_data.csv")))

old_names = unique(colnames(county))
new_names = tolower(trimws(old_names))
setnames(county, old_names, new_names)
setnames(county, "label (grouping)", "label")

county[, label := tolower(trimws(label))]
county[, label := tolower(trimws(gsub("[[:space:]]+", " ", label)))]
county = county[!label %in% c("total population", "        margin of error"),]

