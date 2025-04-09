# This script clean the small data for wages of solar installer and electricity retail prices

library(data.table)
library(glue)
library(ggplot2)
library(zoo)

# Data --------------------------------------------------------------------

elec = fread(data_raw("electricity_price/Average_retail_price_of_electricity_formatted.csv"))
cpi = fread(data_raw("us_cpi.csv"))

# Cleaning Elec Price -----------------------------------------------------

keep = setdiff(colnames(elec), c("units","source key"))
elec = elec[, .SD, .SDcols = keep]
setnames(elec, "description", "state_full")
elec[, state_full := gsub(pattern = ".*:\\s*", replacement = "", x = state_full)]

# Create a vector of full state/region names
states <- c("New England", "Connecticut", "Maine", "Massachusetts", "New Hampshire", 
            "Rhode Island", "Vermont", "Middle Atlantic", "New Jersey", "New York", 
            "Pennsylvania", "East North Central", "Illinois", "Indiana", "Michigan", 
            "Ohio", "Wisconsin", "West North Central", "Iowa", "Kansas", "Minnesota", 
            "Missouri", "Nebraska", "North Dakota", "South Dakota", "South Atlantic", 
            "Delaware", "District Of Columbia", "Florida", "Georgia", "Maryland", 
            "North Carolina", "South Carolina", "Virginia", "West Virginia", 
            "East South Central", "Alabama", "Kentucky", "Mississippi", "Tennessee", 
            "West South Central", "Arkansas", "Louisiana", "Oklahoma", "Texas", "Mountain", 
            "Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", 
            "Wyoming", "Pacific Contiguous", "California", "Oregon", "Washington", 
            "Pacific Noncontiguous", "Alaska", "Hawaii")

# Create a corresponding vector of two-letter abbreviations
abbrevs <- c("nx", "ct", "me", "ma", "nh", "ri", "vt", "ml", "nj", "ny", "pa", "en", "il", 
             "in", "mi", "oh", "wi", "wn", "ia", "ks", "mn", "mo", "ne", "nd", "sd", "sa", 
             "de", "dc", "fl", "ga", "md", "nc", "sc", "va", "wv", "es", "al", "ky", "ms", 
             "tn", "ws", "ar", "la", "ok", "tx", "my", "az", "co", "id", "mt", "nv", "nm", 
             "ut", "wy", "pc", "ca", "or", "wa", "pn", "ak", "hi")

# Combine them into a data frame
state_abbrevs_df <- data.frame(state_full = states, state_short = abbrevs, 
                               stringsAsFactors = FALSE)


elec = merge(elec, state_abbrevs_df, by = 'state_full')

# Deflating 2010 $ value
month = setdiff(colnames(cpi), c("Year", "HALF1", "HALF2"))
cpi[, year_cpi := rowMeans(.SD), .SDcols = month]
base_cpi = cpi[Year == 2010,]$year_cpi
deflated_cpi = cpi[Year %in% 2010:2020, .(deflated_cpi = year_cpi/base_cpi)]

years <- 2010:2020
deflated_cpi$year = as.character(years)

# Loop over each column (each quarter)
for (col in names(elec[, 2:45])) {
  # Extract the year using a regex that captures four digits at the end of the column name.
  y <- sub(".* (\\d{4})$", "\\1", col)

  # Look up the CPI factor for that year.
  factor <- deflated_cpi[year == y]$deflated_cpi

  # Deflate the prices in that quarter by dividing by the corresponding CPI factor.
  elec[, (col) := get(col) / factor]
}

elec = elec[, 2:46]
setnames(elec,"state_short", "state")


dt_long <- melt(elec, 
                id.vars = "state", 
                variable.name = "quarter", 
                value.name = "elec_price")
dt_long[, year := gsub(".* (\\d{4})$", "\\1", quarter)]
dt_long[, mean_price_year := mean(elec_price), by = .(state, year)]

dt_long[, year_quarter := format(as.yearqtr(quarter, format = "Q%q %Y"), format = "%YQ%q")]
dt_long = dt_long[, .SD, .SDcols = c("state", "year_quarter", "elec_price", "mean_price_year")]

ggplot(dt_long[state %in% c("ca", "tx"), ], 
       aes(x = year_quarter, group = state, color = state)) +
  geom_line(aes(y = elec_price), linewidth = 0.8) +
  geom_line(aes(y = mean_price_year), linetype = "dashed", linewidth = 1.2) +
  theme_classic() +
  labs(x = "Quarter", 
       y = "Electricity Price ($/W)", 
       color = "State") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))
ggsave("output/figures/statdesc/elec_price_california_real_price.pdf", width = 10, height = 8)

fwrite(dt_long, data_temp("elec_price.csv"))

# wage_solar_installer ----------------------------------------------------

data_path_wage = data_raw("solar_installer_wage")
files = list.files(path = data_path_wage, pattern = "*12_dl.xls|*13_dl.xls|*.xlsx")
data_to_export = data.table()

for (f in files) {
  # Read file into a temporary data.table (local to each iteration)
  temp_data <- setDT(readxl::read_excel(glue("{data_path_wage}/{f}")))
  
  # Check for the column with installer title. Use the one available.
  if ("OCC_TITLE" %in% names(temp_data)) {
    temp_data <- temp_data[OCC_TITLE == "Solar Photovoltaic Installers", ]
  } else if ("occ_title" %in% names(temp_data)) {
    temp_data <- temp_data[occ_title == "Solar Photovoltaic Installers", ]
  } else {
    stop("Neither 'OCC_TITLE' nor 'occ_title' found in file: ", f)
  }
  
  # Clean remaining columns
  clean_column <- setdiff(colnames(temp_data), c("AREA", "OCC_GROUP", "ANNUAL", "HOURLY"))
  temp_data <- temp_data[, .SD, .SDcols = clean_column]
  
  # Convert all column names to lowercase
  col_names <- tolower(colnames(temp_data))
  setnames(temp_data, colnames(temp_data), col_names)
  
  # Rename column if needed
  if ("st" %in% names(temp_data)) setnames(temp_data, "st", "state_short")
  if ("state" %in% names(temp_data)) {
    temp_data[, state := tolower(state)]
  }
  
  if (f == "state_M2019_dl.xlsx"){
    setnames(temp_data, "area", "state")
    temp_data = merge(temp_data, state_abbrevs_df, by.x = "state", by.y = "state_full")
  }
  if (f == "state_M2020_dl.xlsx"){
    setnames(temp_data, "prim_state", "state_short")
    temp_data[, state_short := tolower(state_short)]
    temp_data = merge(temp_data, state_abbrevs_df, by = "state_short")
    setnames(temp_data, "state_full", "state")
  }
  
  # Extract year from file name
  year_extracted <- gsub(".*?(\\d{4}).*", "\\1", f)
  temp_data[, year := year_extracted]
  
  # Adjust means using the CPI factor (assumes deflated_cpi is a data.table with columns 'year' and 'deflated_cpi')
  temp_data[, `:=` (h_mean = as.numeric(h_mean),
               a_mean = as.numeric(a_mean),
               h_median = as.numeric(h_median),
               a_median =as.numeric(a_median))]
  base_cpi <- deflated_cpi[year == year_extracted, deflated_cpi]
  temp_data[, h_mean := h_mean / base_cpi]
  temp_data[, a_mean := a_mean / base_cpi]
  temp_data[, h_median := h_median / base_cpi]
  temp_data[, a_median := a_median / base_cpi]
  
  # Append the cleaned data from this file to the export data table
  data_to_export <- rbind(data_to_export, temp_data[, .(state_short,state, year, tot_emp, jobs_1000, h_mean, h_median, a_mean, a_median)], fill = TRUE)
}

# 2019 is not working properly, we add it manually
dt_2019 = setDT(readxl::read_excel(glue("{data_path_wage}/state_M2019_dl.xlsx")))
dt_2019 = dt_2019[occ_title == "Solar Photovoltaic Installers"]
setnames(dt_2019, "area_title", "state_long")
clean_column <- setdiff(colnames(dt_2019), c("area","occ_group", "annual", "hourly"))
dt_2019 <- dt_2019[, .SD, .SDcols = clean_column]
col_names <- tolower(colnames(dt_2019))
setnames(dt_2019, colnames(dt_2019), col_names)
dt_2019 = merge(dt_2019, state_abbrevs_df, by.x = "state_long", by.y = "state_full")
setnames(dt_2019, c("state_long"), c("state"))
year_extracted = 2019
dt_2019[, year := year_extracted]
dt_2019[, `:=` (h_mean = as.numeric(h_mean),
                  a_mean = as.numeric(a_mean),
                  h_median = as.numeric(h_median),
                  a_median =as.numeric(a_median))]
base_cpi <- deflated_cpi[year == year_extracted, deflated_cpi]
dt_2019[, h_mean := h_mean / base_cpi]
dt_2019[, a_mean := a_mean / base_cpi]
dt_2019[, h_median := h_median / base_cpi]
dt_2019[, a_median := a_median / base_cpi]
data_to_export = rbind(data_to_export, dt_2019[, .(state_short,state, year, tot_emp, jobs_1000, h_mean, h_median, a_mean, a_median)])

data_to_export[, state_short := tolower(state_short)]
data_to_export[, state := tolower(state)]

ggplot(data_to_export[state == "california", ], 
       aes(x = year, y = h_median, group = 1, color = state)) +
  geom_line(linewidth = 1, color = "blue") +
  labs(x = "Year", y = "Hourly Wage ($)", color = "State") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

data_to_export = unique(data_to_export)
data_to_export[, state := NULL]
setnames(data_to_export, c("state_short"), c("state"))
setorder(data_to_export, year)

fwrite(data_to_export, data_temp("wage_installer_PV.csv"))

# Share of Panel in Installation price ------------------------------------
# This data has been transfered to Houde & al (2025), by the LBNL

share_panel_install = data.table(
  year = c(2012:2018),
  share = c(17.91, 20.04, 18.92, 17.16, 13.33, 12.09, 15.48)
)

fwrite(share_panel_install, data_temp("share_panel_install_price.csv"))
