# We determine the county market share of different brands to try to see if some heterogeneity appears
# We establish our calculus at the county level since it is seems a reasonable scale of operation and disagregation (at least in a first time)

library(arrow)
library(data.table)
library(fitdistrplus)
library(ggplot2)
library(glue)
library(sf)

# Data  -------------------------------------------------------------------

ts = read_parquet(data_temp("TTS_clean_names.parquet"))
top_manufacturer = read_parquet(data_temp("top_manufacturers.parquet"))

# Calculation -------------------------------------------------------------
# Aggregate sales by country of origin, county, and year:
ts <- merge(ts, top_manufacturer, by.x = "module_manufacturer_1", by.y = "Manufacturer")

# 1. Aggregate sales by brand origin for each county and year.
brand_sales_origin <- ts[, .(local_market_sales_origin = sum(module_quantity_1, na.rm = TRUE)), 
                         by = .(year, county, Country)]

# 2. Aggregate total sales by county and year.
total_sales <- ts[, .(sum_local_sales_year = sum(module_quantity_1, na.rm = TRUE)), 
                  by = .(year, county)]

# 3. Merge the two aggregations to have both the origin-specific and total sales.
brand_sales_origin <- merge(brand_sales_origin, total_sales, by = c("year", "county"))

# 4. Compute the market share per brand origin.
brand_sales_origin[, local_market_share_origin := local_market_sales_origin / sum_local_sales_year]

# 5. Clean of Na observation for county
brand_sales_origin = brand_sales_origin[!is.na(county),]

# Histogram ---------------------------------------------------------------

years = 2010:2023
for (y in years){
  data_china <- brand_sales_origin[Country == 'China' & year == y, local_market_share_origin]
  fit_norm <- fitdist(data_china, "norm")
  # plot(fit_norm)
  
  ggplot(data.frame(mshare = data_china), aes(x = mshare)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.05, fill = "skyblue", color = "black") +
    stat_function(fun = dnorm, args = list(mean = fit_norm$estimate["mean"],
                                           sd = fit_norm$estimate["sd"]),
                  color = "red", linewidth = 1) +
    labs(x = "Local Market Share", y = "Density",
         title = glue("Histogram & Normal Fit of Chinese Market Share {y}")) +
    theme_minimal()
  ggsave(glue("output/figures/statdesc/distrib_market_share_county_china_{y}.pdf"))
}

# Maps --------------------------------------------------------------------

## 2014 --------------------------------------------------------------------


# 1. Filter the data for 2014 and Chinese origin.
#    (Assumes your aggregated data table has a column 'Country' indicating origin.)
china_2014 <- brand_sales_origin[year == 2014 & Country == "China", 
                                 .(county, chinese_concentration = local_market_share_origin)]

# Ensure that the county identifier is a character string for proper merging.
china_2014[, county := as.character(county)]

# 2. Read the U.S. counties shapefile.
#    Update the path below to point to your shapefile location.
us_counties <- st_read(data_raw("us-county-boundaries\\us-county-boundaries.shp"))

# Ensure the shapefile's county identifier (here assumed to be GEOID) is also a character.
us_counties$GEOID <- as.character(us_counties$geoid)

# Merge the shapefile with your Chinese concentration data.
# Using 'all.x = TRUE' keeps all counties; counties without data will show as NA.
us_counties <- merge(us_counties, china_2014, by.x = "geoid", by.y = "county", all.x = TRUE)

# 3. Plot the map with a color gradient based on Chinese concentration.
map_2014 <- ggplot(us_counties) +
  geom_sf(aes(fill = chinese_concentration), color = NA) +
  # Choose a gradient from white (low concentration) to red (high concentration)
  scale_fill_gradient(low = "blue", high = "red", 
                      na.value = "grey90", 
                      name = "Chinese\nConcentration") +
  # Set coordinate limits so that the x-axis shows from -120° to -60° and y-axis covers continental US latitudes
  coord_sf(xlim = c(-127, -65), ylim = c(23.5, 50), expand = FALSE) +
  # Add x-axis breaks every 10 degrees
  scale_x_continuous(breaks = seq(-127, -65, by = 10)) +
  labs(title = "Chinese Market Share in U.S. Counties (2014)",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(plot = map_2014, "output/map/chinese_market_share_us_counties_2014.pdf")

# print(map_2014)

## 2019 --------------------------------------------------------------------

# 1. Filter the data for 2014 and Chinese origin.
#    (Assumes your aggregated data table has a column 'Country' indicating origin.)
china_2019 <- brand_sales_origin[year == 2019 & Country == "China", 
                                 .(county, chinese_concentration = local_market_share_origin)]

# Ensure that the county identifier is a character string for proper merging.
china_2019[, county := as.character(county)]

# 2. Read the U.S. counties shapefile.
#    Update the path below to point to your shapefile location.
us_counties <- st_read(data_raw("us-county-boundaries\\us-county-boundaries.shp"))

# Ensure the shapefile's county identifier (here assumed to be GEOID) is also a character.
us_counties$GEOID <- as.character(us_counties$geoid)

# Merge the shapefile with your Chinese concentration data.
# Using 'all.x = TRUE' keeps all counties; counties without data will show as NA.
us_counties <- merge(us_counties, china_2019, by.x = "geoid", by.y = "county", all.x = TRUE)

# 3. Plot the map with a color gradient based on Chinese concentration.
map_2019 <- ggplot(us_counties) +
  geom_sf(aes(fill = chinese_concentration), color = NA) +
  # Choose a gradient from white (low concentration) to red (high concentration)
  scale_fill_gradient(low = "blue", high = "red", 
                      na.value = "grey90", 
                      name = "Chinese\nConcentration") +
  # Set coordinate limits so that the x-axis shows from -120° to -60° and y-axis covers continental US latitudes
  coord_sf(xlim = c(-127, -65), ylim = c(23.5, 50), expand = FALSE) +
  # Add x-axis breaks every 10 degrees
  scale_x_continuous(breaks = seq(-127, -65, by = 10)) +
  labs(title = "Chinese Market Share in U.S. Counties (2019)",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(plot = map_2019, "output/map/chinese_market_share_us_counties_2019.pdf")
# print(map_2019)

## 2022 --------------------------------------------------------------------

# 1. Filter the data for 2014 and Chinese origin.
#    (Assumes your aggregated data table has a column 'Country' indicating origin.)
china_2023 <- brand_sales_origin[year == 2023 & Country == "China", 
                                 .(county, chinese_concentration = local_market_share_origin)]

# Ensure that the county identifier is a character string for proper merging.
china_2023[, county := as.character(county)]

# 2. Read the U.S. counties shapefile.
#    Update the path below to point to your shapefile location.
us_counties <- st_read(data_raw("us-county-boundaries\\us-county-boundaries.shp"))

# Ensure the shapefile's county identifier (here assumed to be GEOID) is also a character.
us_counties$GEOID <- as.character(us_counties$geoid)

# Merge the shapefile with your Chinese concentration data.
# Using 'all.x = TRUE' keeps all counties; counties without data will show as NA.
us_counties <- merge(us_counties, china_2023, by.x = "geoid", by.y = "county", all.x = TRUE)

# 3. Plot the map with a color gradient based on Chinese concentration.
map_2023 <- ggplot(us_counties) +
  geom_sf(aes(fill = chinese_concentration), color = NA) +
  # Choose a gradient from white (low concentration) to red (high concentration)
  scale_fill_gradient(low = "blue", high = "red", 
                      na.value = "grey90", 
                      name = "Chinese\nConcentration") +
  # Set coordinate limits so that the x-axis shows from -120° to -60° and y-axis covers continental US latitudes
  coord_sf(xlim = c(-127, -65), ylim = c(23.5, 50), expand = FALSE) +
  # Add x-axis breaks every 10 degrees
  scale_x_continuous(breaks = seq(-127, -65, by = 10)) +
  labs(title = "Chinese Market Share in U.S. Counties (2023)",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(plot = map_2023, "output/map/chinese_market_share_us_counties_2023.pdf")
# print(map_2019)

