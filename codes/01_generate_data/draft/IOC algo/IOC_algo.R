# This code reproduce the IOC algorithm developped by O'Shaughnessy (2016) to determine the spatial market share

library(arrow)
library(data.table)
library(geosphere)
library(zipcodeR)

# Data --------------------------------------------------------------------

tts_algo = setDT(read_parquet(data_final("TTS_final.parquet")))

# Algorithm ---------------------------------------------------------------

## Step 1 ------------------------------------------------------------------

# (a) Get unique installer-zip combinations.
zip_inst <- unique(tts_algo[year == 2018, .(zip_code, installer_name)])

# (b) Count unique installers per zip.
total_inst <- zip_inst[, .(n_inst = .N), by = zip_code]

# (c) Create all pairs of zip codes that share an installer.
# Joining on installer_name yields one row for each installer shared between two zip codes.
pair_inst <- merge(zip_inst, zip_inst, by = "installer_name", allow.cartesian = TRUE, suffixes = c("_x", "_y"))
# Remove self-pairs (i.e. same zip_code)
pair_inst <- pair_inst[zip_code_x != zip_code_y]
pair_counts <- pair_inst[, .(shared = .N), by = .(zip_code_x, zip_code_y)]

# (d) For each zip-code pair, count shared installers.
pair_counts <- pair_inst[, .(shared = .N), by = .(zip_code_x, zip_code_y)]

# (e) Join in the total number of installers for each zip.
pair_counts <- merge(pair_counts, total_inst, by.x = "zip_code_x", by.y = "zip_code", all.x = TRUE)
setnames(pair_counts, "n_inst", "n_inst_x")
pair_counts <- merge(pair_counts, total_inst, by.x = "zip_code_y", by.y = "zip_code", all.x = TRUE)
setnames(pair_counts, "n_inst", "n_inst_y")

# (f) Compute the shares and then the IOC.
# share_xy = (# shared installers) / (total installers in x)
# share_yx = (# shared installers) / (total installers in y)
pair_counts[, IOC := (shared / n_inst_x) * (shared / n_inst_y)]

# (g) For candidate market selection, compute total IOC per zip code.
# Here we sum IOC for each zip when it is in the x-position.
total_IOC <- pair_counts[, .(sum_IOC = sum(IOC)), by = .(zip_code = zip_code_x)]

## Step 2 ------------------------------------------------------------------
# Finding geographical coordinate
zip_coords = reverse_zipcode(zip_inst$zip_code)
setnames(zip_coords, "zipcode", "zip_code")

# Ensure zip_coords contains only zip codes with installs.
zip_coords <- merge(total_inst[, .(zip_code)], zip_coords, by = "zip_code", all.x = TRUE)

# (a) Create a helper function to compute distances (using geosphere::distHaversine for lat/lon)
dist_fun <- function(lat1, lon1, lat2, lon2) {
  distHaversine(c(lon1, lat1), c(lon2, lat2))
}

# (b) For each zip code, compute distances to all other zip codes.
# We'll compute this in a data.table join style.
# First, create a copy with suffix _nb for neighbor info.
zip_nb <- copy(zip_coords)
setnames(zip_nb, c("zip_code", "lat", "lng"), c("zip_nb", "lat_nb", "lon_nb"))
setnames(zip_coords, c("lng"), c("lon"))

# temporary key
zip_coords[, temp := 1]
zip_nb[, temp := 1]

# Create a cross join of zip_coords and zip_nb.
# Perform the merge on the temporary key (cross join)
neighbors_all <- merge(zip_coords, zip_nb, by = "temp", allow.cartesian = TRUE)[, temp := NULL]

# Exclude self: same zip code.
neighbors_all <- neighbors_all[zip_code != zip_nb]

# Compute distance (in meters, for example)
neighbors_all[, distance := dist_fun(lat, lon, lat_nb, lon_nb), by = .(zip_code, zip_nb)]

# (c) For each zip_code, select the 6 nearest neighbors.
neighbors <- neighbors_all[order(distance), .SD[1:6], by = zip_code]
# neighbors now has: zip_code, lat, lon, zip_nb, lat_nb, lon_nb, distance
setnames(neighbors, "zip_nb", "neighbor_zip")


## Step 3 ------------------------------------------------------------------

# Define a function that, given a candidate zip, returns its near neighbor network (from the "neighbors" table)
get_nn_network <- function(candidate, neighbors_dt, available_zips) {
  # Start with candidate itself.
  network <- candidate
  # To keep track of newly added zip codes in each iteration.
  new_members <- candidate
  
  repeat {
    # For all newly added zip codes, get their 6 nearest neighbors.
    nn <- neighbors_dt[zip_code %in% new_members, unique(neighbor_zip)]
    # Only keep those that are still available (not yet assigned to a market).
    nn <- nn[nn %in% available_zips]
    # New zip codes not already in the network.
    new_members <- setdiff(nn, network)
    if (length(new_members) == 0) break
    network <- union(network, new_members)
  }
  return(network)
}

## Step 4 ------------------------------------------------------------------

# Set market criterion (example: 0.25)
criterion <- 0.25

# All available zip codes (those with installs)
available_zips <- unique(total_inst$zip_code)

# Create an empty table to store market assignments.
market_assignments <- data.table(zip_code = available_zips, market_id = NA_integer_)

# Set initial market id counter.
market_id_counter <- 1

# For easier lookup of IOC between any candidate and another zip,
# create a lookup table from pair_counts.
# We want to be able to get IOC(candidate, other). Note that if a pair is missing, assume 0.
ioc_lookup <- pair_counts[, .(zip_x = zip_code_x, zip_y = zip_code_y, IOC)]
setkey(ioc_lookup, zip_x, zip_y)

# Helper function to look up IOC; if missing, return 0.
get_ioc <- function(candidate, other) {
  res <- ioc_lookup[.(candidate, other), IOC]
  if (is.na(res)) return(0) else return(res)
}

# Main iterative assignment loop.
while(length(available_zips) > 0) {
  # Step 4a: Select candidate market from available zips as the one with maximum sum_IOC.
  # Merge available zips with total_IOC.
  cand_dt <- merge(data.table(zip_code = available_zips), total_IOC, by = "zip_code", all.x = TRUE)
  # If any zip has no pair entry, set sum_IOC to 0.
  cand_dt[is.na(sum_IOC), sum_IOC := 0]
  candidate <- cand_dt[which.max(sum_IOC), zip_code]
  
  # Step 4b: Build the near neighbor network for this candidate.
  nn_network <- get_nn_network(candidate, neighbors, available_zips)
  
  # Step 4c: Within the network, assign zip codes whose IOC with the candidate exceeds the criterion.
  # Include candidate itself.
  market_members <- candidate
  for(z in setdiff(nn_network, candidate)) {
    if(get_ioc(candidate, z) > criterion | get_ioc(z, candidate) > criterion) {
      market_members <- union(market_members, z)
    }
  }
  
  # Optionally, you might also include additional iterations
  # (e.g., use the newly assigned members as additional seeds)
  # For simplicity we use just the candidate's IOC.
  
  # Step 4d: Assign these market_members the current market id.
  market_assignments[zip_code %in% market_members, market_id := market_id_counter]
  
  # Remove these from available zips.
  available_zips <- setdiff(available_zips, market_members)
  
  # Increment market id.
  market_id_counter <- market_id_counter + 1
}

# market_assignments now contains the market id for each zip code.
fwrite(market_assignments, data_temp("market_assignments_ioc.csv"))

