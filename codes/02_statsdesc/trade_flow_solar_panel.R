# This code use BACI yearly data to see the trade flow of solar panels between China and Taiwan and the U.S.

library(data.table)
library(ggplot2)

# Data Import -------------------------------------------------------------

trade = as.data.table(read_parquet(data_final("trade/baci_2010_2020.parquet")))

# Trade flow between China, Taiwan, Canada, Mexico  -----------------------
# 1. Filter for U.S. imports
trade_us <- trade[importer_iso == "USA"]

# 2. Aggregate data by year, product code, and exporter
# This creates exporter-level totals.
trade_us_export <- trade_us[, .(
  export_qty = sum(quantity, na.rm = TRUE),
  export_val = sum(value, na.rm = TRUE)
), by = .(year, code, importer_iso, exporter_iso)]

# 3. Compute total U.S. imports (by quantity and value) for each year and product code.
# Note: importer_iso is always "USA" here.
total_imports <- trade_us_export[, .(
  total_qty = sum(export_qty),
  total_val = sum(export_val)
), by = .(year, code, importer_iso)]

# 4. Merge these total import values back to the exporter-level data.
trade_us_export <- merge(trade_us_export, total_imports, by = c("year", "code", "importer_iso"))

# 5. Create the share variables.
trade_us_export[, share_import_q := export_qty / total_qty]
trade_us_export[, share_import_v := export_val / total_val]

# Identify the top 10 exporters overall (by total export value across the period) --
top_exporters <- trade_us_export[, .(total_val = sum(export_val, na.rm = TRUE)), by = exporter_iso]
top_exporters <- top_exporters[order(-total_val)][1:10, exporter_iso]

# Filter data for only the top exporters ----------------------------------
trade_top <- trade_us_export[exporter_iso %in% top_exporters]

# Reshape data to long format to handle both quantity and value shares --------
trade_long <- melt(trade_top, 
                   id.vars = c("year", "code", "importer_iso", "exporter_iso"),
                   measure.vars = c("share_import_q", "share_import_v"),
                   variable.name = "measure", 
                   value.name = "share")

# Rename measure variable for clarity
trade_long[, measure := ifelse(measure == "share_import_q", "Quantity", "Value")]

# Plot: Four panels (2 goods x 2 measures) ----------------------------------
ggplot(trade_long, aes(x = year, y = share, 
                       color = exporter_iso, group = exporter_iso)) +
  geom_line() +
  # Add vertical lines at 2012, 2014, and 2018
  geom_vline(xintercept = c(2012, 2014, 2018), 
             color = "black", linetype = "dashed", size = 0.5) +
  facet_grid(code ~ measure, scales = "free_y") +
  # Set x-axis breaks to whole years only
  scale_x_continuous(breaks = seq(floor(min(trade_long$year)), ceiling(max(trade_long$year)), by = 1)) +
  labs(title = "Evolution of U.S. Import Shares by Top Exporters",
       subtitle = "Separate panels for each product code (row) and measure (Quantity vs. Value)",
       x = "Year",
       y = "Share of U.S. Imports",
       color = "Exporter") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
