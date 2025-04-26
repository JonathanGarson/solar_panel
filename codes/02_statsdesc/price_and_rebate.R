# -------------------------------------------------------------------
# Description: Script to compute and visualize descriptive statistics 
#              on weighted prices and rebates over time by system (HO/TPO)
# -------------------------------------------------------------------

# Load Packages -----------------------------------------------------
library(arrow)
library(data.table)
library(ggplot2)
library(lubridate)
library(scales)

# Load Data ---------------------------------------------------------
tts <- read_parquet(data_final("tts_final.parquet"))

# Compute Summary Statistics ----------------------------------------

# Compute quarterly mean price and rebate by system (ho)
price_evol <- tts[, .(
  mean_price_w = mean(price_w, na.rm = TRUE),
  mean_rebate_w = mean(rebate_w, na.rm = TRUE)
), by = .(year_quarter)]

# Extract year and quarter, convert to date format
price_evol[, c("year", "quarter") := tstrsplit(year_quarter, "Q")]
price_evol[, `:=`(
  year = as.integer(year),
  quarter = as.integer(quarter),
  quarter_date = yq(paste0(year, " Q", quarter))  
)]

price_evol[, ratio_sub_price := mean_rebate_w/mean_price_w]

# Prepare Data for Plotting ------------------Ho----------------------

# Long format for price and rebate
price_evol_long <- melt(
  price_evol,
  id.vars = c("quarter_date"),
  measure.vars = c("mean_price_w", "mean_rebate_w"),
  variable.name = "type",
  value.name = "value"
)

# Clean and label types
price_evol_long[, type := ifelse(type == "mean_price_w", "Price", "Rebate")]

# Plotting ----------------------------------------------------------

ggplot(price_evol_long, aes(
  x = quarter_date,
  y = value,
  color = type,
  linetype = as.factor(ho),
  group = interaction(type, ho))
) +
  geom_line() +
  scale_color_manual(values = c(
    "Price" = "steelblue",
    "Rebate" = "indianred"
  )) +
  scale_linetype_manual(
    name = "System",
    values = c("0" = "dotted", "1" = "solid"),
    # labels = c("0" = "TPO", "1" = "HO")
  ) +
  scale_y_continuous(
    name = "$/W",
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y Q1"
  ) +
  labs(
    x = "Year",
    y = "Value",
    color = "Type"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
ggsave("output/figures/statdesc/price_rebate_w.pdf", width = 10, height = 8)
