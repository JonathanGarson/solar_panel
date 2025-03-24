# In this code we create the final TTS dataset for our regression:
# - We provide a clear procedure to identify and figure to justify the firm that we keep for our analysis
# - We build the key quality and adoption variable

library(arrow)
library(data.table)
library(ggplot2)
library(glue)
library(gt)

# Data --------------------------------------------------------------------

tts = setDT(read_parquet(data_temp("TTS_merged.parquet")))

# Selecting firms ---------------------------------------------------------

# Market share of installation by brand approach : we select the 15 biggest firms on the period, their market share is above 3%.

# Displaying the number of installation per model per year
# Displaying the number the quantity of model per year
for (y in c(2011, 2013, 2017, 2019)){
  tts[, sales_per_model := sum(module_quantity, na.rm = T) , by = c("year","module_model")]
  sales_dt <- unique(tts[year == `y`, .(module_manufacturer,module_model, sales_per_model)])
  sales_dt[, sum_year := sum(sales_per_model)]
  sales_dt[, pct_sales := sales_per_model/sum_year]
  setorder(sales_dt, cols = -pct_sales)
  sales_dt[, cum_sum_pct_sales := cumsum(pct_sales)]
  
  # Compute summary statistics
  n_models    <- nrow(sales_dt)
  mean_sales  <- mean(sales_dt$sales_per_model, na.rm = TRUE)
  median_sales<- median(sales_dt$sales_per_model, na.rm = TRUE)
  max_sales   <- max(sales_dt$sales_per_model, na.rm = TRUE)
  min_sales   <- min(sales_dt$sales_per_model, na.rm = TRUE)
  
  # Create a label for the annotation
  stats_label <- paste0("Models: ", n_models, "\n",
                        "Mean: ", round(mean_sales, 2), "\n",
                        "Median: ", round(median_sales, 2), "\n",
                        "Max: ", max_sales, "\n",
                        "Min: ", min_sales)

 
  # Plot histogram of sales per model for 2010
  ggplot(sales_dt, aes(x = sales_per_model)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black") +
    labs(
      x = "Sales per Model",
      y = "Frequency"
    ) +
    theme_light() +
    annotate("text", x = Inf, y = Inf, label = stats_label, 
             hjust = 1.1, vjust = 1.1, size = 5)
  ggsave(glue("output/figures/statdesc/sales_distribution_{y}.pdf"), width = 10, height = 8)
}

# Setting different quality criteria --------------------------------------

models_dt = unique(tts[, .(module_model, efficiency_module, year)])

for (y in c(2011, 2013, 2017, 2019)){
  ggplot(models_dt[year == `y`], aes(x = efficiency_module))+
    geom_histogram(bins = 30, fill = "steelblue", color = "black") +
    labs(
      x = "Efficiency per model",
      y = "Frequency"
    ) +
    theme_light() 
  ggsave(glue("output/figures/statdesc/efficiency_distrib_{y}.pdf"), width = 10, height = 8)
}

pct_2012 = quantile(models_dt[year == 2011]$efficiency, probs = c(0.5, 0.9, 0.95))
pct_2013 = quantile(models_dt[year == 2013]$efficiency, probs = c(0.5, 0.9, 0.95))
pct_2017 = quantile(models_dt[year == 2017]$efficiency, probs = c(0.5, 0.9, 0.95))
pct_2019 = quantile(models_dt[year == 2019]$efficiency, probs = c(0.5, 0.9, 0.95))

pct_dt = data.frame(
  Year = c(2012, 2013, 2017, 2019),
  p50 = c(pct_2012[1],pct_2013[1],pct_2017[1],pct_2019[1]),
  p90 = c(pct_2012[2],pct_2013[2],pct_2017[2],pct_2019[2]),
  p95 = c(pct_2012[3],pct_2013[3],pct_2017[3],pct_2019[3])
)

pct_table = gt(pct_dt) %>% 
  as_latex() %>%
  as.character()
writeLines(pct_table, "output/tables/statdesc/quality_pct.tex")

# Premium > 20% 

# Combo inverter + high efficiency

# Maybe use a regression to determine the threshold for quality empirically (regress efficiency)
# SolarReview grades of brands
