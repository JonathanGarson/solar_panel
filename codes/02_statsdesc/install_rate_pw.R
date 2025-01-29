library(arrow)
library(data.table)
library(ggplot2)
library(gt)
library(scales)

# Set theme ggplot --------------------------------------------------------

theme_set(theme_minimal() +
            theme(
              plot.title = element_text(hjust = 0.5),
              legend.position = "top"
            ))

# Data --------------------------------------------------------------------

ts_plot = as.data.table(read_parquet(data_temp("TTS_clean_names.parquet")))
ts_brands = data.table(read_parquet(data_final("TTS_brands_quarter.parquet")))

#quick graph to see the number of reported installion per year
ts_plot[, install_per_quarter := .N, by = c("year_quarter")]
install_rate = unique(ts_plot[, .(year_quarter, install_per_quarter)])

ts_plot[, install_grant_quarter := mean(rebate_or_grant, na.rm = TRUE), by = (year_quarter)]
install_rebate = unique(ts_plot[, .(year_quarter, install_grant_quarter)])

install = merge(install_rate, install_rebate, by = c("year_quarter"))
install[, log_install_grant_quarter := log(install_grant_quarter)]
install[, year := as.integer(as.numeric(year_quarter))]
# install[, install_grant_quarter := as.numeric(install_grant_quarter)]
# install[, log_install_grant_quarter := as.numeric(log_install_grant_quarter)]

scale_factor <- max(install$install_per_quarter, na.rm = TRUE) / max(install$log_install_grant_quarter, na.rm = TRUE)

ggplot(install[year %in% 2010:2022], aes(x = year_quarter)) +
  geom_col(aes(y = install_per_quarter), fill = "blue", alpha = 0.7) +  # Bar chart for installations
  geom_line(aes(y = log_install_grant_quarter * scale_factor), color = "red", linewidth = 1.2, linetype = "dashed") +  # Line for avg grant
  scale_y_continuous(
    name = "Installations per quarter",  # Left y-axis label
    labels = label_comma(),
    sec.axis = sec_axis(
      ~ . / scale_factor,  # Transform back to original scale for avg power capacity
      name = "Average Grant or Rebate (log($))"
    )
  ) +
  # Corrected Vertical Lines
  geom_vline(xintercept = as.yearqtr("2012 Q2", format = "%Y Q%q"), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.yearqtr("2014 Q3", format = "%Y Q%q"), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.yearqtr("2018 Q1", format = "%Y Q%q"), linetype = "dashed", color = "black") +
  annotate(
    "text", x = as.yearqtr("2012 Q2", format = "%Y Q%q"), y = max(install$install_per_quarter, na.rm = TRUE) * 0.65, 
    label = "Tariff", angle = 45, vjust = -1.5, color = "black"
  ) +
  annotate(
    "text", x = as.yearqtr("2014 Q3", format = "%Y Q%q"), y = max(install$install_per_quarter, na.rm = TRUE) * 0.65, 
    label = "Tariff", angle = 45, vjust = -1.5, color = "black"
  ) +
  annotate(
    "text", x = as.yearqtr("2018 Q1", format = "%Y Q%q"), y = max(install$install_per_quarter, na.rm = TRUE) * 0.65, 
    label = "Tariff", angle = 45, vjust = -1.5, color = "black"
  ) +
  labs(
    x = "Year",
    title = "Number of Installations and Average Grant or Rebate"
  ) +
  theme(
    axis.title.y.right = element_text(color = "red"),  # Right axis color
    axis.text.y.right = element_text(color = "red"),   # Right axis text color
    axis.title.y.left = element_text(color = "blue"),  # Left axis color
    axis.text.y.left = element_text(color = "blue")    # Left axis text color
  )
ggsave("output/figures/statdesc/install_rate_loggrant_quarter.pdf")

scale_factor <- max(install$install_per_quarter, na.rm = TRUE) / max(install$install_grant_quarter, na.rm = TRUE)

ggplot(install[year %in% 2010:2022], aes(x = year_quarter)) +
  geom_col(aes(y = install_per_quarter), fill = "blue", alpha = 0.7) +  # Bar chart for installations
  geom_line(aes(y = install_grant_quarter * scale_factor), color = "red", linewidth = 1.2, linetype = "dashed") +  # Line for avg grant
  scale_y_continuous(
    name = "Installations per quarter",  # Left y-axis label
    labels = label_comma(),
    sec.axis = sec_axis(
      ~ . / scale_factor,  # Transform back to original scale for avg power capacity
      name = "Average Grant or Rebate (log($))"
    )
  ) +
  # Corrected Vertical Lines
  geom_vline(xintercept = as.yearqtr("2012 Q2", format = "%Y Q%q"), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.yearqtr("2014 Q3", format = "%Y Q%q"), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.yearqtr("2018 Q1", format = "%Y Q%q"), linetype = "dashed", color = "black") +
  annotate(
    "text", x = as.yearqtr("2012 Q2", format = "%Y Q%q"), y = max(install$install_per_quarter, na.rm = TRUE) * 0.65, 
    label = "Tariff", angle = 45, vjust = -1.5, color = "black"
  ) +
  annotate(
    "text", x = as.yearqtr("2014 Q3", format = "%Y Q%q"), y = max(install$install_per_quarter, na.rm = TRUE) * 0.65, 
    label = "Tariff", angle = 45, vjust = -1.5, color = "black"
  ) +
  annotate(
    "text", x = as.yearqtr("2018 Q1", format = "%Y Q%q"), y = max(install$install_per_quarter, na.rm = TRUE) * 0.65, 
    label = "Tariff", angle = 45, vjust = -1.5, color = "black"
  ) +
  labs(
    x = "Year",
    title = "Number of Installations and Average Grant or Rebate"
  ) +
  theme(
    axis.title.y.right = element_text(color = "red"),  # Right axis color
    axis.text.y.right = element_text(color = "red"),   # Right axis text color
    axis.title.y.left = element_text(color = "blue"),  # Left axis color
    axis.text.y.left = element_text(color = "blue")    # Left axis text color
  )
ggsave("output/figures/statdesc/install_rate_grant_quarter.pdf")
