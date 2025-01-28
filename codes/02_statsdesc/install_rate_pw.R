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
ts_brands = data.table(read_parquet(data_final("TTS_brands.parquet")))

#quick graph to see the number of reported installion per year
ts_plot[, install_peryear := .N, by = c("year")]
install_rate = unique(ts_plot[, .(year, install_peryear)])

ts_plot[, install_pw_year := mean(PV_system_size_DC, na.rm = TRUE), by = (year)]
install_size = unique(ts_plot[, .(year, install_pw_year)])

install = merge(install_rate, install_size, by = c("year"))
install[, log_install_pw_year := log(install_pw_year)]

scale_factor <- max(install$install_peryear, na.rm = TRUE) / max(install$log_install_pw_year, na.rm = TRUE)

ggplot(install, aes(x = year)) +
  geom_col(aes(y = install_peryear), fill = "blue", alpha = 0.7) +  # Bar chart for installations
  geom_line(aes(y = log_install_pw_year * scale_factor), color = "red", linewidth = 1.2, linetype = "dashed") +  # Line for avg power capacity
  scale_y_continuous(
    name = "Installations per year",  # Left y-axis label
    labels = label_comma(),
    sec.axis = sec_axis(
      ~ . / scale_factor,  # Transform back to original scale for avg power capacity
      name = "Average Installed Power Capacity (log(kW))"
    )
  ) +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black") +
  annotate(
    "text", x = 2012, y = max(install$install_peryear, na.rm = TRUE) * 0.65, 
    label = "Tariff", angle = 45, vjust = -1.5, color = "black"
  ) +
  annotate(
    "text", x = 2015, y = max(install$install_peryear, na.rm = TRUE) * 0.65, 
    label = "Tariff", angle = 45, vjust = -1.5, color = "black"
  ) +
  annotate(
    "text", x = 2018, y = max(install$install_peryear, na.rm = TRUE) * 0.65, 
    label = "Tariff", angle = 45, vjust = -1.5, color = "black"
  ) +
  labs(
    x = "Year",
    title = "Number of Installations and Average Installed Power Capacity"
  ) +
  theme(
    axis.title.y.right = element_text(color = "red"),  # Right axis color
    axis.text.y.right = element_text(color = "red"),   # Right axis text color
    axis.title.y.left = element_text(color = "blue"),  # Left axis color
    axis.text.y.left = element_text(color = "blue")    # Left axis text color
  )
ggsave("output/figures/statdesc/install_rate_pw_year.pdf")
