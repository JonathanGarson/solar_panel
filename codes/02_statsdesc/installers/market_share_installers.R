# This code compares the dispersion of market share for TPO and HO systems.

library(data.table)
library(ggplot2)

# Data --------------------------------------------------------------------

tts = read_parquet(data_final("TTS_final.parquet"))

# Manipulation ------------------------------------------------------------
setDT(tts)
installations <- unique(tts[, .(year, installer_name, ho, sum_installation)])
installations[, ho_label := factor(ho, levels = c(0, 1), labels = c("TPO", "HO"))]

ggplot(installations, aes(x = ho_label, y = log(sum_installation), fill = ho_label)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  facet_wrap(~ year) +
  labs(
    x = "System Type",
    y = "Log(Installation Count)",
    fill = "System Type",
  ) +
  theme_minimal()
ggsave("output/figures/installers/violin_plot.pdf", width = 10, height = 8)
