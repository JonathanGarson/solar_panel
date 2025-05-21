# This script processes the output from stata

library(data.table)
library(ggplot2)

# Data --------------------------------------------------------------------

qfe = fread(data_final("quantile_fe_estimate.csv"))

# Prepping ----------------------------------------------------------------

qfe[, `:=` (lower = estimate - 1.96*se,
            upper = estimate + 1.96*se)]
qfe[, term := fcase(
  term == "2010-2013", "Anti-Dumping Tariff: 2010-2013",
  term == "2014-2016", "Anti-Dumping Tariff: 2014-2016",
  term == "2017-2018", "Trade War 2018",
  default = NA
)]

ggplot(qfe, aes(x = tau, y = estimate, color = term, fill = term)) +
  geom_line(size = 1) +
  geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Quantile",
    y = "Effect of Tariff on Panel Efficiency",
    # title = "Tariff Pass-Through to Efficiency Across Quantiles",
    color = "Event",
    fill = "Event"
  ) +
  theme_classic() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.65, 0.02),  # new argument!
    legend.justification = c("left", "bottom"),
    legend.background = element_rect(fill = "white", color = "grey80"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )
ggsave("output/regression/quality_shift/quality_quantile_fe.pdf", width = 10, heigh = 7)
