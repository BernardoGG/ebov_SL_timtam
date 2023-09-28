################################################################################
####################### TimTam & EBOV in Sierra Leone ##########################
################################################################################

#################### Epidemiological trends summary plots ######################

library(ggpattern)

source("TimTam_EBOV_time_matching.R")

############################## Summary plots ###################################
epi_plot <-
  ggplot() +
  geom_col_pattern(
    data = ebov_sl_cases |> select(start, sequenced, unsequenced) |>
      rename("Sequences" = "sequenced", "Cases" = "unsequenced") |>
      pivot_longer(!start, names_to = "data_type", values_to = "count"),
    mapping = aes(x = start,
                  y = count,
                  pattern = data_type),
    fill = "white",
    pattern_spacing = 0.015, pattern_angle = 45
  ) +
  scale_pattern_manual(
    values = c("stripe", "crosshatch")
  ) +
  scale_x_date(
    date_breaks = "2 weeks",
    date_labels = "%b %d",
    limits = c(start_date, end_date),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 120),
    breaks = seq(0, 120, 20),
    expand = c(0, 2)
  ) +
  labs(x = NULL, y = NULL, pattern = NULL, pattern_angle = NULL) +
  theme_bw() +
  theme(
    legend.position = c(0.2, 0.8)
  )

## Export plot
ggsave(filename = "plots/ebov_SL_epidemic_plot.png",
       plot = epi_plot,
       height = 0.7 * 14.8, width = 21.0,
       units = "cm")
