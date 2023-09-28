################################################################################
####################### TimTam & EBOV in Sierra Leone ##########################
################################################################################

########################### TimTam Slam data prep ##############################

library(patchwork)

source("TimTam_EBOV_DataWrangling.R")
source("TimTam_EBOV_slam_datasets.R")


## Compare distribution of sequences over time from both methods
home_method <- ggplot(seq_meta, aes(x = collection_date)) +
  geom_bar() +
  labs(x = "Date", y = "Number of sequences\n(daily)") +
  xlim(min(ebov_sl_cases$start), max(ebov_sl_cases$end))

slam_method <- ggplot(
  data.frame(new_dates =
               sub(".*\\|([^|]+)$", "\\1", labels(slam_fasta_days)) %>%
               as.numeric() %>% sort() %>% floor()), aes(x = new_dates)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Days from origin", y = "Number of sequences")

home_method / slam_method

## Compare distribution of cases over time from both methods
ts_uniform_plot <- ggplot(time_series_uniform) +
  geom_col(aes(x = date, y = count)) +
  theme_minimal()

ts_slam_plot <- ggplot(slam_time_series) +
  geom_col(aes(x = date, y = count)) +
  theme_minimal()

ts_uniform_plot / ts_slam_plot