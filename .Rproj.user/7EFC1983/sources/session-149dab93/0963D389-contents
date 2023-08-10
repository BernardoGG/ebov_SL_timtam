################################################################################
####################### TimTam & EBOV in Sierra Leone ##########################
################################################################################

########################### TimTam Slam data prep ##############################

library(timtamslamR)
library(patchwork)

#source("TimTam_EBOV_DataWrangling.R")

################################# Script #######################################

## Read fasta
slam_fasta <- timtamslamR::read_fasta(input_fasta)
timtamslamR::plot_dates(slam_fasta)

## Convert collection dates to days from origin
slam_fasta_days <- timtamslamR::rename_dates_to_times_a(slam_fasta)
timtamslamR::plot_times(slam_fasta_days)

## Compare distribution of sequences over time from both methods
home_method <- ggplot(seq_meta, aes(x = collection_date)) +
  geom_bar() +
  labs(x = "Date", y = "Number of sequences\n(daily)") +
  xlim(min(ebov_sl_cases$start), max(ebov_sl_cases$end))

slam_method <- ggplot(
  data.frame(new_dates =
               sub(".*\\|([^|]+)$", "\\1", labels(slam_fasta_days)) %>%
               as.numeric() %>% sort()), aes(x = new_dates)) +
  geom_histogram(bins = 50) +
  labs(x = "Days from origin", y = "Number of sequences")


home_method / slam_method

