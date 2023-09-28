################################################################################
####################### TimTam & EBOV in Sierra Leone ##########################
################################################################################

########################### TimTam Slam data prep ##############################

library(timtamslamR)

source("TimTam_EBOV_DataWrangling.R")

############################# Prep genomic data ################################
## Read fasta
slam_fasta <- timtamslamR::read_fasta(input_fasta)

## Convert collection dates to days from origin while distributing them
## uniformly across the collection day.
slam_fasta_days <- timtamslamR::rename_dates_to_times_a(slam_fasta)

# Plot collection date distribution (by time of day) as a check
timtamslamR::plot_times(slam_fasta_days)

# Get 'present' time from genomic sequences (spread across collection days)
present <- get_present(slam_fasta, slam_fasta_days)


############################## Prep case data ##################################
## Spread weekly aggregate cases into week days for time series
slam_time_series <- time_series |>
  select(-mid_date) |>
  rename("week_start" = "start",
         "week_end" = "end",
         "count" = "total") |>
  timtamslamR::spread_across_days()

slam_time_series_p <- rename_time_series(present, slam_time_series)


########################### Print disaster times ###############################
print("Here are the disaster sizes:\n")
paste(slam_time_series_p$count, sep = "", collapse = " ")
print("Here are the backward-times of the disasters:\n")
paste(slam_time_series_p$bwd_times, sep = "", collapse = " ")


############################## Export files ####################################
## Write sequence alignment with re-coded sampling dates
write_fasta(slam_fasta_days,
            "processed_files/dataset_B_cds_recoded_dates.fasta")


# TODO -Include prevalence estimate before first sequence
