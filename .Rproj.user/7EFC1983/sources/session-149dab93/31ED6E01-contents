################################################################################
####################### TimTam & EBOV in Sierra Leone ##########################
################################################################################

########## Time span matching for genomic and epidemiological data #############

library(stringr)
library(magrittr)

source("ebov-expgrowth-example-main/TimTam_EBOV_DataWrangling.R")


########################## Set analysis time span ##############################
## Start time: first day of first epidemiological week in time series
start_date <- ebov_sl_cases$start |>
  as.Date() |>
  min()

## End time: last day of last collected genome sequence, coincides with last day
## of last epidemiological week in time series
end_date <- taxon_strings |>
  str_extract(pattern = "(2014)[-][0-9][0-9][-][0-9][0-9].*$") |>
  as.Date() |>
  max()


####################### Epi + genomic date matching ############################
### Given weekly case counts and individual dates from genome sequences, we take
### different approaches towards matching both sets of data.

### Approach 1: Uniform distribution of (scheduled) unsequenced cases across 
### epidemic weeks.

# Create data frame
time_series_uniform <- NULL
for(i in 1:nrow(time_series)){
  tmp <- sample(seq.Date(time_series$start[i], time_series$end[i],
                         by = "days"),
                time_series$total[i],
                replace = ifelse(time_series$total[i] <= 7,
                                 FALSE,
                                 TRUE)) |>
    table() |>
    as.data.frame()
  time_series_uniform <-  rbind(time_series_uniform, tmp)
}

time_series_uniform <- time_series_uniform |>
  mutate(Var1 = as.Date(Var1)) |>
  complete(Var1 = seq.Date(min(Var1), max(Var1), by="day")) |>
  mutate(Freq = replace_na(Freq, 0))

colnames(time_series_uniform) <-  c("date", "count")

# Plot to inspect visually
ggplot(time_series_uniform) +
  geom_col(aes(x = date, y = count)) +
  theme_minimal()


################## Change dates to relative epidemic days ######################
################### Adapted from Antoine Zwaans function #######################
taxa_days <- difftime(date_decimal(taxon_dates) |> as.Date(),
                      end_date,
                      unit = 'days') * (-1)

taxa_times <- numeric(length(taxa_days))
for (day in unique(taxa_days)) {
  day_mask <- taxa_days == day
  num_samples <- sum(day_mask)
  if (num_samples %% 2 == 0) {
    taxa_times[day_mask] <-
      seq(from = day, to = day + 1, length.out = num_samples + 2) |>
      head(-1) |>
      tail(-1)
  } else if (num_samples %% 2 == 1) {
    taxa_times[day_mask] <-
      seq(from = day, to = day + 1, length.out = num_samples + 2 + 1) |>
      head(-2) |>
      tail(-1)
  } else {
    stop("Could not make taxa sample times.")
  }
}


########### Export sequence new sampling 'dates' as epidemic days ##############
data.frame(taxa = taxon_strings,
           day = taxa_times) |>
  write.table("dataset_B_cds_sample_days.dat",
              sep="\t",
              row.names = FALSE,
              quote = FALSE)
