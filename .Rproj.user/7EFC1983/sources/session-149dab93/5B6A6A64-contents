################################################################################
####################### TimTam & EBOV in Sierra Leone ##########################
################################################################################

############### Print disaster data script for the EBOV data ###################
#################### adapted from Alex Zarebski script #########################

source("TimTam_EBOV_time_matching.R")

####################### Get disaster times and sizes ###########################
disaster_times <- str_flatten(
  string = (difftime(time_series_uniform$date,
                    end_date,
                    unit = 'days') + 0.5) * (-1),
  collapse = " "
)

disaster_sizes <- str_flatten(
  string = time_series_uniform$count,
  collapse = " "
)

change_times_weekly <- difftime(ebov_sl_cases$end, end_date,
                                units = 'days')[1:10] * (-1)
history_times <- str_flatten(
  string = change_times_weekly + 0.375,
  collapse = " "
)


######################## Print in console/terminal #############################
cat("\nDisaster times:\n")
cat(disaster_times)
cat("\n\nDisaster sizes:\n")
cat(disaster_sizes)
cat("\n\nHistory times:\n")
cat(history_times)
cat("\n")
