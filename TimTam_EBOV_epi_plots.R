################################################################################
####################### TimTam & EBOV in Sierra Leone ##########################
################################################################################

#################### Epidemiological trends summary plots ######################

library(patchwork)

source("ebov-expgrowth-example-main/TimTam_EBOV_time_matching.R")

############################## Summary plots ###################################
## Plot cases and sequences by epidemiological week
ggplot(ebov_sl_cases, aes(x = epiweek, y = total)) +
  geom_col(fill = "lightgrey") +
  geom_line(aes(x = epiweek, y = sequenced),
            color = "darkred", linewidth = 1) +
  scale_y_continuous(name = "Number of cases",
                     sec.axis = sec_axis(
                       trans = ~.*1,
                       name = "Number of genomic sequences")) +
  labs(x = "Epiweek", y = "Number of cases") +
  theme_minimal() +
  theme(axis.title.y.right = element_text(color = "darkred"),
        axis.text.y.right = element_text(color = "darkred"))

## Plot sequences by epidemiological week and by day
x <- ggplot(ebov_sl_cases, aes(x = mid_date, y = sequenced)) +
  geom_col() +
  labs(x = "Date (mid-week)",
       y = "Number of sequences\n(aggregated over epiweeks)") +
  theme_minimal()

y <- ggplot(seq_meta, aes(x = collection_date)) +
  geom_bar() +
  labs(x = "Date", y = "Number of sequences\n(daily)") +
  xlim(min(ebov_sl_cases$start), max(ebov_sl_cases$end))

x / y

## Plot cases and sequences
x <- ggplot(time_series_uniform, aes(x = date, y = count)) +
  geom_col() +
  labs(x = "Date",
       y = "Number of cases (uniformly\ndistributed over epiweeks)") +
  theme_minimal()

y <- ggplot(seq_meta, aes(x = collection_date)) +
  geom_bar() +
  labs(x = "Date", y = "Number of sequences\n(daily)") +
  xlim(min(ebov_sl_cases$start), max(ebov_sl_cases$end))

x / y
