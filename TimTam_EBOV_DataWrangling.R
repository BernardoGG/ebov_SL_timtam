################################################################################
####################### TimTam & EBOV in Sierra Leone ##########################
################################################################################

############################## Data wrangling ##################################

library(tidyverse)
library(lubridate)
library(SGmisc)
library(ape)

############################### Import data ####################################
#### Files from *Dataset B* from Louis du Plessis 'Exponential growth' project.

## Time series (case count) data
ebov_sl_cases <- read.csv(
  "data/dataset_B_cases_conf.csv",
  sep = ",")

ebov_sl_cases$start <- as.Date(ebov_sl_cases$start)
ebov_sl_cases$end <- as.Date(ebov_sl_cases$end)
ebov_sl_cases$mid_date <- mid_date(ebov_sl_cases$start, ebov_sl_cases$end)

time_series <- ebov_sl_cases |> select(start, end, mid_date, total)

## Sequence metadata
seq_meta <- read.csv(
  "data/sequences_all.csv",
  sep = ",")
seq_meta$date <- as.Date(seq_meta$date)
seq_meta$collection_date <- as.Date(seq_meta$collection_date)
seq_meta <- seq_meta |> filter(date <= max(ebov_sl_cases$end) &
                                  country == "SLE" &
                                  location == "Kenema" | location == "Kailahun")

## Sequence data (.fasta)
input_fasta <- "data/dataset_B_cds.fasta"
fasta_seqs <- read.dna(input_fasta, format = "fasta")


########################### Extract information ################################
taxon_strings <- labels(fasta_seqs)

## Extract sequencing dates from genomic data
taxon_dates  <- taxon_strings |>
  str_extract(pattern = "(2014)[-][0-9][0-9][-][0-9][0-9].*$") |>
  as.Date() |>
  decimal_date()


############################## Export files ####################################

