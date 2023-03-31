# Author: Mike Ackerman
# Purpose: get tagging details for the Lemhi River, parse them by various groups, and write tag lists for upload to PTAGIS
#
# Created: March 30, 2023
# Last Modified:

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(here)
library(janitor)

# read in tagging details
lem_tag_deets = list.files(here("analysis/data/raw_data/tagging_details"), pattern = "*.csv", full.names = T) %>%
  map_df(~read_csv(., show_col_types = F, col_types = cols(
    `Species Code` = col_double(),
    `Run Code` = col_double()
    ))) %>%
  as_tibble() %>%
  clean_names() %>%
  # filter for just Chinook salmon and steelhead, e-fishing and screw traps
  filter(species_name == c("Chinook", "Steelhead"),
         capture_method_code == c("SHOCK", "SCREWT"),
         # not many tags prior to 2003
         mark_year_yyyy >= 2003,
         # filter out some very small or large sizes
         length_mm >= 50,
         length_mm <= 250)

tabyl(lem_tag_deets$mark_year_yyyy)
tabyl(lem_tag_deets$length_mm)

# write tag lists to file (by mark year)
lem_tag_deets %>%
  split(list(.$mark_year_yyyy)) %>%
  map(.f = function(x) {
    x %>%
      select(tag_code) %>%
      distinct() %>%
      write_delim(file = here("analysis/data/raw_data/tag_lists/lem_surv",
                              paste0("MY",
                                     unique(x$mark_year_yyyy),
                                     "_tags.txt")),
                  delim = "\n",
                  col_names = F)
  })

# write tag lists to file (by species & mark year)
lem_tag_deets %>%
  split(list(.$species_name, .$mark_year_yyyy)) %>%
  map(.f = function(x) {
    x %>%
      select(tag_code) %>%
      distinct() %>%
      write_delim(file = here("analysis/data/raw_data/tag_lists/lem_surv",
                              paste0(unique(x$species_name),
                                     "_MY",
                                     unique(x$mark_year_yyyy),
                                     "_tags.txt")),
                  delim = "\n",
                  col_names = F)
  })

# save tagging details
write_rds(lem_tag_deets,
          file = here('analysis/data/derived_data/lemhi_taggging_details.rds'))
