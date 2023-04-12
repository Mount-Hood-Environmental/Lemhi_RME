# Author: Mike Ackerman
# Purpose: get tagging details for the Lemhi River, parse them by various groups, and write tag lists for upload to PTAGIS
#
# Created: March 30, 2023
# Last Modified: April 3, 2023

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(here)
library(janitor)

# read in tagging details
lem_chnk_tag_deets = list.files(here("analysis/data/raw_data/tagging_details"), pattern = "*.csv", full.names = T) %>%
  map_df(~read_csv(., show_col_types = F, col_types = cols(
    `Species Code` = col_double(),
    `Run Code` = col_double()
    ))) %>%
  as_tibble() %>%
  clean_names() %>%
  # filter for just Chinook salmon, e-fishing and screw traps
  filter(species_name == "Chinook",
         capture_method_code == "SHOCK" | capture_method_code == "SCREWT",
         # filter out some very small or large sizes
         length_mm >= 50,
         length_mm <= 200,
         # no e-fishing prior to 2009
         mark_year_yyyy >= 2009) %>%
  mutate(juv_stage = case_when(
    mark_month_number > 8 ~ "Presmolt",
    mark_month_number < 7 ~ "Smolt",
    between(mark_month_number, 7, 8) ~ "Parr"
  )) %>%
  mutate(mark_site_code_value = case_when(
    mark_site_code_value %in% c("LEMHIW") ~ "LEMTRP",
    TRUE ~ mark_site_code_value
  )) %>%
  mutate(brood_year_yyyy = if_else(juv_stage == "Smolt",
                                   mark_year_yyyy - 2,
                                   mark_year_yyyy - 1))

# write tag lists to file, by capture method and brood year
lem_chnk_tag_deets %>%
  split(list(.$capture_method_code, .$brood_year_yyyy)) %>%
  map(.f = function(x) {
    x %>%
      select(tag_code) %>%
      distinct() %>%
      write_delim(file = here("analysis/data/raw_data/tag_lists/lem_surv",
                              paste0("BY",
                                     unique(x$brood_year_yyyy),
                                     "_",
                                     unique(x$capture_method_code),
                                     "_tags.txt")),
                  delim = "\n",
                  col_names = F)
  })

# save tagging details
write_rds(lem_chnk_tag_deets,
          file = "S:/main/data/fish/lem_surv/lemhi_tagging_details.rds")

# END SCRIPT
