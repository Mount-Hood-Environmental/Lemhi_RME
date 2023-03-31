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
lem_chnk_tag_deets = list.files(here("analysis/data/raw_data/tagging_details"), pattern = "*.csv", full.names = T) %>%
  map_df(~read_csv(., show_col_types = F, col_types = cols(
    `Species Code` = col_double(),
    `Run Code` = col_double()
    ))) %>%
  as_tibble() %>%
  clean_names() %>%
  # filter for just Chinook salmon and steelhead, e-fishing and screw traps
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
  mutate(brood_year_yyyy = if_else(juv_stage == "Smolt",
                                   mark_year_yyyy - 2,
                                   mark_year_yyyy - 1))

tabyl(lem_chnk_tag_deets, mark_year_yyyy, capture_method_code)

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
# write_rds(lem_chnk_tag_deets,
#           file = here('analysis/data/derived_data/lemhi_tagging_details.rds'))

# load PITcleanr
# remotes::install_github("mackerman44/PITcleanr@main")
library(PITcleanr)

# build a configuration file, recode and/or combine some sites
config_file = buildConfig() %>%
  mutate(node = site_code) %>%
  # GRS = Lower Granite Dam Spillway
  mutate(node = case_when(
    site_code %in% c("GRS")                                ~ "GRJ", # Lower Granite Dam Spillway
    site_code %in% c("LGRLDR")                             ~ "GRA", # LGR - Release into the Adult Fish Ladder
    site_code %in% c("IHA", "IHR")                         ~ "ICH", # Ice Harbor Adult, Ice Harbor Dam
    site_code %in% c("Mc1", "MC2")                          ~ "MCN", # McNary Oregon Shore Ladder, McNary Washington Shore Ladder,
    site_code %in% c("JDALD1", "JDALD2")                   ~ "JO1", # Release into South Fish Ladder, Release into North Fish Ladder
    site_code %in% c("TDA", "TD2")                         ~ "TD1", # The Dalles Dam, The Dalles North Fish Ladder
    site_code %in% c("B2J", "BCC", "B1J", "BVX")           ~ "BOJ", # Combine all juvenile sites at Bonneville
    site_code %in% c("BO1", "BO2", "BO3", "BO4", "BONAFF") ~ "BON", # Combine all adult sites at Bonneville
    TRUE ~ node
  ))

# create parent-child table
parent_child = tribble(~parent, ~child,
                       "LEMHI", "LLR",
                       "LLR", "GRJ",
                       "GRJ", "GOJ",
                       "GOJ", "LMJ",
                       "LMJ", "ICH",
                       "ICH", "MCJ",
                       "MCJ", "JDJ",
                       "JDJ", "BOJ",
                       "BOJ", "BON",
                       "BON", "TD1",
                       "TD1", "JO1",
                       "JO1", "MCN",
                       "MCN", "ICH",
                       "ICH", "LMA",
                       "LMA", "GOA",
                       "GOA", "GRA",
                       "GRA", "USE")

# plot parent-child table
plotNodes(parent_child = parent_child)

# get observation data from all brood years and both capture methods
cases = expand.grid(2007:2021,
                  c("SCREWT", "SHOCK")) %>%
  mutate(case = paste0("BY",
                       Var1,
                       "_",
                       Var2)) %>%
  select(case) %>%
  filter(case != "BY2007_SHOCK") %>%
  as_tibble()

obs_df = cases %>%
  mutate(ptagis_raw = map(cases,
                          .f = function(cs) {
                            readCTH(here("analysis/data/raw_data/PTAGIS/lem_surv",
                                         paste0(cs, ".csv")))
                          }))



