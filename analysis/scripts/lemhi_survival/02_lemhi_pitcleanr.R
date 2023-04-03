# Author: Mike Ackerman
# Purpose: Set up configuration and parent-child tables, prep complete tag histories for CJS model
#
# Created: April 3, 2023
# Last Modified:

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(here)

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
    site_code %in% c("Mc1", "MC2")                         ~ "MCN", # McNary Oregon Shore Ladder, McNary Washington Shore Ladder,
    site_code %in% c("JDALD1", "JDALD2")                   ~ "JO1", # Release into South Fish Ladder, Release into North Fish Ladder
    site_code %in% c("TDA", "TD2")                         ~ "TD1", # The Dalles Dam, The Dalles North Fish Ladder
    site_code %in% c("B2J", "BCC", "B1J", "BVX")           ~ "BOJ", # Combine all juvenile sites at Bonneville
    site_code %in% c("BO1", "BO2", "BO3", "BO4", "BONAFF") ~ "BON", # Combine all adult sites at Bonneville
    TRUE ~ node
  ))

# I need to fix the below to better reflect the sites in the Lemhi that we're interested in
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

# create tibble of "cases"
cases = expand.grid(2007:2021,
                    c("SCREWT", "SHOCK")) %>%
  mutate(case = paste0("BY", Var1, "_", Var2)) %>%
  select(case) %>%
  filter(case != "BY2007_SHOCK") %>%
  as_tibble()

# get observation data from all brood years and both capture methods
obs_df = cases %>%
  mutate(ptagis_raw = map(cases,
                          .f = function(cs) {
                            readCTH(here("analysis/data/raw_data/PTAGIS/lem_surv",
                                         paste0(cs, ".csv")))
                          })) %>%
  # compress PTAGIS detections
  mutate(comp = map(ptagis_raw,
                    .f = function(x) {
                      compress(x,
                               configuration = config_file,
                               max_minutes = 60 * 24 * 10,
                               units = "days",
                               ignore_event_vs_release = T)
                    }))

# save(config_file,
#      parent_child,
#      obs_df,
#      file = "S:/main/data/fish/lem_surv/lem_survival.Rdata")

load("S:/main/data/fish/lem_surv/lem_survival.Rdata")

