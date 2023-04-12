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
library(sf)

# load PITcleanr
# remotes::install_github("mackerman44/PITcleanr@main", build_vignettes = T, force = T)
browseVignettes("PITcleanr")
library(PITcleanr)

# read in tagging details, as needed
lem_chnk_tag_deets = read_rds("S:/main/data/fish/lem_surv/lemhi_tagging_details.rds")

# tabyl(lem_chnk_tag_deets, mark_site_code_value, mark_year_yyyy)
# mark_site_code_value 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022
#               18MILC    0    0    0    0    0    0    0    0    0    0    0    3    0    0 Eighteenmile Creek
#               BIG8MC    0    0    0    0    0    1   90   24    6    1   75    5    0    0 Big Eightmile Creek
#               BIGSPC    0    0   56   28    6   24   77   81   35    8  146  145    0    0 Big Springs Creek
#               BOHANC    0    0    0    0    0    1    9   13    2    1    0    0    0    0 Bohannon Creek
#               BTIMBC    0    0    0   24    4    4   43   42    8    1   39  129   24    7 Big Timber Creek
#               CANY2C    0    0    2   39    1    0   49   55    1    1   23   32    0    1 Canyon Creek
#               HAYDNC 2035 1709 2029 3126 2485 1899 1888 2276 2697  281  481 1032  387  728 Hayden Creek
#               HYDTRP    0    0    0    0    0    0    0    0    0  252  654 1445  721   50 Hayden Creek Rotary Screw Trap
#                KENYC    0    0    0    1    0    0    0    1    0    0    0    0    0    0 Kenney Creek
#                 LEEC    0    0    0    0    0    1   37   47   11    8  145   27    0    0 Lee Creek
#               LEMHIR 1279 3310 2230  882 4755 3164 7893 8646 7720 1259 1021  907    0    0 Lemhi River
#               LEMHIW  691 4467 3968 3117 1490 2952 3059 3284 3164    0    0    0    0    0 Lemhi River Weir
#               LEMTRP    0    0    0    0    0    0    0    0    0 1849 4193 2133 2900 5137 Upper Lemhi River Rotary Screw Trap
#                LLRTP    0    0    0    0    0    0    0    0    0 4098 3947 5012 4367 1753 Lower Lemhi River Rotary Screw Trap
#               LLSPRC    0    0    0    7   10    3   26   14    2   12    8   62   20    6 Little Springs Creek
#               WIMPYC    0    0    0    0    4    0   16    5    3    0    0    0    0    0 Wimpey Creek

# LEMHIW rkm: 522.303.416.049
# LEMTRP rkm: 522.303.416.049
# LLRTP  rkm: 522.303.416.007
# LEMHIR rkm: 522.303.416.___

# LEMHIW should be recoded to LEMTRP
# Does LEMHIR need to be recoded to LLRTP, both in marking details and in configuration file?

# build a configuration file, recode and/or combine some sites
config_file = buildConfig() %>%
  mutate(node = site_code) %>%
  # GRS = Lower Granite Dam Spillway
  mutate(node = case_when(
    site_code %in% c("LEMHIW")                             ~ "LEMTRP", # Lemhi River Weir -> Upper Lemhi River Rotary Screw Trap
    site_code %in% c("GRS")                                ~ "GRJ",    # Lower Granite Dam Spillway
    site_code %in% c("LGRLDR")                             ~ "GRA",    # LGR - Release into the Adult Fish Ladder
    site_code %in% c("IHA", "IHR")                         ~ "ICH",    # Ice Harbor Adult, Ice Harbor Dam
    site_code %in% c("Mc1", "MC2")                         ~ "MCN",    # McNary Oregon Shore Ladder, McNary Washington Shore Ladder,
    site_code %in% c("JDALD1", "JDALD2")                   ~ "JO1",    # Release into South Fish Ladder, Release into North Fish Ladder
    site_code %in% c("TDA", "TD2")                         ~ "TD1",    # The Dalles Dam, The Dalles North Fish Ladder
    site_code %in% c("B2J", "BCC", "B1J", "BVX")           ~ "BOJ",    # Combine all juvenile sites at Bonneville
    site_code %in% c("BO1", "BO2", "BO3", "BO4", "BONAFF") ~ "BON",    # Combine all adult sites at Bonneville
    TRUE ~ node
  ))

# our sites of interest
sites_of_interest = c("18MILC", "BIG8MC", "BIGSPC", "BOHANC", "BTIMBC", "CANY2C", "HAYDNC", "KENYC", "LEEC", "LLSPRC", "WIMPYC", # tribs
                      "LEMHIR",                     # Lemhi mainstem
                      "HYDTRP", "LEMHIW", "LEMTRP", # upper traps
                      "LLRTP",                      # lower trap
                      "LLR",                        # lower Lemhi array
                      "GRJ", "GOJ", "LMJ", "ICH", "MCJ", "JDJ", "BOJ", "BON", "TD1", "JO1", "MCN", "ICH", "LMA", "GOA", "GRA", "USE")

# create sf of sites of interest
sites_sf = config_file %>%
  filter(node %in% sites_of_interest) %>%
  st_as_sf(coords = c("longitude",
                      "latitude"),
           crs = 4326)

# create tibble of "cases"
# only doing BY2015 - BY2020 for now to keep file sizes reasonable and BY2020 the last complete BY, for now
cases = expand.grid(2015:2020,
                    c("SCREWT", "SHOCK")) %>%
  mutate(cases = paste0("BY", Var1, "_", Var2)) %>%
  select(cases) %>%
  filter(cases != "BY2007_SHOCK") %>%
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

# convert compressed ptagis cths into capture histories
ch_df = obs_df$comp[[1]]


# This still needs to be revised to reflect the various sites in the Lemhi that we're interested in
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

# save(config_file,
#      parent_child,
#      obs_df,
#      file = "S:/main/data/fish/lem_surv/lem_survival.Rdata")

load("S:/main/data/fish/lem_surv/lem_survival.Rdata")

