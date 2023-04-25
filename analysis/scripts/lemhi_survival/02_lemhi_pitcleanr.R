# Author: Mike Ackerman
# Purpose: Set up configuration and parent-child tables, prep complete tag histories for CJS model
#
# Created: April 3, 2023
# Last Modified: April 21, 2023

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(here)
library(sf)
library(janitor)

# load PITcleanr
# remotes::install_github("mackerman44/PITcleanr@main", build_vignettes = T, force = T)
library(PITcleanr)
browseVignettes("PITcleanr")

# read in tagging details, as needed
lem_chnk_tag_deets = read_rds("S:/main/data/fish/lem_surv/lemhi_tagging_details.rds")

#tabyl(lem_chnk_tag_deets, mark_site_code_value, brood_year_yyyy)
tabyl(lem_chnk_tag_deets, mark_site_code_value, mark_year_yyyy)
# mark_site_code_value 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022
#               18MILC    0    0    0    0    0    0    0    0    0    0    0    3    0    0
#               BIG8MC    0    0    0    0    0    1   90   24    6    1   75    5    0    0
#               BIGSPC    0    0   56   28    6   24   77   81   35    8  146  145    0    0
#               BOHANC    0    0    0    0    0    1    9   13    2    1    0    0    0    0
#               BTIMBC    0    0    0   24    4    4   43   42    8    1   39  129   24    7
#               CANY2C    0    0    2   39    1    0   49   55    1    1   23   32    0    1
#               HAYDNC 2035 1709 2029 3126 2485 1899 1888 2276 2697  281  481 1032  387  728
#               HYDTRP    0    0    0    0    0    0    0    0    0  252  654 1445  721   50
#                KENYC    0    0    0    1    0    0    0    1    0    0    0    0    0    0
#                 LEEC    0    0    0    0    0    1   37   47   11    8  145   27    0    0
#               LEMHIR  104  949  504  882  232  189 1500 2467 1648 1259 1021  907    0    0
#               LEMTRP  691 4467 3968 3117 1490 2952 3059 3284 3164 1849 4193 2133 2900 5137
#                LLRTP 1175 2361 1726    0 4523 2975 6393 6179 6072 4098 3947 5012 4367 1753
#               LLSPRC    0    0    0    7   10    3   26   14    2   12    8   62   20    6
#               WIMPYC    0    0    0    0    4    0   16    5    3    0    0    0    0    0

# LEMHIW rkm: 522.303.416.049
# LEMTRP rkm: 522.303.416.049
# LLRTP  rkm: 522.303.416.007
# LEMHIR rkm: 522.303.416.___

# build a configuration file, recode and/or combine some sites
config_file = buildConfig() %>%
  mutate(node = site_code) %>%
  # GRS = Lower Granite Dam Spillway
  mutate(node = case_when(
    site_code %in% c("LEMHIW")                                 ~ "LEMTRP", # Lemhi River Weir -> Upper Lemhi River Rotary Screw Trap
    site_code %in% c("GRS")                                    ~ "GRJ",    # Lower Granite Dam Spillway
    site_code %in% c("LGRLDR")                                 ~ "GRA",    # LGR - Release into the Adult Fish Ladder
    site_code %in% c("IHA", "IHR")                             ~ "ICH",    # Ice Harbor Adult, Ice Harbor Dam
    site_code %in% c("Mc1", "MC2")                             ~ "MCN",    # McNary Oregon Shore Ladder, McNary Washington Shore Ladder,
    site_code %in% c("JDALD1", "JDALD2")                       ~ "JO1",    # Release into South Fish Ladder, Release into North Fish Ladder
    site_code %in% c("TDA", "TD2")                             ~ "TD1",    # The Dalles Dam, The Dalles North Fish Ladder
    site_code %in% c("B2J", "BCC", "B1J", "BVX")               ~ "BOJ",    # Combine all juvenile sites at Bonneville
    site_code %in% c("BO1", "BO2", "BO3", "BO4", "BONAFF")     ~ "BON",    # Combine all adult sites at Bonneville (not necessary as we may not be using it here)
    TRUE ~ node
  )) %>%
  # Finally, combine all juvenile detections below GRJ into a single node
  mutate(node = case_when(
    node %in% c("GOJ", "LMJ", "ICH", "MCJ", "JDJ", "BOJ") ~ "BLW_GRJ",
    TRUE ~ node
  ))

# our sites of interest
nodes_of_interest = c("18MILC", "CANY2C", "BTIMBC", "BIGSPC", "BIG8MC", "LEEC", "LLSPRC", "LEMHIR", "LEMTRP", # upper Lemhi tribs, ending with trap
                      "HAYDNC", "HAYDTRP", "HYC",                                                             # Hayden Creek, ending with trap
                      "KENYC", "WIMPYC", "BOHANC", "LLRTP",                                                   # lower Lemhi tribs, with trap
                      "LLR", "GRJ", "BLW_GRJ")                                                                # LLR and juvenile hydrosystem

#-----------------------------------------------------------------
# code inserted by Kevin
#-----------------------------------------------------------------

# create sf of sites of interest
sites_sf = config_file %>%
  filter(node %in% nodes_of_interest) %>%
  group_by(site_code) %>%
  filter(config_id == max(config_id)) %>%
  ungroup() %>%
  select(node,
         site_code,
         site_name,
         site_type = site_type_name,
         type = site_type,
         rkm,
         rkm_total,
         site_description = site_description,
         latitude, longitude) %>%
  distinct() %>%
  arrange(node) %>%
  group_by(node) %>%
  filter(rkm_total == min(rkm_total)) %>%
  slice(1) %>%
  ungroup() %>%
  st_as_sf(coords = c("longitude",
                      "latitude"),
           crs = 4326)

# download the NHDPlus v2 flowlines
# do you want flowlines downstream of root site? Set to TRUE if you have downstream sites
dwn_flw = T
nhd_list = queryFlowlines(sites_sf = sites_sf,
                          root_site_code = "LLR",
                          min_strm_order = 2,
                          dwnstrm_sites = dwn_flw,
                          dwn_min_stream_order_diff = 4)

# compile the upstream and downstream flowlines
flowlines = nhd_list$flowlines
if(dwn_flw) {
  flowlines <- flowlines
    rbind(nhd_list$dwn_flowlines)
}


# plot the flowlines and the sites
ggplot() +
  geom_sf(data = flowlines |>
            filter(str_detect(REACHCODE, "^17060204000")),
          aes(color = as.factor(StreamOrde),
              size = StreamOrde)) +
  scale_color_viridis_d(direction = -1,
                        option = "D",
                        name = "Stream\nOrder",
                        end = 0.8) +
  scale_size_continuous(range = c(0.2, 1.2),
                        guide = 'none') +
  geom_sf(data = nhd_list$basin,
          fill = NA,
          lwd = 2) +
  geom_sf(data = sites_sf %>%
            filter(str_detect(rkm, "522.303.416")),
          size = 4,
          color = "black") +
  # geom_sf_label(data = sites_sf %>%
  #                 filter(str_detect(rkm, "522.303.416")),
  #               aes(label = site_code)) +
  ggrepel::geom_label_repel(
    data = sites_sf %>%
      filter(str_detect(rkm, "522.303.416"),
             site_code != "LLR"),
    aes(label = site_code,
        geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0
  ) +
  geom_sf_label(data = sites_sf %>%
                  filter(site_code == "LLR"),
                aes(label = site_code),
                color = "red") +
  theme_bw() +
  theme(axis.title = element_blank())



# create parent-child table based on site locations and flowlines
parent_child = sites_sf %>%
  buildParentChild(flowlines,
                   # rm_na_parent = T,
                   add_rkm = F) %>%
  editParentChild(fix_list =
                    list(c(NA, "GRJ", "B1J"),
                         c(NA, "LLR", "GRJ"),
                         c("B1J", "LLRTP", "LLR"),
                         c("GRJ", "LLRTP", "LLR"),
                         c("BIGSPC", "BTIMBC", "LEMHIW"),
                         c("BIGSPC", "CANY2C", "LEMHIW"),
                         c("BIGSPC", "18MILC", "LEMHIW"),
                         c("LEMHIR", "HYC", "LLRTP"),
                         c("LLSPRC", "BIGSPC", "LEMHIW"),
                         c("LLSPRC", "BIG8MC", "LEMHIW"),
                         c("LLSPRC", "LEEC", "LEMHIW"))) %>%
  filter(!is.na(parent))

plotNodes(parent_child)


buildNodeOrder(parent_child,
               direction = "d") |>
  mutate(across(node,
                ~ factor(.,
                         levels = nodes_of_interest))) %>%
  arrange(node, node_order)

# flip parent and child relationships to reflect downstream movement
parent_child <- parent_child %>%
  rename(p = parent,
         ph = parent_hydro,
         c = child,
         ch = child_hydro) %>%
  rename(parent = c,
         parent_hydro = ch,
         child = p,
         child_hydro = ph) |>
  select(parent,
         child,
         parent_hydro,
         child_hydro) |>
  arrange(desc(child_hydro),
          desc(parent_hydro))



#-----------------------------------------------------------------
# end of Kevin's code
#-----------------------------------------------------------------



# let's look at some sites
sites_sf %>%
  select(node, site_code, site_type, geometry) %>%
  # just look at lemhi sites
  filter(!node %in% c("GRJ", "BLW_GRJ")) %>%
  distinct() %>%
  ggplot() +
  geom_sf(aes(colour = site_type)) +
  geom_sf_text(aes(label = site_code),
               size = 3) +
  theme(legend.position = "top",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  labs(colour = "Site Type")

# write to shapefile
# sites_sf %>%
#   st_write(here("analysis/data/derived_data/sites_sf.shp"), append = T)

# create parent-child table
parent_child = tribble(~parent, ~child,
                       "18MILC", "LEMTRP",
                       "CANY2C", "LEMTRP",
                       "BTIMBC", "LEMTRP",
                       "BIGSPC", "LEMTRP",
                       "BIG8MC", "LEMTRP",
                       "LEEC",   "LEMTRP",
                       "LLSPRC", "LEMTRP",
                       "HAYDNC", "HYDTRP",
                       "HYDTRP", "HYC",
                       "HYC",    "LLRTP",
                       "LEMTRP", "LLRTP",
                       "KENYC",  "LLRTP",
                       "WIMPYC", "LLRTP",
                       "BOHANC", "LLRTP",
                       "LLRTP",  "LLR",
                       "LLR",    "GRJ",
                       "GRJ",    "BLW_GRJ")

# plot parent-child table
plotNodes(parent_child = parent_child)
# as of 4/21/23, the parent-child table is not being used. But if it does get used, could use a review, maybe from Kevin.

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
  mutate(brood_year = str_extract(cases, "[:digit:]+"),
         across(brood_year,
                as.numeric),
         capture_method = str_split(cases, "_", simplify = T)[,2]) %>%
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
                    })) %>%
  # convert compressed ptagis cths into capture histories
  mutate(ch = map(comp,
                  .f = function(comp_obs) {
                    comp_obs %>%
                      filter(node %in% nodes_of_interest) %>%
                      mutate(node = factor(node,
                                           levels = nodes_of_interest)) %>%
                      select(tag_code, node) %>%
                      distinct() %>%
                      mutate(seen = 1) %>%
                      # convert to wide format
                      pivot_wider(names_from = node,
                                  values_from = seen,
                                  values_fill = 0,
                                  names_sort = T,
                                  names_expand = T) %>% # makes sure all nodes are includes in ch's
                      unite(ch, 2:(length(nodes_of_interest) + 1), sep = "") %>%
                      # grab information of interest from tagging details
                      left_join(lem_chnk_tag_deets %>%
                                  select(tag_code,
                                         mark_site_code_value,
                                         brood_year_yyyy,
                                         capture_method_code,
                                         length_mm,
                                         weight_g,
                                         juv_stage))
                  }))

# convert compressed ptagis cths into capture histories
# ch_df = obs_df$comp[[12]] %>%
#   filter(node %in% nodes_of_interest) %>%
#   mutate(node = factor(node,
#                        levels = nodes_of_interest)) %>%
#   select(tag_code, node) %>%
#   distinct() %>%
#   mutate(seen = 1) %>%
#   pivot_wider(names_from = node,
#               values_from = seen,
#               values_fill = 0,
#               names_sort = T,
#               names_expand = T) %>%
#   unite(ch, 2:(length(nodes_of_interest) + 1), sep = "") %>%
#   left_join(lem_chnk_tag_deets %>%
#               select(tag_code,
#                      mark_site_code_value,
#                      brood_year_yyyy,
#                      capture_method_code,
#                      length_mm,
#                      weight_g,
#                      juv_stage))

save(config_file,
     nodes_of_interest,
     obs_df,
     file = "S:/main/data/fish/lem_surv/lem_survival.Rdata")

# END SCRIPT

