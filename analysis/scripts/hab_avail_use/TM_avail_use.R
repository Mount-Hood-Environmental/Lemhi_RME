# set working directory
setwd("C:/Git/Lemhi_RME/analysis/data/derived_data")
# load libraries
library(tidyverse)
library(skimr)
library(vroom)
library(lubridate)
library(forcats)
library(stringr)
library(here)
library(magrittr)
library(janitor)
library(sf)
library(EMT)
library(DescTools)
library(knitr)
library(kableExtra)
# isolate use file for tags that selected habitat
use_select <- xs_use %>%
filter(habitat_selected == "Selected",
       same_location_as_last_survey == "no")
# look at data use & select data
skim(use_select)
str(use_select)
# improve some column names
use <- rename(use_select,
              release_loc = location,
              release_date = release,
              xs_name = Name,
              stream_name = Stream_Nam,
              sin_cat = Category,
              frazil_ice = frazel_ice)%>%
  #correct for whitespace
  mutate(dominant_substrate_1mx1m = str_trim(dominant_substrate_1mx1m))%>%
  # correct the code for small side channels
  mutate(channel_unit_type = recode(channel_unit_type,
                                    `SC` = "SSC")) %>%
  # round distance to cover to nearest 0.1m and rename
  mutate(distance_to_cover = round(distance_to_cover_round_to_nearest_0_1m, 1)) %>%
  select(-distance_to_cover_round_to_nearest_0_1m) %>%
  # fix some names within dominant substrate and reorder levels
  mutate(dominant_substrate_1mx1m = fct_recode(dominant_substrate_1mx1m,
                                               Unknown = "unknown",
                                               Boulder = "boulder",
                                               Cobble = "cobble",
                                               "Fines" = "silt_fines",
                                               "Sand" = "sand",
                                               "Gravel" = "gravel"),
         dominant_substrate_1mx1m = fct_relevel(dominant_substrate_1mx1m,
                                                "Gravel",
                                                after = 2),
         dominant_substrate_1mx1m = fct_relevel(dominant_substrate_1mx1m,
                                                "Sand",
                                                after = 3)) %>%
  # add a variable of whether there's any cover present within 1.5m
  mutate(has_cover = if_else(dominant_cover_type_1_5m_radius %in%
        c("No Cover", "Unknown") | is.na(dominant_cover_type_1_5m_radius),
                             "No", "Yes"))

# combine habitat use and habitat availability data
xs_all = xs_use %>%
  mutate(source = "use") %>%
  bind_rows(xs_avail %>%
              mutate(source = "avail"))

xs_summary = xs_all %>%
  filter(source == "avail") %>%
  group_by(xs_name, sin_cat) %>%
  summarise(n_pts = n_distinct(xs_point_number))

# read in center point lines for availability and categorize by sinousity
center_pts_all = st_read("S:/main/data/habitat/lemhi_telemetry/availability/raw/Centerline_XS_Intersects.shp",
                         quiet = T) %>%
  st_zm() %>%
  mutate(Category = factor(Category,
                           levels = c("Low",
                                      "Med",
                                      "High")))

cu_fig = use %>%
  ggplot(aes(x = source,
             fill = channel_unit_type)) +
  geom_bar(position = position_fill()) +
  scale_fill_brewer(palette = "Set2",
                    name = "CU Type") +
  facet_wrap(~ sin_cat) +
  labs(x = "Data Set",
       y = "Percentage")
cu_fig
xs_select <- xs_use %>%
  filter(habitat_selected == "Selected", same_location_as_last_survey == "no", Category == "High")
use_unique <- xs_use %>%
  distinct(radio_frequency) %>%
summarize(count = n())
xs_use %>%
  filter(tag_status == "Recovered") %>%
  distinct(radio_frequency) %>%
  summarize(count = n())
