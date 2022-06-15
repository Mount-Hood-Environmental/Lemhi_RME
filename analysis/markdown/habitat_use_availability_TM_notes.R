library(here)
library(knitr)
library(kableExtra)
library(tidyverse)
library(here)
library(magrittr)
library(janitor)
library(sf)
library(EMT)
library(DescTools)
load(here("C:/Git/Lemhi_RME/analysis/data/derived_data/habitat_available.rda"))
load(here("C:/Git/Lemhi_RME/analysis/data/derived_data/habitat_use.rda"))
xs_all = xs_use %>%
  mutate(source = "use") %>%
  bind_rows(xs_avail %>%
              mutate(source = "avail")) %>%
  clean_names() %>%
  # improve some column names
  rename(release_loc = location,
         release_date = release,
         xs_name = name,
         stream_name = stream_nam,
         sin_cat = category,
         frazil_ice = frazel_ice) %>%
  # correct the code for small side channels
  mutate(channel_unit_type = recode(channel_unit_type,
                                    `SC` = "SSC")) %>%
  # round distance to cover to nearest 0.1m and rename
  mutate(distance_to_cover = round(distance_to_cover_round_to_nearest_0_1m, 1)) %>%
  select(-distance_to_cover_round_to_nearest_0_1m) %>%
  # fix some names within dominant substrate and reorder levels
  mutate(dominant_substrate_1mx1m = fct_recode(dominant_substrate_1mx1m,
                                               Boulder = "boulder",
                                               Cobble = "cobble",
                                               Fines = "silt_fines",
                                               Sand = "sand",
                                               Gravel = "gravel"),
         dominant_substrate_1mx1m = fct_relevel(dominant_substrate_1mx1m,
                                                "Gravel",
                                                after = 2),
         dominant_substrate_1mx1m = fct_relevel(dominant_substrate_1mx1m,
                                                "Sand",
                                                after = 3)) %>%
  # add a variable of whether there's any cover present within 1.5m
  mutate(has_cover = if_else(dominant_cover_type_1_5m_radius %in% c("No Cover", "Unknown") | is.na(dominant_cover_type_1_5m_radius),
                             "No", "Yes"))%>%

  # add variable for new selected habitat (filtering out duplicate selected locations)
  mutate(habitat_selected_new = if_else(habitat_selected == "Selected" & same_location_as_last_survey == "no", "Yes", "No"))

xs_summary = xs_all %>%
  filter(source == "avail") %>%
  group_by(xs_name, sin_cat) %>%
  summarise(n_pts = n_distinct(xs_point_number))
cu_fig = xs_all %>%
  filter(source == "avail", habitat_selected_new == "Yes") %>%
  ggplot(aes(x = source,
             fill = channel_unit_type)) +
  geom_bar(position = position_fill()) +
  scale_fill_brewer(palette = "Set2",
                    name = "CU Type") +
  facet_wrap(~ sin_cat) +
  labs(x = "Data Set",
       y = "Percentage")
cu_fig
# concealment figure
conceal_fig = xs_all %>%
  filter(source == "avail" & habitat_selected_new == "Yes") %>%
  filter(substrate_concealment %in% c("N", "Y")) %>%
  ggplot(aes(x = source,
             fill = substrate_concealment)) +
  geom_bar(position = position_fill()) +
  scale_fill_brewer(palette = "Set2",
                    name = "Concealment") +
  facet_wrap(~ sin_cat) +
  labs(x = "Data Set",
       y = "Percentage")
conceal_fig

cover_fig = xs_all %>%
  filter(source == "avail" & habitat_selected_new == "Yes") %>%
  ggplot(aes(x = source,
             fill = has_cover)) +
  geom_bar(position = position_fill()) +
  scale_fill_brewer(palette = "Set2",
                    name = "Cover") +
  facet_wrap(~ sin_cat) +
  labs(x = "Data Set",
       y = "Percentage")
cover_fig
# unique radio tag detections
use_unique <- xs_use %>%
  distinct(radio_frequency)
# filtering out selected habitat with depth and velocity measurements only
xs_select_dv <- xs_use %>%
  mutate(habitat_selected_new = if_else(habitat_selected == "Selected" & same_location_as_last_survey == "no", "Yes", "No")) %>%
  filter(habitat_selected_new == "Yes", depth_m != "NA", mean_column_velocity != "NA") %>%
  select(depth_m, mean_column_velocity)

# boxplot of depth for habitat use
boxplot_depth <- xs_use %>%
  mutate(habitat_selected_new = if_else(habitat_selected == "Selected" & same_location_as_last_survey == "no", "Yes", "No")) %>%
  filter(habitat_selected_new == "Yes", depth_m != "NA", mean_column_velocity != "NA") %>%
  select(depth_m)
boxplot(boxplot_depth, main = "Depth by Habitat Use", ylab = "Depth (m)")

# read in depth and velocities from the habitat availability transects
xs_raster = crossing(metric = c("depth", "velocity"),
                     sin_class = c("Low", "Med", "High")) %>%
  mutate(sample_xs = map2(metric,
                          sin_class,
                          .f = function(x, y) {
                            st_read(paste0("S:/main/data/habitat/lemhi_telemetry/availability/raw/cross_sections/DV_extract/", y, "_", str_sub(x, 0 ,1), ".shp"),
                                    quiet = T) %>%
                              dplyr::select(Name:Sinuosity,
                                            Category,
                                            raster_val = RASTERVALU) %>%
                              mutate_at(vars(raster_val),
                                        list(~ if_else(. == -9999,
                                                       NA_real_,
                                                       .))) %>%
                              mutate(metric = x)
                          }))

# depth & velocity cross section values - sf object
dv_sf = xs_raster %>%
  select(-metric) %>%
  unnest(cols = sample_xs) %>%
  st_as_sf()

# depth & velocity cross section values - data frame
dv_df = dv_sf %>%
  st_drop_geometry() %>%
  as_tibble()

# extract depths for available habitat
depth_avail <- dv_df %>%
  filter(metric == "depth") %>%
  select(raster_val)

# boxplot available depths
boxplot(depth_avail, main = "Depth by Available Habitat", ylab = "Depth (m)")

# boxplot for velocity use
boxplot_vel <- xs_use %>%
  mutate(habitat_selected_new = if_else(habitat_selected == "Selected" & same_location_as_last_survey == "no", "Yes", "No")) %>%
  filter(habitat_selected_new == "Yes", depth_m != "NA", mean_column_velocity != "NA") %>%
  select(mean_column_velocity)
boxplot(boxplot_vel, main = "Velocity by Habitat Use", ylab = "Velocity (m/s)")

# boxplot of velocity for available habitat
vel_avail <- dv_df %>%
  filter(metric == "velocity") %>%
  select(raster_val)
boxplot(vel_avail, main = "Velocity by Available Habitat", ylab = "Velocity (m/s)")
setwd("C:/Users/Tmac/OneDrive - Mount Hood Environmental/Desktop/Work/Mt Hood Environmental/Lemhi Habitat Availability & Use")
#read in csv for availability and use depths and velocities
avail_use_depth <- read_csv("avail_use_depth.csv")
avail_use_vel <- read_csv("avail_use_vel.csv")

#
 boxplot comparisons for available/use depths/velocities
boxplot(avail_use_depth$`Depth`~ avail_use_depth$Habitat, main = "Distribution of Depths", ylab = "Depth (m)", xlab ="")

boxplot(avail_use_vel$`Velocity`~ avail_use_vel$Habitat, main = "Distribution of Velocities", ylab = "Velocity (m/s)", xlab ="")

# variance test for depth
var.test(Depth ~ Habitat, data = avail_use_depth)

# Welch test for depth
depth_t <- t.test(Depth ~ Habitat, data = avail_use_depth)
tidy_depth <- tidy(depth_t) %>%
  rename("habitat_avail_mean" = ,
         )

# variance test for velocity
var.test(Velocity ~ Habitat, data = avail_use_vel)

# Welch test for velocity
vel_t <-t.test(Velocity ~ Habitat, data = avail_use_vel)
tidy_vel <- tidy(vel_t)

library(ggplot2)

# read in sinuosity points
center_pts_all = st_read("S:/main/data/habitat/lemhi_telemetry/availability/raw/Centerline_XS_Intersects.shp", quiet = T) %>%
  st_zm() %>%
  mutate(Category = factor(Category,
                           levels = c("Low",
                                      "Med",
                                      "High"))) %>%

  st_drop_geometry() %>%
  select(Category) %>%
  count(Category) %>%
  mutate(Percentage = n/sum(n)) %>%
  add_column(Habitat = "Available")

# Habitat use by sinuosity
sin_use <- xs_use %>%
filter(habitat_selected == "Selected" & same_location_as_last_survey == "no") %>%
select(Category) %>%
count(Category) %>%
mutate(Percentage = n/sum(n)) %>%
add_column(Habitat = "Use") %>%
bind_rows(center_pts_all) %>%

# Bar graph of use by sinuosity
ggplot(mapping = aes(x=Category, y=Percentage, fill = Habitat)) +
geom_bar (position = "dodge", stat = "identity") +
ggtitle('Habitat Use by Sinuosity')+
theme(plot.title = element_text(hjust = 0.5)) +
xlab('Sinuosity Category') +
ylab('Proportion')

use_facet = sin_use %>%
  ggplot(mapping = aes(x = Habitat, y = Percentage,
             fill = Category,)) +
  geom_bar(position = position_fill(), stat = "identity") +
  scale_fill_brewer(palette = "Set2",
                    name = "Sinuosity") +
  facet_wrap(~ Habitat) +
  labs(x = "Data Set",
       y = "Percentage")
use_facet



