# Author: Mike Ackerman
# Purpose: Import habitat availability and use datasets from the MHE
#   NAS and save to the repo
# Created: 7/7/222
# Notes:

#-------------------
# clear environment
rm(list = ls())

#-------------------
# load packages
library(tidyverse)
library(sf)

#-------------------
# habitat availability center pts
center_pts_all = st_read("S:/main/data/habitat/lemhi_telemetry/availability/raw/Centerline_XS_Intersects.shp",
                         quiet = T) %>%
  st_zm() %>%
  mutate(Category = factor(Category,
                           levels = c("Low",
                                      "Med",
                                      "High")))
# and save
save(center_pts_all,
     file = here("analysis/data/derived_data/center_pts_all.rda"))

#-------------------
# depths and velocities from the habitat availability
xs_raster = crossing(metric = c("depth", "velocity"),
                     sin_class = c("Low", "Med", "High")) %>%
  mutate(sample_xs = map2(metric,
                          sin_class,
                          .f = function(x, y) {
                            st_read(paste0("S:/main/data/habitat/lemhi_telemetry/availability/raw/cross_sections/DV_extract/", y,
                                           "_", str_sub(x, 0 ,1), ".shp"),
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
# and save
save(xs_raster,
     file = here("analysis/data/derived_data/xs_raster.rda"))

### END SCRIPT
