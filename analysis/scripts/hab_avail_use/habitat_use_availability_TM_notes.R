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


# install packages
install.packages(c("tidyselect","dials","dplyr","rlang","tibble",
                   "tune","workflows","workflowsets","tidymodels"))
library(tidyverse)
library(tidymodels)
library(tidytext)
install.packages(c("rstatix", "corrr"))
library(rstatix)
library(corrr)
library(ggplot2)

# Cluster analysis of depth and velocities for selected habitat
kclusts <-
  tibble(k = 3) %>%
  mutate(kclust = map(k, ~kmeans(xs_select_dv, .x)),
  tidied = map(kclust, tidy),
glanced = map(kclust, glance),
augmented = map(kclust, augment, xs_select_dv))

clusters <-
  kclusts %>%
  unnest(cols = c(tidied))
assignments <-
  kclusts %>%
  unnest(cols = c(augmented))
clusterings <-
  kclusts %>%
  unnest(cols = c(glanced))

select_dv_map <-
  ggplot(assignments, aes(x = mean_column_velocity, y = depth_m)) +
  geom_point(aes(color = .cluster), alpha = 0.8) +
  facet_wrap(~ k)
select_dv_map
select_dv_x <- select_dv_map + geom_point(data = clusters, size = 10, shape = "x")
select_dv_x

ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()

three_select_dv <- xs_select_dv %>%
  kmeans(centers = 3)
three_select_dv
summary(three_select_dv)

augment(three_select_dv,xs_select_dv)

glance(three_select_dv)

three_dv <- tidy(three_select_dv)
three_dv
Comparison <- three_dv %>%
  ggplot(mapping = aes(
    x = mean_column_velocity,
    y = depth_m
  )) +
  geom_point(aes(colour = factor(cluster),
                 size = size))

# filtering out selected habitat with depth and velocity measurements by  low sinuousity
xs_low_dv <- xs_use %>%
  mutate(habitat_selected_new = if_else(habitat_selected == "Selected" & same_location_as_last_survey == "no", "Yes", "No")) %>%
  filter(habitat_selected_new == "Yes", depth_m != "NA", mean_column_velocity != "NA", Category == "Low") %>%
  select(depth_m, mean_column_velocity)

# Clustering analysis
kclusts <-
  tibble(k = 3) %>%
  mutate(kclust = map(k, ~kmeans(xs_low_dv, .x)),
         tidied = map(kclust, tidy),
         glanced = map(kclust, glance),
         augmented = map(kclust, augment, xs_low_dv))

clusters <-
  kclusts %>%
  unnest(cols = c(tidied))
assignments <-
  kclusts %>%
  unnest(cols = c(augmented))
clusterings <-
  kclusts %>%
  unnest(cols = c(glanced))


low_dv_map <-
  ggplot(assignments, aes(x = mean_column_velocity, y = depth_m)) +
  geom_point(aes(color = .cluster), alpha = 0.8) +
  facet_wrap(~ k) +
  labs(title = "Low Use", x = "Mean Column Velocity (m/s)", y = "Depth (m)") +
theme(plot.title = element_text(hjust = 0.5))
low_dv_map

# filtering out selected habitat with depth and velocity measurements by medium sinuousity
xs_med_dv <- xs_use %>%
  mutate(habitat_selected_new = if_else(habitat_selected == "Selected" & same_location_as_last_survey == "no", "Yes", "No")) %>%
  filter(habitat_selected_new == "Yes", depth_m != "NA", mean_column_velocity != "NA", Category == "Med") %>%
  select(depth_m, mean_column_velocity)

# Clustering analysis
kclusts <-
  tibble(k = 3) %>%
  mutate(kclust = map(k, ~kmeans(xs_med_dv, .x)),
         tidied = map(kclust, tidy),
         glanced = map(kclust, glance),
         augmented = map(kclust, augment, xs_med_dv))

clusters <-
  kclusts %>%
  unnest(cols = c(tidied))
assignments <-
  kclusts %>%
  unnest(cols = c(augmented))
clusterings <-
  kclusts %>%
  unnest(cols = c(glanced))


med_dv_map <-
  ggplot(assignments, aes(x = mean_column_velocity, y = depth_m)) +
  geom_point(aes(color = .cluster), alpha = 0.8) +
  facet_wrap(~ k) +
  labs(title = "Med Use", x = "Mean Column Velocity (m/s)", y = "Depth (m)") +
  theme(plot.title = element_text(hjust = 0.5))
med_dv_map

# filtering out selected habitat with depth and velocity measurements by high sinuousity
xs_high_dv <- xs_use %>%
  mutate(habitat_selected_new = if_else(habitat_selected == "Selected" & same_location_as_last_survey == "no", "Yes", "No")) %>%
  filter(habitat_selected_new == "Yes", depth_m != "NA", mean_column_velocity != "NA", Category == "High") %>%
  select(depth_m, mean_column_velocity)

# Clustering analysis
kclusts <-
  tibble(k = 3) %>%
  mutate(kclust = map(k, ~kmeans(xs_high_dv, .x)),
         tidied = map(kclust, tidy),
         glanced = map(kclust, glance),
         augmented = map(kclust, augment, xs_high_dv))

clusters <-
  kclusts %>%
  unnest(cols = c(tidied))
assignments <-
  kclusts %>%
  unnest(cols = c(augmented))
clusterings <-
  kclusts %>%
  unnest(cols = c(glanced))


high_dv_map <-
  ggplot(assignments, aes(x = mean_column_velocity, y = depth_m)) +
  geom_point(aes(color = .cluster), alpha = 0.8) +
  facet_wrap(~ k) +
  labs(title = "High Use", x = "Mean Column Velocity (m/s)", y = "Depth (m)") +
  theme(plot.title = element_text(hjust = 0.5))
high_dv_map
