# PIE Summit Clean Script
# Updated 2023-10-11 by Alex Spurrier

# load --------
library(tidyverse)
library(edbuildr)
library(scales)
library(viridis)

options(scipen = 999)

# pull data from edbuild
dist_fy19_raw <- masterpull(data_type = "geo", data_year = 2019)

# clean --------

# create a df of your state's data
your_state_ed_data <- dist_fy19_raw |>
  filter(State == "Minnesota") |> 
  rename(district = NAME,
         county = County,
         enroll = ENROLL, 
         urbanicity = dUrbanicity,
         operational_schools = dOperational_schools, 
         district_type = dType, 
         econ_dis_enroll = StPov,
         pov_pct = StPovRate,
         mhi = MHI, 
         mpv = MPV,
         lrpp = LRPP,
         srpp = SRPP,
         slrpp = SLRPP) |> 
  select(district, county, enroll, lrpp, srpp, slrpp, mhi, mpv,
         urbanicity, operational_schools, district_type, 
         pov_pct)

# create a df that allows us to see the data that has NA values in the 
# "slrpp" and then "pov_pct" variables
your_state_na_dist <- your_state_ed_data |>
  filter(is.na(slrpp) | is.na(pov_pct))

# create clean dataframe w/o missing value rows
your_state_ed_clean <- your_state_ed_data |>
  filter(!is.na(slrpp), 
         !is.na(pov_pct))

# plot -------

ggplot(your_state_ed_clean, 
       aes(x = pov_pct, y = slrpp, size = enroll, color = mhi)) +
  geom_point(alpha = .7) + 
  geom_smooth(method = "lm", se = FALSE, 
              show.legend = FALSE,
              color = "#FF8200") + 
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) + 
  scale_size_area(labels = label_comma(),
                  max_size = 10) +
  scale_color_viridis(labels = label_dollar()) + 
  labs(x = "Student Poverty Rate", 
       y = "State + Local Per-Pupil Revenue",
       title = "YOUR STATE Per-Pupil Revenue by Student Poverty Rate",
       subtitle = "Data from school year 2018-19; trend line indicates relationship",
       caption = "Source: EdBuild",
       size = "Enrollment",
       color = "MHI") +
  theme_bw()

# export ------------

ggsave("state_plot.png", units = "in", height = 5, width = 7)
