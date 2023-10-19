# PIE Summit National Script
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

# create a df of national data
national_ed_data <- dist_fy19_raw |>
  rename(district = NAME,
         state = State,
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
  select(district, state, county, enroll, lrpp, srpp, slrpp, mhi, mpv,
         urbanicity, operational_schools, district_type, 
         pov_pct)

# create a df that allows us to see the data that has NA values in the 
# "slrpp" and then "pov_pct" variables
national_na_dist <- national_ed_data |>
  filter(is.na(slrpp) | is.na(pov_pct))

# create clean dataframe w/o missing value rows
national_ed_clean <- national_ed_data |>
  filter(!is.na(slrpp), 
         !is.na(pov_pct))

# plot -------

ggplot(national_ed_clean, 
       aes(x = pov_pct, y = slrpp, size = enroll, color = mhi)) +
  geom_point(alpha = .7) + 
  geom_smooth(method = "lm", se = FALSE,
              show.legend = FALSE,
              color = "#FF8200") +
  facet_wrap(~state, scales = "free") +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) + 
  scale_size_area(labels = label_comma(),
                  max_size = 10) +
  scale_color_viridis(labels = label_dollar()) + 
  labs(x = "Student Poverty Rate", 
       y = "State + Local Per-Pupil Revenue",
       title = "Per-Pupil Revenue by Student Poverty Rate",
       subtitle = "Data from school year 2018-19; trend line indicates state-level relationship",
       caption = "Source: EdBuild",
       size = "Enrollment",
       color = "MHI") +
  theme_bw()

# export ------------

ggsave("national_plot.png", units = "in", height = 20, width = 27)
