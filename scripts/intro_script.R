# PIE Summit R Workshop Exercise
# Updated 2023-10-11 by Alex Spurrier

# load --------

library(tidyverse)
library(edbuildr)
library(scales)
library(viridis)

options(scipen = 999)

# we'll use masterpull() to get EdBuild's clean finance data for each state + DC 
# for transparency, use "_raw" in the variable name to denote that it's the
# original version of this data 
dist_fy19_raw <- masterpull(data_type = "geo", data_year = 2019)

# clean -----

# filter, rename, and mutate data for your state
# create a new dataframe from the dist_fy19_raw df that
# will only include your state's data data 
your_state_ed_data <- dist_fy19_raw |>
  # use the filter() function to select only your state's data 
  # replace "Minnesota" with your state's name in title case
  filter(State == "Minnesota") |> 
  # use the rename() function to rename the variables. 
  # in R, it's a best practice to keep column names lowercase and use a "_" to
  # connect 2+ words. in each line of this function, the renamed column is on 
  # the left hand side and the original column name is on the right hand side 
  rename(district = NAME,
         county = County,
         state = State,
         enroll = ENROLL, 
         urbanicity = dUrbanicity,
         operational_schools = dOperational_schools, 
         district_type = dType, 
         econ_dis_enroll = StPov,
         pov_pct = StPovRate, # census poverty rate
         mhi = MHI, # median household income
         mpv = MPV, # median property value
         lrpp = LRPP, # local revenue per pupil
         srpp = SRPP, # state revenue per pupil
         slrpp = SLRPP) |> # state and local revenue per pupil
  # use the select() function to choose the columns that we want to keep in 
  # the dataframe and their order
  select(district, county, state, enroll, lrpp, srpp, slrpp, mhi, mpv,
         urbanicity, operational_schools, district_type, 
         pov_pct)

# Step 1: Create a basic scatterplot ------

# first layer of a basic plot for your state
ggplot(your_state_ed_data, 
       aes(x = pov_pct, y = slrpp)) +
  geom_point()

# Step 2: Address missing values ------

# create a df that allows us to see the data that has NA values in the 
# "slrpp" and then "pov_pct" variables (the vars we used to plot) 
your_state_na_dist <- your_state_ed_data |>
  filter(is.na(slrpp) | is.na(pov_pct))

# The view() function will allow you to explore this new dataframe 
view(your_state_na_dist)

# now that we understand our missing data, we can create an updated dataframe 
# and re-plot our newly cleaned data.

# create tidy dataframe 
your_state_ed_clean <- your_state_ed_data |>
  # use the filter() function to remove the 8 values that have NAs 
  # the ! operator provides the opposite value, so in this case, we
  # will only keep rows that DO NOT return "TRUE" for the is.na() function
  filter(!is.na(slrpp), 
         !is.na(pov_pct)) |> 
  # use arrange to sort by enrollment
  arrange(enroll)

view(your_state_ed_clean)

# redo the basic scatter plot for your state w/ clean data
ggplot(your_state_ed_clean,
       aes(x = pov_pct, y = slrpp)) +
  geom_point() 

# Step 3: Clean up formatting of chart elements (1/3) -------

# add a basic theme
ggplot(your_state_ed_clean, 
       aes(x = pov_pct, y = slrpp)) +
  geom_point() + 
  # add a basic theme with theme_bw()
  theme_bw()

# Step 3: Clean up formatting of chart elements (2/3) ----------

# format data labels
ggplot(your_state_ed_clean, 
       aes(x = pov_pct, y = slrpp)) +
  geom_point() + 
  # make sure you have the `scales` package loaded!
  # format x-axis data labels as percentages 
  scale_x_continuous(labels = label_percent()) +
  # format y-axis data labels as dollars 
  scale_y_continuous(labels = label_dollar()) + 
  theme_bw()

# Step 3: Clean up formatting of chart elements (3/3) ---------

# add data labels to the scatterplot 
ggplot(your_state_ed_clean, 
       aes(x = pov_pct, y = slrpp)) +
  geom_point() + 
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) +
  # the labs() function allows you to set labels for plot elements
  labs(x = "Student Poverty Rate", 
       y = "State + Local Per-Pupil Revenue",
       title = "YOUR STATE Per-Pupil Revenue by Student Poverty Rate",
       subtitle = "Data from school year 2018-19",
       caption = "Source: EdBuild") +
  theme_bw()

# Step 4: Add a new layer of data --------

# add size element 
# the "size = enroll" will change the points so that the size corresponds to a 
# district's enrollment
ggplot(your_state_ed_clean, 
       aes(x = pov_pct, y = slrpp, size = enroll)) +
  geom_point() + 
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) +
  labs(x = "Student Poverty Rate", 
       y = "State + Local Per-Pupil Revenue",
       title = "YOUR STATE Per-Pupil Revenue by Student Poverty Rate",
       subtitle = "Data from school year 2018-19",
       caption = "Source: EdBuild") +
  theme_bw()

# Step 5: Tidy up new variable formatting (1/2) -------

# create more accurate contrast by size
ggplot(your_state_ed_clean, aes(x = pov_pct, y = slrpp, size = enroll)) +
  # adjusting the alpha will change the point opacity; alpha = 1 is the default  
  # and creates fully opaque points - the closer you set it to 0, the more 
  # transparent points will appear
  geom_point(alpha = .7) + 
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) + 
  # the scale_size_area() function changes point size from being based on 
  # diameter to area -- a more accurate way to represent data
  # max_size sets the limit for the largest point size on your plot
  scale_size_area(max_size = 10) +
  labs(x = "Student Poverty Rate", 
       y = "State + Local Per-Pupil Revenue",
       title = "YOUR STATE Per-Pupil Revenue by Student Poverty Rate",
       subtitle = "Data from school year 2018-19",
       caption = "Source: EdBuild") +
  theme_bw()

# Step 5: Tidy up new variable formatting (2/2) --------

# tidy up legend for the new variable
ggplot(your_state_ed_clean, aes(x = pov_pct, y = slrpp, size = enroll)) +
  geom_point(alpha = .7) + 
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) + 
  # format legend data labels with a thousands comma separator
  scale_size_area(labels = label_comma(),
                  max_size = 10) +
  labs(x = "Student Poverty Rate", 
       y = "State + Local Per-Pupil Revenue",
       title = "YOUR STATE Per-Pupil Revenue by Student Poverty Rate",
       subtitle = "Data from school year 2018-19",
       caption = "Source: EdBuild",
       # add label for the size element
       size = "Enrollment") +
  theme_bw()


# Step 6: Repeat steps 4-5 as needed (1/X) -------

# add in color based on median household income
ggplot(your_state_ed_clean, 
       # add new variable with "color = mhi"
       aes(x = pov_pct, y = slrpp, size = enroll, color = mhi)) +
  geom_point(alpha = .7) + 
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) + 
  scale_size_area(labels = label_comma(),
                  max_size = 10) +
  labs(x = "Student Poverty Rate", 
       y = "State + Local Per-Pupil Revenue",
       title = "YOUR STATE Per-Pupil Revenue by Student Poverty Rate",
       subtitle = "Data from school year 2018-19",
       caption = "Source: EdBuild",
       size = "Enrollment") +
  theme_bw()

# Step 6: Repeat steps 4-5 as needed (2/X) ---------

# use a more accessible color palette
# be sure the "viridis" package is loaded!
ggplot(your_state_ed_clean, 
       aes(x = pov_pct, y = slrpp, size = enroll, color = mhi)) +
  geom_point(alpha = .7) + 
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) + 
  scale_size_area(labels = label_comma(),
                  max_size = 10) +
  # add more accessible color palette 
  scale_color_viridis() + 
  labs(x = "Student Poverty Rate", 
       y = "State + Local Per-Pupil Revenue",
       title = "YOUR STATE Per-Pupil Revenue by Student Poverty Rate",
       subtitle = "Data from school year 2018-19",
       caption = "Source: EdBuild",
       size = "Enrollment") +
  theme_bw()

# Step 6: Repeat steps 4-5 as needed (3/6) -------

# adjust formatting for new variable
ggplot(your_state_ed_clean, 
       aes(x = pov_pct, y = slrpp, size = enroll, color = mhi)) +
  geom_point(alpha = .7) + 
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) + 
  scale_size_area(labels = label_comma(),
                  max_size = 10) +
  # convert data labels to dollar format
  scale_color_viridis(labels = label_dollar()) + 
  labs(x = "Student Poverty Rate", 
       y = "State + Local Per-Pupil Revenue",
       title = "YOUR STATE Per-Pupil Revenue by Student Poverty Rate",
       subtitle = "Data from school year 2018-19",
       caption = "Source: EdBuild",
       size = "Enrollment",
       # add label for legend 
       color = "MHI") +
  theme_bw()

# Step 6: Repeat steps 4-5 as needed (4/6) -------

# add a trend line
ggplot(your_state_ed_clean, 
       aes(x = pov_pct, y = slrpp, size = enroll, color = mhi)) +
  geom_point(alpha = .7) + 
  # add a regression line
  geom_smooth() + 
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) + 
  scale_size_area(labels = label_comma(),
                  max_size = 10) +
  scale_color_viridis(labels = label_dollar()) + 
  labs(x = "Student Poverty Rate", 
       y = "State + Local Per-Pupil Revenue",
       title = "YOUR STATE Per-Pupil Revenue by Student Poverty Rate",
       subtitle = "Data from school year 2018-19",
       caption = "Source: EdBuild",
       size = "Enrollment",
       color = "MHI") +
  theme_bw()


# Step 6: Repeat steps 4-5 as needed (5/6) -------

# add a trend line
ggplot(your_state_ed_clean, 
       aes(x = pov_pct, y = slrpp, size = enroll, color = mhi)) +
  geom_point(alpha = .7) + 
  # set options to use linear regression, remove the error band, prevent  
  # this layer from messing up the enrollment legend, and make it UT orange
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
       subtitle = "Data from school year 2018-19",
       caption = "Source: EdBuild",
       size = "Enrollment",
       color = "MHI") +
  theme_bw()

# Step 6: Repeat steps 4-5 as needed 6/6) -------

# update the subtitle
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
       # revise subtitle
       subtitle = "Data from school year 2018-19; trend line indicates relationship",
       caption = "Source: EdBuild",
       size = "Enrollment",
       color = "MHI") +
  theme_bw()

# Step 7: Export your plot ------------

ggsave("state_plot.png", units = "in", height = 5, width = 7)
