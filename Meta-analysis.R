# Script to run meta-analysis on Rs data
# Shoshi Hornum 2021

library(dplyr)
library(tidyr)
library(ggplot2)
library(metafor)
library(ggmap)
library(readr)

# Step 1 - read the data using R's read.csv() function
dat <- read.csv("Data_MA.csv", skip = 2)

# Step 2 - reshape the data into form needed by metafor
# (This is kind of a PITA  -BBL)
# Construct the control data frame
dat %>% 
  filter(Manipulation == "None") %>% 
  select(-Record_number, -Author, -Manipulation, -Manipulation_level, -N,
         -SM_mean, -SM_sd)->
  dat_control

dat_control %>% 
  select(-starts_with("SD_"), -Percent_control) %>% 
  pivot_longer(cols = c(Rs_annual, Rh_annual, Rs_growingseason), 
               names_to = "depvar", values_to = "control") ->
  cont1

dat_control %>% 
  select(-Rs_annual, -Rh_annual, -Rs_growingseason, -Percent_control) %>% 
  pivot_longer(cols = c(SD_Rs_annual, SD_Rh_annual, SD_Rs_growingseason),
               names_to = "depvar", values_to = "SD_control") %>% 
  mutate(depvar = gsub("SD_", "", depvar)) ->
  cont2

cont1 %>% left_join(cont2) -> dat_control

# and manipulation data frame
dat %>% 
  filter(Manipulation != "None") %>% 
  select(-starts_with("SD_")) %>% 
  pivot_longer(cols = c(Rs_annual, Rh_annual, Rs_growingseason), 
               names_to = "depvar", values_to = "manip") ->
  manip1

dat %>% 
  filter(Manipulation != "None") %>% 
  select(-Rs_annual, -Rh_annual, -Rs_growingseason) %>% 
  pivot_longer(cols = c(SD_Rs_annual, SD_Rh_annual, SD_Rs_growingseason), 
               names_to = "depvar", values_to = "SD_manip") %>% 
  mutate(depvar = gsub("SD_", "", depvar)) ->
  manip2

manip1 %>% left_join(manip2) -> dat_manip

# ...and join with the manipulation data
dat_manip %>% 
  left_join(dat_control, 
            by = c("Study_number", "Study_midyear", "Ecosystem_type",
                   "Latitude", "Longitude", "Meas_method", "Soil_type", "Soil_drainage",
                   "Elevation", "depvar")) %>% 
  filter(!is.na(manip)) ->
  dat_rs

# Step 2.5 - Use filter() and select() to isolate data we're interested in
# Filter for water addition
dat_rs %>%
  filter(depvar == "Rs_growingseason", manip != "NA", control != "NA", 
         Percent_control > 100) %>% 
  select(N, manip, SD_manip, control, SD_control, Percent_control, 
         Author) -> rs_growing_addition

# Filter for water removal
dat_rs %>% 
  filter(depvar == "Rs_growingseason", manip != "NA", control != "NA", 
         Percent_control < 100) %>% 
  select(N, manip, SD_manip, control, SD_control, Percent_control, 
         Author) -> rs_growing_removal

# Compute Effect Sizes
# Effect size for water addition
rsg_es_addition <- escalc(measure = "ROM", m1i = manip, m2i = control, 
                          sd1i = SD_manip, sd2i = SD_control, n1i = N, 
                          n2i = N, data = rs_growing_addition)

# Effect size for water removal
rsg_es_removal <- escalc(measure = "ROM", m1i = manip, m2i = control, 
                         sd1i = SD_manip, sd2i = SD_control, n1i = N, 
                         n2i = N, data = rs_growing_removal)

# Run the Meta-Analysis
# Water addition
rsg_ma_add <- rma(yi, vi, data = rsg_es_addition)

# Water removal
rsg_ma_remove <- rma(yi, vi, data = rsg_es_removal)

# Plots of Effect Sizes
# Water addition
ggplot(rsg_es_addition, aes(yi, Author, color = Percent_control)) +
  geom_point() + 
  theme_minimal() + 
  scale_color_gradient(low = "purple", high = "yellow") +
  geom_vline(xintercept = 0.0, color = "black") + 
  xlab("Effect Size") + 
  ggtitle("Water Addition") + 
  geom_vline(xintercept = rsg_ma_add$beta, linetype = 2, color = "tomato3")+ 
  geom_rect(xmin = rsg_ma_add$ci.lb,
            xmax = rsg_ma_add$ci.ub, 
            ymin = -Inf, 
            ymax = Inf, 
            alpha = 0.002, color = "tomato3")

# Water addition 2
ggplot(rsg_es_addition, aes(yi, Author)) + 
  geom_boxplot(fill = "purple") + 
  geom_jitter(alpha = 0.2) + 
  geom_vline(xintercept = 0.0, color = "black") +
  theme_minimal() + xlab("Effect Size") + 
  ggtitle("Water Addition") +
  geom_vline(xintercept = rsg_ma_add$beta, linetype = 2, color = "red") +
  geom_rect(xmin = rsg_ma_add$ci.lb, 
            xmax = rsg_ma_add$ci.ub,
            ymin = -Inf,
            ymax = Inf, 
            alpha = 0.002, color = "red")

# Water removal
ggplot(rsg_es_removal, aes(yi, Author, color = Percent_control)) +
  geom_point() + 
  theme_minimal() + 
  scale_color_gradient(low = "purple", high = "yellow") + 
  geom_vline(xintercept = 0.0, color = "black") + 
  xlab("Effect Size") +
  ggtitle("Water Removal") +
  geom_vline(xintercept = rsg_ma_remove$beta, linetype = 2, color = "tomato3")+
  geom_rect(xmin = rsg_ma_remove$ci.lb, 
            xmax = rsg_ma_remove$ci.ub,
            ymin = -Inf, 
            ymax = Inf, 
            alpha = 0.002, color = "tomato3")

# Water removal 2
ggplot(rsg_es_removal, aes(yi, Author)) + geom_boxplot(fill = "purple") + 
  geom_jitter(alpha = 0.2) + geom_vline(xintercept = 0.0, color = "black") +
  theme_minimal() + xlab("Effect Size") + ggtitle("Water Removal") +
  geom_vline(xintercept = rsg_ma_remove$beta, linetype = 2, color = "red") +
  geom_rect(xmin = rsg_ma_remove$ci.lb, 
            xmax = rsg_ma_remove$ci.ub,
            ymin = -Inf,
            ymax = Inf, 
            alpha = 0.002, color = "red")

# Map!

# Select the columns we want to use
dat_rs %>% 
  distinct(Study_number, .keep_all = TRUE) -> rs_df

# Create a bounding box
bbox <- make_bbox(lon = c(-170, 170), lat = c(-70, 70)) # make a coordinate box

# Pull map tiles from online based on your bounding box `bbox`
map <- get_map(location = bbox, source = "stamen", maptype = "terrain") # load map from online

# Plot!!!!
p <- ggmap(map) +
  geom_point(data = dat_rs, aes(x = Longitude, y = Latitude)) +
  labs(x = "Longitude", y = "Latitude") + 
  theme_minimal() +
  theme(legend.position = "bottom")
print(p)

# Hard data
dat_rs %>% 
  filter(Percent_control != "NA") %>% 
  ggplot(aes(x = control, y = manip, color = Percent_control)) + 
  geom_point() + 
  ylab("Manipulation") + xlab("Control") +
  theme_minimal() + 
  facet_wrap(~depvar, scales = "free") + 
  scale_color_gradient(low = "purple", high = "yellow")
