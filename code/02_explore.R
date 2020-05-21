# Load packages  and functions ------------------------------------------------

library(here)
library(tidyverse)
library(car)
source(here("code", "functions", "map_shapiro.R"))
source(here("code", "functions", "map_levene.R"))

# Load and prepare data -------------------------------------------------------

source(here("code", "01_tidy_data.R"))

con_0m <- df_long %>%
  filter(group == "Control", time == "Baseline")
con_6m <- df_long %>%
  filter(group == "Control", time == "6 months after")
int_0m <- df_long %>%
  filter(group == "Intervention", time == "Baseline")
int_6m <- df_long %>%
  filter(group == "Intervention", time == "6 months after")

df_control <- df_long %>% filter(group == "Control")
df_intervention <- df_long %>% filter(group == "Intervention")
df_0m <- df_long %>% filter(time == "Baseline")
df_6m <- df_long %>% filter(time == "6 months after")

# Make a list of data frames to be used in the map functions
data <- list(con_0m, con_6m, int_0m, int_6m)

# Separate the variables by "group"
# Demographic and anthropometric variables
demo_anthro_vars <- c(
  "age", "height", "weight", "bmi", 
  "waist_circumference", "hip_circumference", "waist_hip_ratio"
)

# Body composition variables
body_comp_vars <- c(
  "percent_whole_body_fat_mass", "android_total_mass", "gynoid_total_mass",
  "android_gynoid_ratio", "lower_limbs_total_mass", "upper_body_total_mass",
  "whole_body_total_mass", "whole_body_fat_mass", "whole_body_lean_mass"
)

# Strength variables
strength_vars <- c(
  "pt_knee_60ds_exten", "pt_knee_60ds_flexi",
  "pt_knee_60ds_exten_divided_whole_body_total_mass",
  "pt_knee_60ds_flexi_divided_whole_body_total_mass"
)

# Balance variables
balance_vars <- c(
  "ellipse_eo", "vap_eo", "sdap_eo", "vml_eo", "sdml_eo", "vt_eo",
  "ellipse_ec", "vap_ec", "sdap_ec", "vml_ec", "sdml_ec", "vt_ec"
)

# Physical activity variables
pa_vars <- c("steps", "sb", "lpa", "mpa", "mvpa")

# SF36 variables
sf36_vars <- c(
  "physical_functioning", "role_limitations_physical", "bodily_pain",
  "general_health", "vitality", "social_functioning", 
  "role_limitations_emotional", "mental_health", 
  "physical_component", "mental_component"
)

# Normality tests -------------------------------------------------------------

map(data, ~ map_shapiro(.x, demo_anthro_vars))
map(data, ~ map_shapiro(.x, body_comp_vars))
map(data, ~ map_shapiro(.x, strength_vars))
map(data, ~ map_shapiro(.x, balance_vars))
map(data, ~ map_shapiro(.x, pa_vars))
map(data, ~ map_shapiro(.x, sf36_vars))

# Test for homocedasticity ----------------------------------------------------

# By time
map_levene(df_control, demo_anthro_vars, "time")
map_levene(df_control, body_comp_vars, "time")
map_levene(df_control, strength_vars, "time")
map_levene(df_control, balance_vars, "time")
map_levene(df_control, pa_vars, "time")
map_levene(df_control, sf36_vars, "time")

map_levene(df_intervention, demo_anthro_vars, "time")
map_levene(df_intervention, body_comp_vars, "time")
map_levene(df_intervention, strength_vars, "time")
map_levene(df_intervention, balance_vars, "time")
map_levene(df_intervention, pa_vars, "time")
map_levene(df_intervention, sf36_vars, "time")

# By group
map_levene(df_0m, demo_anthro_vars, "group")
map_levene(df_0m, body_comp_vars, "group")
map_levene(df_0m, strength_vars, "group")
map_levene(df_0m, balance_vars, "group")
map_levene(df_0m, pa_vars, "group")
map_levene(df_0m, sf36_vars, "group")

map_levene(df_6m, demo_anthro_vars, "group")
map_levene(df_6m, body_comp_vars, "group")
map_levene(df_6m, strength_vars, "group")
map_levene(df_6m, balance_vars, "group")
map_levene(df_6m, pa_vars, "group")
map_levene(df_6m, sf36_vars, "group")

# Descriptives ----------------------------------------------------------------

df_long %>% 
  group_by(group, time) %>% 
  summarise_at(
    demo_anthro_vars, list(mean = mean, sd = sd), na.rm = TRUE
  )
df_long %>% 
  group_by(group, time) %>% 
  summarise_at(
    body_comp_vars, list(mean = mean, sd = sd), na.rm = TRUE
  )
df_long %>% 
  group_by(group, time) %>% 
  summarise_at(
    strength_vars, list(mean = mean, sd = sd), na.rm = TRUE
  )
df_long %>% 
  group_by(group, time) %>% 
  summarise_at(
    balance_vars, list(mean = mean, sd = sd), na.rm = TRUE
  )
df_long %>% 
  group_by(group, time) %>% 
  summarise_at(
    pa_vars, list(mean = mean, sd = sd), na.rm = TRUE
  )
df_long %>% 
  group_by(group, time) %>% 
  summarise_at(
    sf36_vars, list(mean = mean, sd = sd), na.rm = TRUE
  )
