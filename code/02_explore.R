# Load packages  and functions ------------------------------------------------

library(here)
library(tidyverse)
library(car)
source(here("code", "functions", "map_shapiro.R"))

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

# Make a list of data frames to be used in the map functions
data <- list(con_0m, con_6m, int_0m, int_6m)

# Normality tests -------------------------------------------------------------

# Demographic and anthropometric variables
demo_anthro_vars <- c(
  "age", "height", "weight", "bmi", 
  "waist_circumference", "hip_circumference", "waist_hip_ratio"
)
map(data, ~ map_shapiro(.x, demo_anthro_vars))

# Body composition variables
body_comp_vars <- c(
  "percent_whole_body_fat_mass", "android_total_mass", "gynoid_total_mass",
  "android_gynoid_ratio", "lower_limbs_total_mass", "upper_body_total_mass",
  "whole_body_total_mass", "whole_body_fat_mass", "whole_body_lean_mass"
)
map(data, ~ map_shapiro(.x, body_comp_vars))

# Strength variables
strength_vars <- c(
  "pt_knee_60ds_exten", "pt_knee_60ds_flexi",
  "pt_knee_60ds_exten_divided_whole_body_total_mass",
  "pt_knee_60ds_flexi_divided_whole_body_total_mass"
)
map(data, ~ map_shapiro(.x, strength_vars))

# Balance variables
balance_vars <- c(
  "ellipse_eo", "vap_eo", "sdap_eo", "vml_eo", "sdml_eo", "vt_eo",
  "ellipse_ec", "vap_ec", "sdap_ec", "vml_ec", "sdml_ec", "vt_ec"
)
map(data, ~ map_shapiro(.x, balance_vars))

# Physical activity variables
pa_vars <- c("steps", "sb", "lpa", "mpa", "vpa", "mvpa")
map(data, ~ map_shapiro(.x, pa_vars))

# SF36 variables
sf36_vars <- c(
  "physical_functioning", "role_limitations_physical", "bodily_pain",
  "general_health", "vitality", "social_functioning", 
  "role_limitations_emotional", "mental_health", 
  "physical_component", "mental_component"
)
map(data, ~ map_shapiro(.x, sf36_vars))
