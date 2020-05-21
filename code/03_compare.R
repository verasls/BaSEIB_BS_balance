# Load packages  and functions ------------------------------------------------

library(here)
library(tidyverse)
library(gmodels)
library(npsm)
source(here("code", "functions", "map_compare_2_groups.R"))
source(here("code", "functions", "map_ancova.R"))

# Load and prepare data -------------------------------------------------------

source(here("code", "01_tidy_data.R"))

df_control <- df_long %>% filter(group == "Control")
df_intervention <- df_long %>% filter(group == "Intervention")
df_0m <- df_long %>% filter(time == "Baseline")

# Separate the variables by "group"
# Demographic and anthropometric variables
demo_anthro_vars <- c(
  "age", "height", "weight", "bmi", 
  "waist_circumference", "hip_circumference", "waist_hip_ratio"
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

# Group effect at baseline ----------------------------------------------------

map_compare_2_groups(df_0m, demo_anthro_vars, "group")
map_compare_2_groups(df_0m, strength_vars, "group")
map_compare_2_groups(df_0m, balance_vars, "group")

CrossTable(
  df_0m[["sex"]], df_0m[["group"]],
  fisher = TRUE, chisq = TRUE, format = "SPSS"
)

# Time effect -----------------------------------------------------------------

map_compare_2_groups(df_control, demo_anthro_vars, "time", paired = TRUE)
map_compare_2_groups(df_control, strength_vars, "time", paired = TRUE)
map_compare_2_groups(df_control, balance_vars, "time", paired = TRUE)
map_compare_2_groups(df_control, pa_vars, "time", paired = TRUE)
map_compare_2_groups(df_control, sf36_vars, "time", paired = TRUE)

map_compare_2_groups(df_intervention, demo_anthro_vars, "time", paired = TRUE)
map_compare_2_groups(df_intervention, strength_vars, "time", paired = TRUE)
map_compare_2_groups(df_intervention, balance_vars, "time", paired = TRUE)
map_compare_2_groups(df_intervention, pa_vars, "time", paired = TRUE)
map_compare_2_groups(df_intervention, sf36_vars, "time", paired = TRUE)

# Treatment effect ------------------------------------------------------------

map_ancova(df, demo_anthro_vars[- c(1, 2)])
map_ancova(df, strength_vars)
map_ancova(df, balance_vars)
map_ancova(df, pa_vars)
map_ancova(df, sf36_vars[- c(9, 6, 7, 10)])

# Non parametric ANCOVA
data <- df %>% as.data.frame()

onecova(
  2,
  data[, c("social_functioning_6m", "group")] %>% na.omit(),
  data["social_functioning_0m"] %>% na.omit()
)

onecova(
  2,
  data[, c("physical_component_6m", "group")] %>% na.omit(),
  data["physical_component_0m"] %>% na.omit()
)

onecova(
  2,
  data[, c("role_limitations_emotional_6m", "group")] %>% na.omit(),
  data["role_limitations_emotional_0m"] %>% na.omit()
)

onecova(
  2,
  data[, c("mental_component_6m", "group")] %>% na.omit(),
  data["mental_component_0m"] %>% na.omit()
)
