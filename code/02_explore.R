# Load packages  and functions ------------------------------------------------

library(here)
library(tidyverse)
library(car)

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

# Normality tests -------------------------------------------------------------

# Demographic and anthropometric variables
demo_anthro_vars <- c(
  "age", "height", "weight", "bmi", 
  "waist_circumference", "hip_circumference", "waist_hip_ratio"
)
# Control group - 0M
demo_anthro_con_0m_tests <- map(demo_anthro_vars, ~ con_0m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Control", time = "0M", variables = demo_anthro_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(demo_anthro_con_0m_tests, "statistic"),
    p_value = map_dbl(demo_anthro_con_0m_tests, "p.value")
  )
# Control group - 6M
demo_anthro_con_6m_tests <- map(demo_anthro_vars, ~ con_6m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Control", time = "6M", variables = demo_anthro_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(demo_anthro_con_6m_tests, "statistic"),
    p_value = map_dbl(demo_anthro_con_6m_tests, "p.value")
  )
# Intervention group - 0M
demo_anthro_int_0m_tests <- map(demo_anthro_vars, ~ int_0m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Intervention", time = "0M", variables = demo_anthro_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(demo_anthro_int_0m_tests, "statistic"),
    p_value = map_dbl(demo_anthro_int_0m_tests, "p.value")
  )
# Intervention group - 6M
demo_anthro_int_6m_tests <- map(demo_anthro_vars, ~ int_6m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Intervention", time = "6M", variables = demo_anthro_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(demo_anthro_int_6m_tests, "statistic"),
    p_value = map_dbl(demo_anthro_int_6m_tests, "p.value")
  )

# Body composition variables
body_comp_vars <- c(
  "percent_whole_body_fat_mass", "android_total_mass", "gynoid_total_mass",
  "android_gynoid_ratio", "lower_limbs_total_mass", "upper_body_total_mass",
  "whole_body_total_mass", "whole_body_fat_mass", "whole_body_lean_mass"
)
# Control group - 0M
body_comp_con_0m_tests <- map(body_comp_vars, ~ con_0m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Control", time = "0M", variables = body_comp_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(body_comp_con_0m_tests, "statistic"),
    p_value = map_dbl(body_comp_con_0m_tests, "p.value")
  )
# Control group - 6M
body_comp_con_6m_tests <- map(body_comp_vars, ~ con_6m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Control", time = "6M", variables = body_comp_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(body_comp_con_6m_tests, "statistic"),
    p_value = map_dbl(body_comp_con_6m_tests, "p.value")
  )
# Intervention group - 0M
body_comp_int_0m_tests <- map(body_comp_vars, ~ int_0m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Intervention", time = "0M", variables = body_comp_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(body_comp_int_0m_tests, "statistic"),
    p_value = map_dbl(body_comp_int_0m_tests, "p.value")
  )
# Intervention group - 6M
body_comp_int_6m_tests <- map(body_comp_vars, ~ int_6m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Intervention", time = "6M", variables = body_comp_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(body_comp_int_6m_tests, "statistic"),
    p_value = map_dbl(body_comp_int_6m_tests, "p.value")
  )

# Strength variables
strength_vars <- c(
  "pt_knee_60ds_exten", "pt_knee_60ds_flexi",
  "pt_knee_60ds_exten_divided_whole_body_total_mass",
  "pt_knee_60ds_flexi_divided_whole_body_total_mass"
)
# Control group - 0M
strength_con_0m_tests <- map(strength_vars, ~ con_0m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Control", time = "0M", variables = strength_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(strength_con_0m_tests, "statistic"),
    p_value = map_dbl(strength_con_0m_tests, "p.value")
  )
# Control group - 6M
strength_con_6m_tests <- map(strength_vars, ~ con_6m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Control", time = "6M", variables = strength_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(strength_con_6m_tests, "statistic"),
    p_value = map_dbl(strength_con_6m_tests, "p.value")
  )
# Intervention group - 0M
strength_int_0m_tests <- map(strength_vars, ~ int_0m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Intervention", time = "0M", variables = strength_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(strength_int_0m_tests, "statistic"),
    p_value = map_dbl(strength_int_0m_tests, "p.value")
  )
# Intervention group - 6M
strength_int_6m_tests <- map(strength_vars, ~ int_6m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Intervention", time = "6M", variables = strength_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(strength_int_6m_tests, "statistic"),
    p_value = map_dbl(strength_int_6m_tests, "p.value")
  )

# Balance variables
balance_vars <- c(
  "ellipse_eo", "vap_eo", "sdap_eo", "vml_eo", "sdml_eo", "vt_eo",
  "ellipse_ec", "vap_ec", "sdap_ec", "vml_ec", "sdml_ec", "vt_ec"
)
# Control group - 0M
balance_con_0m_tests <- map(balance_vars, ~ con_0m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Control", time = "0M", variables = balance_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(balance_con_0m_tests, "statistic"),
    p_value = map_dbl(balance_con_0m_tests, "p.value")
  )
# Control group - 6M
balance_con_6m_tests <- map(balance_vars, ~ con_6m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Control", time = "6M", variables = balance_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(balance_con_6m_tests, "statistic"),
    p_value = map_dbl(balance_con_6m_tests, "p.value")
  )
# Intervention group - 0M
balance_int_0m_tests <- map(balance_vars, ~ int_0m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Intervention", time = "0M", variables = balance_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(balance_int_0m_tests, "statistic"),
    p_value = map_dbl(balance_int_0m_tests, "p.value")
  )
# Intervention group - 6M
balance_int_6m_tests <- map(balance_vars, ~ int_6m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Intervention", time = "6M", variables = balance_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(balance_int_6m_tests, "statistic"),
    p_value = map_dbl(balance_int_6m_tests, "p.value")
  )

# Physical activity variables
pa_vars <- c("steps", "sb", "lpa", "mpa", "vpa", "mvpa")
# Control group - 0M
pa_con_0m_tests <- map(pa_vars, ~ con_0m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Control", time = "0M", variables = pa_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(pa_con_0m_tests, "statistic"),
    p_value = map_dbl(pa_con_0m_tests, "p.value")
  )
# Control group - 6M
pa_con_6m_tests <- map(pa_vars, ~ con_6m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Control", time = "6M", variables = pa_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(pa_con_6m_tests, "statistic"),
    p_value = map_dbl(pa_con_6m_tests, "p.value")
  )
# Intervention group - 0M
pa_int_0m_tests <- map(pa_vars, ~ int_0m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Intervention", time = "0M", variables = pa_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(pa_int_0m_tests, "statistic"),
    p_value = map_dbl(pa_int_0m_tests, "p.value")
  )
# Intervention group - 6M
pa_int_6m_tests <- map(pa_vars, ~ int_6m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Intervention", time = "6M", variables = pa_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(pa_int_6m_tests, "statistic"),
    p_value = map_dbl(pa_int_6m_tests, "p.value")
  )

# SF36 variables
sf36_vars <- c(
  "physical_functioning", "role_limitations_physical", "bodily_pain",
  "general_health", "vitality", "social_functioning", 
  "role_limitations_emotional", "mental_health", 
  "physical_component", "mental_component"
)
# Control group - 0M
sf36_con_0m_tests <- map(sf36_vars, ~ con_0m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Control", time = "0M", variables = sf36_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(sf36_con_0m_tests, "statistic"),
    p_value = map_dbl(sf36_con_0m_tests, "p.value")
  )
# Control group - 6M
sf36_con_6m_tests <- map(sf36_vars, ~ con_6m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Control", time = "6M", variables = sf36_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(sf36_con_6m_tests, "statistic"),
    p_value = map_dbl(sf36_con_6m_tests, "p.value")
  )
# Intervention group - 0M
sf36_int_0m_tests <- map(sf36_vars, ~ int_0m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Intervention", time = "0M", variables = sf36_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(sf36_int_0m_tests, "statistic"),
    p_value = map_dbl(sf36_int_0m_tests, "p.value")
  )
# Intervention group - 6M
sf36_int_6m_tests <- map(sf36_vars, ~ int_6m[[.x]]) %>% 
  map(shapiro.test)
tibble(group = "Intervention", time = "6M", variables = sf36_vars) %>% 
  mutate(
    shapiro_wilk = map_dbl(sf36_int_6m_tests, "statistic"),
    p_value = map_dbl(sf36_int_6m_tests, "p.value")
  )
