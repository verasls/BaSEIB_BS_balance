# Load packages  and functions ------------------------------------------------

library(here)
library(tidyverse)
library(janitor)
source(here("code", "functions", "percent_change.R"))
source(here("code", "functions", "clean_sample_size.R"))

# Load and tidy data ----------------------------------------------------------

df <- read_delim(here("data", "data.csv"), delim = ";") %>%
  clean_names() %>% 
  rename(age = age_0m) %>% 
  # Recode factors
  mutate(
    group = recode_factor(
      as.factor(group), "0" = "Control", "1" = "Intervention"
    ),
    sex = recode_factor(as.factor(sex), "0" = "Female", "1" = "Male")
  ) %>% 
  # Correct the type of variables wrongly parsed as character
  mutate_if(is.character, as.numeric)

# Clean sample size
args_css <- list(
  var = c(
    "weight","bmi","waist_circumference","hip_circumference","waist_hip_ratio",
    "pt_knee_60ds_exten", "pt_knee_60ds_exten_divided_whole_body_total_mass",
    "ellipse_eo","vap_eo", "sdap_eo","vml_eo","sdml_eo","vt_eo","steps","sb",
    "lpa","mvpa", "physical_functioning","role_limitations_physical",
    "bodily_pain", "general_health","vitality","social_functioning",
    "role_limitations_emotional","mental_health","physical_component",
    "mental_component"
  ),
  m1 = rep("0m", 31),
  m2 = rep("6m", 31)
)
for (i in 1:length(args_css[[1]])) {
 df <- clean_sample_size(df, args_css$var[i], args_css$m1[i], args_css$m2[i])
}

# Compute deltas
baseline <- map_chr(args_css, ~ paste0(.x, "_0m"))
followup <- map_chr(args_css, ~ paste0(.x, "_6m"))
var_name <- map_chr(args_css, ~ paste0("delta_", .x))
args_pc <- list(baseline = baseline, followup = followup, var_name = var_name)
for (i in 1:length(args_pc[[1]])) {
  df <- percent_change(
    df, args_pc$baseline[i], args_pc$followup[i], args_pc$var_name[i]
  )
}

# Create a new data frame in long form
# Select and rename variables with baseline values
df_0m <- df %>% 
  select(id, group, sex, age, height, ends_with("_0m")) %>% 
  add_column(time = "Baseline", .after = "group")
names(df_0m)[7:ncol(df_0m)] <- df_0m %>% 
  select(ends_with("_0m")) %>% 
  names() %>% 
  map_chr(str_sub, start = 1, end = - 4)

# Select and rename variables with follow-up values
df_6m <- df %>% 
  select(id, group, sex, age, height, ends_with("_6m")) %>% 
  add_column(time = "6 months after", .after = "group")
names(df_6m)[7:ncol(df_6m)] <- df_6m %>% 
  select(ends_with("_6m")) %>% 
  names() %>% 
  map_chr(str_sub, start = 1, end = - 4)

# Join both data frames
df_long <- df_0m %>% 
  full_join(df_6m) %>% 
  mutate(time = as_factor(time))
