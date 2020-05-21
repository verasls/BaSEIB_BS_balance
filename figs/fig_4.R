# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(cowplot)
source(here("code", "01_tidy_data.R"))

# Prepare data ------------------------------------------------------------

# Select SF-36 variables
SF36_df <- df_long %>%
  select(
    id, time, group,
    PF = physical_functioning,
    RP = role_limitations_physical,
    BP = bodily_pain,
    GH = general_health,
    VT = vitality,
    SF = social_functioning,
    RE = role_limitations_emotional,
    MH = mental_health
  )


# Get normative values for the portuguese population. Obtained from: Lopes
# Ferreira et al., 2012 (http://dx.doi.org/10.1016/j.rpsp.2012.12.007)
norm <- tibble(
  component = c(
    "PF", "RP", "BP", "GH",
    "VT", "SF", "RE", "MH"
  ),
  mean = c(
    80.1632, 78.4403, 71.4440, 59.5460,
    63.0078, 79.9599, 79.8306, 72.9736
  ),
  sd = c(
    24.66564, 25.63070, 24.27063, 15.36130, 
    23.06333, 23.38157, 24.70710, 23.27415
  )
)


# Compute Z scores
SF36_df <- SF36_df %>% 
  mutate(
    PF_z = (PF - norm[[1, 2]]) / norm[[1, 3]],
    RP_z = (RP - norm[[2, 2]]) / norm[[2, 3]],
    BP_z = (BP - norm[[3, 2]]) / norm[[3, 3]],
    GH_z = (GH - norm[[4, 2]]) / norm[[4, 3]],
    VT_z = (VT - norm[[5, 2]]) / norm[[5, 3]],
    SF_z = (SF - norm[[6, 2]]) / norm[[6, 3]],
    RE_z = (RE - norm[[7, 2]]) / norm[[7, 3]],
    MH_z = (MH - norm[[8, 2]]) / norm[[8, 3]]
  )

# Compute summary scores
SF36_df <- SF36_df %>%
  mutate(
    PCS_z = (
      + 0.489 * PF_z
      + 0.305 * RP_z
      + 0.344 * BP_z
      + 0.362 * GH_z
      + 0.037 * VT_z
      - 0.298 * SF_z
      - 0.082 * RE_z
      - 0.243 * MH_z
    ),
    MCS_z = (
      - 0.292 * PF_z
      - 0.085 * RP_z
      - 0.148 * BP_z
      - 0.164 * GH_z
      + 0.200 * VT_z
      + 0.534 * SF_z
      + 0.324 * RE_z
      + 0.491 * MH_z
    )
  )

# Center mean to 50 and standard deviation to 10
SF36_df <- SF36_df %>% 
  mutate(
    normalized_PF = PF_z * 10 + 50,
    normalized_RP = RP_z * 10 + 50,
    normalized_BP = BP_z * 10 + 50,
    normalized_GH = GH_z * 10 + 50,
    normalized_VT = VT_z * 10 + 50,
    normalized_SF = SF_z * 10 + 50,
    normalized_RE = RE_z * 10 + 50,
    normalized_MH = MH_z * 10 + 50,
    normalized_PCS = PCS_z * 10 + 50,
    normalized_MCS = MCS_z * 10 + 50
  )

# Put in long form and get scores mean and sd
SF36_df <- SF36_df %>% 
  pivot_longer(
    -c(id, time, group),
    names_to = "component",
    values_to = "score"
  ) %>% 
  group_by(time, group, component) %>% 
  summarise(
    score_mean = mean(score, na.rm = TRUE),
    score_sd = sd(score, na.rm = TRUE)
  )


# Original scores data frame
original_scores_df <- SF36_df %>%
  filter(
    component %in% c(
      "PF", "RP", "BP", "GH",
      "VT", "SF", "RE","MH"
    )
  )
# Reorder component factor levels
original_scores_df$component <- factor(
  original_scores_df$component,
  levels = c(
    "PF", "RP", "BP", "GH", 
    "VT", "SF", "RE", "MH"
  )
)

# Normalized scores data frame
normalized_scores_df <- SF36_df %>% 
  filter(
    component %in% c(
      "normalized_PF", "normalized_RP", "normalized_BP", "normalized_GH",
      "normalized_VT", "normalized_SF", "normalized_RE", "normalized_MH",
      "normalized_PCS", "normalized_MCS"
    )
  )
# Rename normalized scores component levels
normalized_scores_df$component <- str_sub(normalized_scores_df$component, -3)
for (i in 1:length(normalized_scores_df$component)) {
  if (str_detect(normalized_scores_df$component[i], "_")) {
    normalized_scores_df$component[i] <- str_sub(
      normalized_scores_df$component[i], 2, 3
    )
  }
}
# Reorder component factor levels
normalized_scores_df$component <- factor(
  normalized_scores_df$component,
  levels = c(
    "PF", "RP", "BP", "GH", 
    "VT", "SF", "RE", "MH",
    "PCS", "MCS"
  )
)
# Identify the summary scores
normalized_scores_df$summary <- NA
for (i in 1:nrow(normalized_scores_df)) {
  if (normalized_scores_df$component[i] %in% c("PCS", "MCS")) {
    normalized_scores_df$summary[i] <- "Yes"
  } else {
    normalized_scores_df$summary[i] <- "No"
  }
}
normalized_scores_df$summary <- as_factor(normalized_scores_df$summary)

# Separate into time and group
# Original scores
original_CB_df <- filter(
  original_scores_df, 
  group == "Control" & time == "Baseline"
)
original_C6_df <- filter(
  original_scores_df, 
  group == "Control" & time == "6 months after"
)
original_IB_df <- filter(
  original_scores_df, 
  group == "Intervention" & time == "Baseline"
)
original_I6_df <- filter(
  original_scores_df, 
  group == "Intervention" & time == "6 months after"
)
# Normalized scores
normalized_CB_df <- filter(
  normalized_scores_df, 
  group == "Control" & time == "Baseline"
)
normalized_C6_df <- filter(
  normalized_scores_df, 
  group == "Control" & time == "6 months after"
)
normalized_IB_df <- filter(
  normalized_scores_df, 
  group == "Intervention" & time == "Baseline"
)
normalized_I6_df <- filter(
  normalized_scores_df, 
  group == "Intervention" & time == "6 months after"
)

# Original plots ----------------------------------------------------------

original_CB_plot <- ggplot() +
  geom_bar(
    data = original_CB_df, 
    mapping = aes(x = component, y = score_mean),
    stat = "identity",
    color = "black",
    fill = "gray"
  ) +
  geom_line(
    data = norm,
    mapping = aes(x = component, y = mean, group = 1),
    size = 1
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  ) +
  labs(
    title = "Control Group - Baseline",
    y = "SF-36 original scores"
  )

original_C6_plot <- ggplot() +
  geom_bar(
    data = original_C6_df, 
    mapping = aes(x = component, y = score_mean),
    stat = "identity",
    color = "black",
    fill = "gray"
  ) +
  geom_line(
    data = norm,
    mapping = aes(x = component, y = mean, group = 1),
    size = 1
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  ) +
  labs(
    title = "Control Group - 6 months after",
    y = "SF-36 original scores"
  )

original_IB_plot <- ggplot() +
  geom_bar(
    data = original_IB_df, 
    mapping = aes(x = component, y = score_mean),
    stat = "identity",
    color = "black",
    fill = "gray"
  ) +
  geom_line(
    data = norm,
    mapping = aes(x = component, y = mean, group = 1),
    size = 1
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  ) +
  labs(
    title = "Intervention Group - Baseline",
    y = "SF-36 original scores"
  )

original_I6_plot <- ggplot() +
  geom_bar(
    data = original_I6_df, 
    mapping = aes(x = component, y = score_mean),
    stat = "identity",
    color = "black",
    fill = "gray"
  ) +
  geom_line(
    data = norm,
    mapping = aes(x = component, y = mean, group = 1),
    size = 1
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  ) +
  labs(
    title = "Intervention Group - 6 months after",
    y = "SF-36 original scores"
  )

# Normalized plots --------------------------------------------------------

normalized_CB_plot <- ggplot() +
  geom_bar(
    data = normalized_CB_df, 
    mapping = aes(x = component, y = score_mean, fill = summary),
    stat = "identity",
    color = "black"
  ) +
  geom_segment(
    mapping = aes(x = 1, xend = 10, y = 50, yend = 50),
    size = 1
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = c("gray", "gray95")) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  ) +
  labs(
    title = "Control Group - Baseline",
    y = "SF-36 normalized scores"
  )

normalized_C6_plot <- ggplot() +
  geom_bar(
    data = normalized_C6_df, 
    mapping = aes(x = component, y = score_mean, fill = summary),
    stat = "identity",
    color = "black"
  ) +
  geom_segment(
    mapping = aes(x = 1, xend = 10, y = 50, yend = 50),
    size = 1
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = c("gray", "gray95")) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  ) +
  labs(
    title = "Control Group - 6 months after",
    y = "SF-36 normalized scores"
  )

normalized_IB_plot <- ggplot() +
  geom_bar(
    data = normalized_IB_df, 
    mapping = aes(x = component, y = score_mean, fill = summary),
    stat = "identity",
    color = "black"
  ) +
  geom_segment(
    mapping = aes(x = 1, xend = 10, y = 50, yend = 50),
    size = 1
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = c("gray", "gray95")) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  ) +
  labs(
    title = "Intervention Group - Baseline",
    y = "SF-36 normalized scores"
  )

normalized_I6_plot <- ggplot() +
  geom_bar(
    data = normalized_I6_df, 
    mapping = aes(x = component, y = score_mean, fill = summary),
    stat = "identity",
    color = "black"
  ) +
  geom_segment(
    mapping = aes(x = 1, xend = 10, y = 50, yend = 50),
    size = 1
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = c("gray", "gray95")) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  ) +
  labs(
    title = "Intervention Group - 6 months after",
    y = "SF-36 normalized scores"
  )

# Original plot grid ------------------------------------------------------

original_plots <- plot_grid(
  original_CB_plot,
  original_C6_plot,
  original_IB_plot,
  original_I6_plot,
  ncol = 2, nrow = 2
)

ggsave(
  filename = here("figs", "fig_4_original.tiff"),
  plot = original_plots, width = 25, height = 25, dpi = 400, units = "cm"
)

# Normalized plot grid ----------------------------------------------------

normalized_plots <- plot_grid(
  normalized_CB_plot,
  normalized_C6_plot,
  normalized_IB_plot,
  normalized_I6_plot,
  ncol = 2, nrow = 2
)

ggsave(
  filename = here("figs", "fig_4_normalized.tiff"),
  plot = normalized_plots, width = 25, height = 25, dpi = 400, units = "cm"
)
