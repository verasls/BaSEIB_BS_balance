# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(cowplot)
source(here("code", "01_tidy_data.R"))

# Build plot data frames --------------------------------------------------

df_plot <- df
df_plot$group <- recode_factor(
  df_plot$group,
  "Control" = "Control Group",
  "Intervention" = "Intervention Group"
)

# Steps
steps_plot_data <- df_plot %>%
  select(id, group, steps_0m, steps_6m) %>% 
  na.omit() %>% 
  pivot_longer(
    c("steps_0m", "steps_6m"),
    names_to = "Time",
    values_to = "Steps"
  ) %>% 
  mutate(
    Time = recode_factor(
      Time,
      steps_0m = "Baseline",
      steps_6m = "6 months after"
    )
  ) %>%  
  group_by(Time, group) %>% 
  summarise(
    n = n(),
    mean = mean(Steps),
    sd = sd(Steps)
  ) %>%  
  mutate(
    uCI = mean + (qt(0.975, df = n - 1) * sd / sqrt(n)),
    lCI = mean - (qt(0.975, df = n - 1) * sd / sqrt(n))
  )

# Sedentary behavior
SB_plot_data <- df_plot %>% 
  select(id, group, sb_0m, sb_6m) %>% 
  na.omit() %>% 
  pivot_longer(
    c("sb_0m", "sb_6m"),
    names_to = "Time",
    values_to = "SB"
  ) %>% 
  mutate(
    Time = recode_factor(
      Time,
      sb_0m = "Baseline",
      sb_6m = "6 months after"
    )
  ) %>%  
  group_by(Time, group) %>% 
  summarise(
    n = n(),
    mean = mean(SB),
    sd = sd(SB)
  ) %>%  
  mutate(
    uCI = mean + (qt(0.975, df = n - 1) * sd / sqrt(n)),
    lCI = mean - (qt(0.975, df = n - 1) * sd / sqrt(n))
  )

# Light physical activity
LPA_plot_data <- df_plot %>% 
  select(id, group, lpa_0m, lpa_6m) %>% 
  na.omit() %>% 
  pivot_longer(
    c("lpa_0m", "lpa_6m"),
    names_to = "Time",
    values_to = "LPA"
  ) %>% 
  mutate(
    Time = recode_factor(
      Time,
      lpa_0m = "Baseline",
      lpa_6m = "6 months after"
    )
  ) %>%  
  group_by(Time, group) %>% 
  summarise(
    n = n(),
    mean = mean(LPA),
    sd = sd(LPA)
  ) %>%  
  mutate(
    uCI = mean + (qt(0.975, df = n - 1) * sd / sqrt(n)),
    lCI = mean - (qt(0.975, df = n - 1) * sd / sqrt(n))
  )

# Moderate-tot-vigorous physical activity
MVPA_plot_data <- df_plot %>% 
  select(id, group, mvpa_0m, mvpa_6m) %>% 
  na.omit() %>% 
  pivot_longer(
    c("mvpa_0m", "mvpa_6m"),
    names_to = "Time",
    values_to = "MVPA"
  ) %>% 
  mutate(
    Time = recode_factor(
      Time,
      mvpa_0m = "Baseline",
      mvpa_6m = "6 months after"
    )
  ) %>%  
  group_by(Time, group) %>% 
  summarise(
    n = n(),
    mean = mean(MVPA),
    sd = sd(MVPA)
  ) %>%  
  mutate(
    uCI = mean + (qt(0.975, df = n - 1) * sd / sqrt(n)),
    lCI = mean - (qt(0.975, df = n - 1) * sd / sqrt(n))
  )

# Set position dodge
dodge <- position_dodge(0.2)

# Steps plot --------------------------------------------------------------

steps_plot <- ggplot(data = steps_plot_data) +
  geom_point(
    aes(x = Time, y = mean, shape = group),
    position = dodge, size = 3
  ) +
  geom_line(
    aes(x = Time, y = mean, linetype = group, group = group),
    position = dodge, size = 0.6
  ) +
  geom_errorbar(
    aes(x = Time, ymin = lCI, ymax = uCI, group = group), 
    position = dodge, size = 0.6, width = 0.1
  ) +
  scale_y_continuous(
    limits = c(0, 10200),
    breaks = seq(0, 10000, 1000),
    expand = c(0, 0)
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  labs(
    x = "",
    y = quote("Steps"~(n%.%day^-1))
  ) +
  annotate("segment", x = 2.15, xend = 2.15, y = 3100, yend = 9200) +
  annotate("segment", x = 2.10, xend = 2.15, y = 3100, yend = 3100) +
  annotate("segment", x = 2.10, xend = 2.15, y = 9200, yend = 9200) +
  annotate(
    "text", x = 2.25, y = 6150, angle = 90, 
    label = expression(paste("Treatment effect: ", italic("p "), "= 0.016"))
  ) +
  annotate("text", x = 1.5, y = 6300, label = "*", size = 9)

# Sedentary behavior plot -------------------------------------------------

SB_plot <- ggplot(data = SB_plot_data) +
  geom_point(
    aes(x = Time, y = mean, shape = group),
    position = dodge, size = 3
  ) +
  geom_line(
    aes(x = Time, y = mean, linetype = group, group = group),
    position = dodge, size = 0.6
  ) +
  geom_errorbar(
    aes(x = Time, ymin = lCI, ymax = uCI, group = group), 
    position = dodge, size = 0.6, width = 0.1
  ) +
  scale_y_continuous(
    limits = c(0, 730),
    breaks = seq(0, 700, 100),
    expand = c(0, 0)
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  labs(
    x = "",
    y = quote("Sedentary behavior"~(min%.%day^-1))
  ) +
  annotate("segment", x = 2.15, xend = 2.15, y = 300, yend = 610) +
  annotate("segment", x = 2.10, xend = 2.15, y = 300, yend = 300) +
  annotate("segment", x = 2.10, xend = 2.15, y = 610, yend = 610) +
  annotate(
    "text", x = 2.25, y = 450, angle = 90, 
    label = expression(paste("Treatment effect: ", italic("p "), "= 0.002"))
  ) 

# Light physical activity plot --------------------------------------------

LPA_plot <- ggplot(data = LPA_plot_data) +
  geom_point(
    aes(x = Time, y = mean, shape = group),
    position = dodge, size = 3
  ) +
  geom_line(
    aes(x = Time, y = mean, linetype = group, group = group),
    position = dodge, size = 0.6
  ) +
  geom_errorbar(
    aes(x = Time, ymin = lCI, ymax = uCI, group = group), 
    position = dodge, size = 0.6, width = 0.1
  ) +
  scale_y_continuous(
    limits = c(0, 520),
    breaks = seq(0, 500, 100),
    expand = c(0, 0)
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  labs(
    x = "",
    y = quote("Light physical activity"~(min%.%day^-1))
  )

# Moderate-to-vigorous physical activity plot -----------------------------

MVPA_plot <- ggplot(data = MVPA_plot_data) +
  geom_point(
    aes(x = Time, y = mean, shape = group),
    position = dodge, size = 3
  ) +
  geom_line(
    aes(x = Time, y = mean, linetype = group, group = group),
    position = dodge, size = 0.6
  ) +
  geom_errorbar(
    aes(x = Time, ymin = lCI, ymax = uCI, group = group), 
    position = dodge, size = 0.6, width = 0.1
  ) +
  scale_y_continuous(
    limits = c(0, 62),
    breaks = seq(0, 60, 10),
    expand = c(0, 0)
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  labs(
    x = "",
    y = expression(
      atop(
        NA,
        atop(
          textstyle("Moderate-to-vigorous"),
          textstyle("physical activity"~(min%.%day^-1))
        )
      )
    )
  )

# Plot grid ---------------------------------------------------------------

plots <- plot_grid(
  steps_plot + theme(legend.position = "none"), 
  SB_plot + theme(legend.position = "none"), 
  LPA_plot + theme(legend.position = "none"), 
  MVPA_plot + theme(legend.position = "none"),
  align = "h", vjust = 1,
  ncol = 4, nrow = 1
)

legend <- get_legend(steps_plot)

plot_grid <- plot_grid(plots, legend, ncol = 1, rel_heights = c(1, 0.1))

ggsave(
  filename = here("figs", "fig_2.tiff"),
  plot = plot_grid, width = 40, height = 10, dpi = 400, units = "cm"
)
