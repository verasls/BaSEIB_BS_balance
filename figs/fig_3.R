# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(cowplot)
source(here("code", "01_tidy_data.R"))

# Prepare data ------------------------------------------------------------

df_plot <- df_long
df_plot$group <- recode_factor(
  df_plot$group,
  "Control" = "Control Group",
  "Intervention" = "Intervention Group"
)

# Physical functioning plot -----------------------------------------------

PF_plot <- ggplot(
  data = df_plot,
  mapping = aes(x = group, y = physical_functioning, fill = time)
) +
  geom_boxplot() +
  geom_dotplot(
    binaxis = 'y',
    stackdir = 'center',
    stackratio = 1,
    dotsize = 0.7, 
    binwidth = 4,
    position = position_dodge(0.75)
  ) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 130),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.line.y = element_blank(),
  ) +
  labs(y = "Physical functioning (score)              ") +
  coord_cartesian(xlim = c(1, 2)) +
  annotate("segment", x = 0.4, xend = 0.4, y = 0, yend = 105, size = 1) +
  annotate("segment", x = 1, xend = 2, y = 122, yend = 122) +
  annotate("segment", x = 1, xend = 1, y = 119, yend = 122) +
  annotate("segment", x = 2, xend = 2, y = 119, yend = 122) +
  annotate("text", x = 1.5, y = 126, label = expression(paste("Treatment effect: ", italic("p "), "< 0.001"))) +
  annotate("segment", x = 1.75, xend = 2.25, y = 110, yend = 110) +
  annotate("segment", x = 1.75, xend = 1.75, y = 107, yend = 110) +
  annotate("segment", x = 2.25, xend = 2.25, y = 107, yend = 110) +
  annotate("text", x = 2, y = 114, label = expression(paste("Time effect: ", italic("p "), "= 0.001")))

# Role limitations physical plot ------------------------------------------

RLP_plot <- ggplot(
  data = df_plot,
  mapping = aes(x = group, y = role_limitations_physical, fill = time)
) +
  geom_boxplot() +
  geom_dotplot(
    binaxis = 'y',
    stackdir = 'center',
    stackratio = 1,
    dotsize = 0.7, 
    binwidth = 4,
    position = position_dodge(0.75)
  ) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 130),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.line.y = element_blank(),
  ) +
  labs(y = "Role-physical (score)              ") +
  coord_cartesian(xlim = c(1, 2)) +
  annotate("segment", x = 0.4, xend = 0.4, y = 0, yend = 105, size = 1) +
  annotate("segment", x = 1, xend = 2, y = 122, yend = 122) +
  annotate("segment", x = 1, xend = 1, y = 119, yend = 122) +
  annotate("segment", x = 2, xend = 2, y = 119, yend = 122) +
  annotate("text", x = 1.5, y = 126, label = expression(paste("Treatment effect: ", italic("p "), "= 0.015")))

# Bodily pain plot --------------------------------------------------------

BP_plot <- ggplot(
  data = df_plot,
  mapping = aes(x = group, y = bodily_pain, fill = time)
) +
  geom_boxplot() +
  geom_dotplot(
    binaxis = 'y',
    stackdir = 'center',
    stackratio = 1,
    dotsize = 0.7, 
    binwidth = 4,
    position = position_dodge(0.75)
  ) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 130),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.line.y = element_blank(),
  ) +
  labs(y = "Bodily pain (score)              ") +
  coord_cartesian(xlim = c(1, 2)) +
  annotate("segment", x = 0.4, xend = 0.4, y = 0, yend = 105, size = 1) +
  annotate("segment", x = 1, xend = 2, y = 122, yend = 122) +
  annotate("segment", x = 1, xend = 1, y = 119, yend = 122) +
  annotate("segment", x = 2, xend = 2, y = 119, yend = 122) +
  annotate("text", x = 1.5, y = 126, label = expression(paste("Treatment effect: ", italic("p "), "= 0.004"))) +
  annotate("segment", x = 1.75, xend = 2.25, y = 110, yend = 110) +
  annotate("segment", x = 1.75, xend = 1.75, y = 107, yend = 110) +
  annotate("segment", x = 2.25, xend = 2.25, y = 107, yend = 110) +
  annotate("text", x = 2, y = 114, label = expression(paste("Time effect: ", italic("p "), "= 0.005")))

# General health plot -----------------------------------------------------

GH_plot <- ggplot(
  data = df_plot,
  mapping = aes(x = group, y = general_health, fill = time)
) +
  geom_boxplot() +
  geom_dotplot(
    binaxis = 'y',
    stackdir = 'center',
    stackratio = 1,
    dotsize = 0.7, 
    binwidth = 4,
    position = position_dodge(0.75)
  ) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 130),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.line.y = element_blank(),
  ) +
  labs(y = "General health (score)              ") +
  coord_cartesian(xlim = c(1, 2)) +
  annotate("segment", x = 0.4, xend = 0.4, y = 0, yend = 105, size = 1) +
  annotate("segment", x = 1, xend = 2, y = 122, yend = 122) +
  annotate("segment", x = 1, xend = 1, y = 119, yend = 122) +
  annotate("segment", x = 2, xend = 2, y = 119, yend = 122) +
  annotate("text", x = 1.5, y = 126, label = expression(paste("Treatment effect: ", italic("p "), "= 0.001"))) +
  annotate("segment", x = 1.75, xend = 2.25, y = 110, yend = 110) +
  annotate("segment", x = 1.75, xend = 1.75, y = 107, yend = 110) +
  annotate("segment", x = 2.25, xend = 2.25, y = 107, yend = 110) +
  annotate("text", x = 2, y = 114, label = expression(paste("Time effect: ", italic("p "), "< 0.001")))

# Vitality plot -----------------------------------------------------------

V_plot <- ggplot(
  data = df_plot,
  mapping = aes(x = group, y = vitality, fill = time)
) +
  geom_boxplot() +
  geom_dotplot(
    binaxis = 'y',
    stackdir = 'center',
    stackratio = 1,
    dotsize = 0.7, 
    binwidth = 4,
    position = position_dodge(0.75)
  ) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 130),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.line.y = element_blank(),
  ) +
  labs(y = "Vitality (score)              ") +
  coord_cartesian(xlim = c(1, 2)) +
  annotate("segment", x = 0.4, xend = 0.4, y = 0, yend = 105, size = 1) +
  annotate("segment", x = 1, xend = 2, y = 122, yend = 122) +
  annotate("segment", x = 1, xend = 1, y = 119, yend = 122) +
  annotate("segment", x = 2, xend = 2, y = 119, yend = 122) +
  annotate("text", x = 1.5, y = 126, label = expression(paste("Treatment effect: ", italic("p "), "= 0.008"))) +
  annotate("segment", x = 1.75, xend = 2.25, y = 110, yend = 110) +
  annotate("segment", x = 1.75, xend = 1.75, y = 107, yend = 110) +
  annotate("segment", x = 2.25, xend = 2.25, y = 107, yend = 110) +
  annotate("text", x = 2, y = 114, label = expression(paste("Time effect: ", italic("p "), "< 0.001")))

# Social functioning plot -------------------------------------------------

SF_plot <- ggplot(
  data = df_plot,
  mapping = aes(x = group, y = social_functioning, fill = time)
) +
  geom_boxplot() +
  geom_dotplot(
    binaxis = 'y',
    stackdir = 'center',
    stackratio = 1,
    dotsize = 0.7, 
    binwidth = 4,
    position = position_dodge(0.75)
  ) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 130),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.line.y = element_blank(),
  ) +
  labs(y = "Social functioning (score)              ") +
  coord_cartesian(xlim = c(1, 2)) +
  annotate("segment", x = 0.4, xend = 0.4, y = 0, yend = 105, size = 1)

# Role limitations emotional plot -----------------------------------------

RLE_plot <- ggplot(
  data = df_plot,
  mapping = aes(x = group, y = role_limitations_emotional, fill = time)
) +
  geom_boxplot() +
  geom_dotplot(
    binaxis = 'y',
    stackdir = 'center',
    stackratio = 1,
    dotsize = 0.7, 
    binwidth = 4,
    position = position_dodge(0.75)
  ) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 130),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.line.y = element_blank(),
  ) +
  labs(y = "Role-emotional (score)              ") +
  coord_cartesian(xlim = c(1, 2)) +
  annotate("segment", x = 0.4, xend = 0.4, y = 0, yend = 105, size = 1)

# Mental health plot ------------------------------------------------------

MH_plot <- ggplot(
  data = df_plot,
  mapping = aes(x = group, y = mental_health, fill = time)
) +
  geom_boxplot() +
  geom_dotplot(
    binaxis = 'y',
    stackdir = 'center',
    stackratio = 1,
    dotsize = 0.7, 
    binwidth = 4,
    position = position_dodge(0.75)
  ) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 130),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.line.y = element_blank(),
  ) +
  labs(y = "Mental health (score)              ") +
  coord_cartesian(xlim = c(1, 2)) +
  annotate("segment", x = 0.4, xend = 0.4, y = 0, yend = 105, size = 1) +
  annotate("segment", x = 1.75, xend = 2.25, y = 110, yend = 110) +
  annotate("segment", x = 1.75, xend = 1.75, y = 107, yend = 110) +
  annotate("segment", x = 2.25, xend = 2.25, y = 107, yend = 110) +
  annotate("text", x = 2, y = 114, label = expression(paste("Time effect: ", italic("p "), "= 0.022")))

# Physical component plot -------------------------------------------------

PC_plot <- ggplot(
  data = df_plot,
  mapping = aes(x = group, y = physical_component, fill = time)
) +
  geom_boxplot() +
  geom_dotplot(
    binaxis = 'y',
    stackdir = 'center',
    stackratio = 1,
    dotsize = 0.7, 
    binwidth = 4,
    position = position_dodge(0.75)
  ) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 130),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.line.y = element_blank(),
  ) +
  labs(y = "Physical component (score)              ") +
  coord_cartesian(xlim = c(1, 2)) +
  annotate("segment", x = 0.4, xend = 0.4, y = 0, yend = 105, size = 1) +
  annotate("segment", x = 1, xend = 2, y = 122, yend = 122) +
  annotate("segment", x = 1, xend = 1, y = 119, yend = 122) +
  annotate("segment", x = 2, xend = 2, y = 119, yend = 122) +
  annotate("text", x = 1.5, y = 126, label = expression(paste("Treatment effect: ", italic("p "), "< 0.001"))) +
  annotate("segment", x = 1.75, xend = 2.25, y = 110, yend = 110) +
  annotate("segment", x = 1.75, xend = 1.75, y = 107, yend = 110) +
  annotate("segment", x = 2.25, xend = 2.25, y = 107, yend = 110) +
  annotate("text", x = 2, y = 114, label = expression(paste("Time effect: ", italic("p "), "= 0.003")))

# Mental component plot ---------------------------------------------------

MC_plot <- ggplot(
  data = df_plot,
  mapping = aes(x = group, y = mental_component, fill = time)
) +
  geom_boxplot() +
  geom_dotplot(
    binaxis = 'y',
    stackdir = 'center',
    stackratio = 1,
    dotsize = 0.7, 
    binwidth = 4,
    position = position_dodge(0.75)
  ) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 130),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.line.y = element_blank(),
  ) +
  labs(y = "Mental component (score)              ") +
  coord_cartesian(xlim = c(1, 2)) +
  annotate("segment", x = 0.4, xend = 0.4, y = 0, yend = 105, size = 1) +
  annotate("segment", x = 1, xend = 2, y = 122, yend = 122) +
  annotate("segment", x = 1, xend = 1, y = 119, yend = 122) +
  annotate("segment", x = 2, xend = 2, y = 119, yend = 122) +
  annotate("text", x = 1.5, y = 126, label = expression(paste("Treatment effect: ", italic("p "), "= 0.009"))) +
  annotate("segment", x = 1.75, xend = 2.25, y = 110, yend = 110) +
  annotate("segment", x = 1.75, xend = 1.75, y = 107, yend = 110) +
  annotate("segment", x = 2.25, xend = 2.25, y = 107, yend = 110) +
  annotate("text", x = 2, y = 114, label = expression(paste("Time effect: ", italic("p "), "= 0.005")))

# Plot grid ---------------------------------------------------------------

plots <- plot_grid(
  PF_plot + theme(legend.position = "none"), 
  V_plot + theme(legend.position = "none"), 
  RLP_plot + theme(legend.position = "none"), 
  SF_plot + theme(legend.position = "none"), 
  BP_plot + theme(legend.position = "none"), 
  RLE_plot + theme(legend.position = "none"), 
  GH_plot + theme(legend.position = "none"), 
  MH_plot + theme(legend.position = "none"), 
  PC_plot + theme(legend.position = "none"), 
  MC_plot + theme(legend.position = "none"), 
  ncol = 2, nrow = 5, scale = 0.9
)

legend <- get_legend(PF_plot)

plots_grid <- plot_grid(
  plots, legend, ncol = 1, rel_heights = c(1, 0.01)
)

ggsave(
  filename = here("figs", "fig_3.tiff"),
  plot = plots_grid, width = 20, height = 50, dpi = 400, units = "cm"
)
