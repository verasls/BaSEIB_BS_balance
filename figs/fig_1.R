# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(cowplot)
source(here("code", "01_tidy_data.R"))

# Prepare data ------------------------------------------------------------

df_plot <- df
df_plot$group <- recode_factor(
  df_plot$group,
  "Control" = "Control Group",
  "Intervention" = "Intervention Group"
)

# Plot VAP x Weight -------------------------------------------------------

lm_VAP_weight <- lm(delta_vap_eo ~ delta_weight, data = df_plot)
summary(lm_VAP_weight)
shapiro.test(lm_VAP_weight$residuals)
VAP_weight_plot <- ggplot(data = df_plot) +
  geom_point(
    mapping = aes(x = delta_weight, y = delta_vap_eo, shape = group),
    size = 3
  ) +
  geom_smooth(
    mapping = aes(x = delta_weight, y = delta_vap_eo),
    method = "lm",
    se = FALSE,
    color = "black"
  ) +
  scale_shape_manual(values = c(1, 20)) +
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
    x = expression(Delta*" Weight (%)"), 
    y = expression(Delta*" Antero-posterior velocity (%)")
  ) +
  annotate(
    "text", 
    x = min(df_plot$delta_weight, na.rm = TRUE), 
    y = max(df_plot$delta_vap_eo, na.rm = TRUE),
    hjust = 0,
    label = expression(paste(R^2==0.32, ", ", italic(p)<"0.01")),
    parse = TRUE,
    size = 5
  )

# Plot VAP x BMI ----------------------------------------------------------

lm_VAP_BMI <- lm(delta_vap_eo ~ delta_bmi, data = df_plot)
summary(lm_VAP_BMI)
shapiro.test(lm_VAP_BMI$residuals)
VAP_BMI_plot <- ggplot(data = df_plot) +
  geom_point(
    mapping = aes(x = delta_bmi, y = delta_vap_eo, shape = group),
    size = 3
  ) +
  geom_smooth(
    mapping = aes(x = delta_bmi, y = delta_vap_eo),
    method = "lm",
    se = FALSE,
    color = "black"
  ) +
  scale_shape_manual(values = c(1, 20)) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  labs(
    x = expression(Delta*" Body mass index (%)"), 
    y = expression(Delta*" Antero-posterior velocity (%)")
  ) +
  annotate(
    "text", 
    x = min(df_plot$delta_bmi, na.rm = TRUE), 
    y = max(df_plot$delta_vap_eo, na.rm = TRUE),
    hjust = 0,
    label = expression(paste(R^2==0.32, ", ", italic(p)<"0.01")),
    parse = TRUE,
    size = 5
  )

# Plot VAP x Waist --------------------------------------------------------

lm_VAP_waist <- lm(delta_vap_eo ~ delta_waist_circumference, data = df_plot)
summary(lm_VAP_waist)
shapiro.test(lm_VAP_waist$residuals)
VAP_waist_plot <- ggplot(data = df_plot) +
  geom_point(
    mapping = aes(x = delta_waist_circumference, y = delta_vap_eo, shape = group),
    size = 3
  ) +
  geom_smooth(
    mapping = aes(x = delta_waist_circumference, y = delta_vap_eo),
    method = "lm",
    se = FALSE,
    color = "black"
  ) +
  scale_shape_manual(values = c(1, 20)) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  labs(
    x = expression(Delta*" Waist circunference (%)"), 
    y = expression(Delta*" Antero-posterior velocity (%)")
  ) +
  annotate(
    "text", 
    x = min(df_plot$delta_waist_circumference, na.rm = TRUE), 
    y = max(df_plot$delta_vap_eo, na.rm = TRUE),
    hjust = 0,
    label = expression(paste(R^2==0.25, ", ", italic(p)<"0.01")),
    parse = TRUE,
    size = 5
  )

# Plot VAP x Hip ----------------------------------------------------------

lm_VAP_hip <- lm(delta_vap_eo ~ delta_hip_circumference, data = df_plot)
summary(lm_VAP_hip)
shapiro.test(lm_VAP_hip$residuals)
VAP_hip_plot <- ggplot(data = df_plot) +
  geom_point(
    mapping = aes(x = delta_hip_circumference, y = delta_vap_eo, shape = group),
    size = 3
  ) +
  geom_smooth(
    mapping = aes(x = delta_hip_circumference, y = delta_vap_eo),
    method = "lm",
    se = FALSE,
    color = "black"
  ) +
  scale_shape_manual(values = c(1, 20)) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  labs(
    x = expression(Delta*" Hip circunference (%)"), 
    y = expression(Delta*" Antero-posterior velocity (%)")
  ) +
  annotate(
    "text", 
    x = min(df_plot$delta_hip_circumference, na.rm = TRUE), 
    y = max(df_plot$delta_vap_eo, na.rm = TRUE),
    hjust = 0,
    label = expression(paste(R^2==0.37, ", ", italic(p)<"0.01")),
    parse = TRUE,
    size = 5
  )

# Plot VAP x Abs knee strength  -------------------------------------------

lm_VAP_abs_strength <- lm(delta_vap_eo ~ delta_pt_knee_60ds_exten, data = df_plot)
summary(lm_VAP_abs_strength)
shapiro.test(lm_VAP_abs_strength$residuals)
VAP_abs_strength_plot <- ggplot(data = df_plot) +
  geom_point(
    mapping = aes(x = delta_pt_knee_60ds_exten, y = delta_vap_eo, shape = group),
    size = 3
  ) +
  geom_smooth(
    mapping = aes(x = delta_pt_knee_60ds_exten, y = delta_vap_eo),
    method = "lm",
    se = FALSE,
    color = "black"
  ) +
  scale_shape_manual(values = c(1, 20)) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  labs(
    x = expression(Delta*" Knee absolute strength (%)"), 
    y = expression(Delta*" Antero-posterior velocity (%)")
  ) +
  annotate(
    "text", 
    x = min(df_plot$delta_pt_knee_60ds_exten, na.rm = TRUE), 
    y = max(df_plot$delta_vap_eo, na.rm = TRUE),
    hjust = 0,
    label = expression(paste(R^2==0.01, ", ", italic(p)==0.38)),
    parse = TRUE,
    size = 5
  )

# Plot VAP x Rel knee strength  -------------------------------------------

lm_VAP_rel_strength <- lm(delta_vap_eo ~ delta_pt_knee_60ds_exten_divided_whole_body_total_mass, data = df_plot)
summary(lm_VAP_rel_strength)
shapiro.test(lm_VAP_rel_strength$residuals)
VAP_rel_strength_plot <- ggplot(data = df_plot) +
  geom_point(
    mapping = aes(x = delta_pt_knee_60ds_exten_divided_whole_body_total_mass, y = delta_vap_eo, shape = group),
    size = 3
  ) +
  geom_smooth(
    mapping = aes(x = delta_pt_knee_60ds_exten_divided_whole_body_total_mass, y = delta_vap_eo),
    method = "lm",
    se = FALSE,
    color = "black"
  ) +
  scale_shape_manual(values = c(1, 20)) +
  scale_y_continuous(limits = c(- 60, 30)) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  labs(
    x = expression(Delta*" Knee relative strength (%)"), 
    y = expression(Delta*" Antero-posterior velocity (%)")
  ) +
  annotate(
    "text", 
    x = min(df_plot$delta_pt_knee_60ds_exten_divided_whole_body_total_mass, na.rm = TRUE), 
    y = max(df_plot$delta_vap_eo, na.rm = TRUE) + 6,
    hjust = 0,
    label = expression(paste(R^2==0.09, ", ", italic(p)==0.08)),
    parse = TRUE,
    size = 5
  )

# Plot VT x Weight --------------------------------------------------------

lm_VT_weight <- lm(delta_vt_eo ~ delta_weight, data = df_plot)
summary(lm_VT_weight)
shapiro.test(lm_VT_weight$residuals)
VT_weight_plot <- ggplot(data = df_plot) +
  geom_point(
    mapping = aes(x = delta_weight, y = delta_vt_eo, shape = group),
    size = 3
  ) +
  geom_smooth(
    mapping = aes(x = delta_weight, y = delta_vt_eo),
    method = "lm",
    se = FALSE,
    color = "black"
  ) +
  scale_shape_manual(values = c(1, 20)) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  labs(
    x = expression(Delta*" Weight (%)"), 
    y = expression(Delta*" Total velocity (%)")
  ) +
  annotate(
    "text", 
    x = min(df_plot$delta_weight, na.rm = TRUE), 
    y = max(df_plot$delta_vt_eo, na.rm = TRUE),
    hjust = 0,
    label = expression(paste(R^2==0.29, ", ", italic(p)<"0.01")),
    parse = TRUE,
    size = 5
  )

# Plot VT x BMI -----------------------------------------------------------

lm_VT_BMI <- lm(delta_vt_eo ~ delta_bmi, data = df_plot)
summary(lm_VT_weight)
shapiro.test(lm_VT_weight$residuals)
VT_BMI_plot <- ggplot(data = df_plot) +
  geom_point(
    mapping = aes(x = delta_bmi, y = delta_vt_eo, shape = group),
    size = 3
  ) +
  geom_smooth(
    mapping = aes(x = delta_bmi, y = delta_vt_eo),
    method = "lm",
    se = FALSE,
    color = "black"
  ) +
  scale_shape_manual(values = c(1, 20)) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  labs(
    x = expression(Delta*" Body mass index (%)"), 
    y = expression(Delta*" Total velocity (%)")
  ) +
  annotate(
    "text", 
    x = min(df_plot$delta_bmi, na.rm = TRUE), 
    y = max(df_plot$delta_vt_eo, na.rm = TRUE),
    hjust = 0,
    label = expression(paste(R^2==0.29, ", ", italic(p)<"0.01")),
    parse = TRUE,
    size = 5
  )

# Plot VT x Waist ---------------------------------------------------------

lm_VT_waist <- lm(delta_vt_eo ~ delta_waist_circumference, data = df_plot)
summary(lm_VT_waist)
shapiro.test(lm_VT_waist$residuals)
VT_waist_plot <- ggplot(data = df_plot) +
  geom_point(
    mapping = aes(x = delta_waist_circumference, y = delta_vt_eo, shape = group),
    size = 3
  ) +
  geom_smooth(
    mapping = aes(x = delta_waist_circumference, y = delta_vt_eo),
    method = "lm",
    se = FALSE,
    color = "black"
  ) +
  scale_shape_manual(values = c(1, 20)) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  labs(
    x = expression(Delta*" Waist circunference (%)"), 
    y = expression(Delta*" Total velocity (%)")
  ) +
  annotate(
    "text", 
    x = min(df_plot$delta_waist_circumference, na.rm = TRUE), 
    y = max(df_plot$delta_vt_eo, na.rm = TRUE),
    hjust = 0,
    label = expression(paste(R^2==0.22, ", ", italic(p)==0.01)),
    parse = TRUE,
    size = 5
  )

# Plot VT x Hip ----------------------------------------------------------

lm_VT_hip <- lm(delta_vt_eo ~ delta_hip_circumference, data = df_plot)
summary(lm_VT_hip)
shapiro.test(lm_VT_hip$residuals)
VT_hip_plot <- ggplot(data = df_plot) +
  geom_point(
    mapping = aes(x = delta_hip_circumference, y = delta_vt_eo, shape = group),
    size = 3
  ) +
  geom_smooth(
    mapping = aes(x = delta_hip_circumference, y = delta_vt_eo),
    method = "lm",
    se = FALSE,
    color = "black"
  ) +
  scale_shape_manual(values = c(1, 20)) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  labs(
    x = expression(Delta*" Hip circunference (%)"), 
    y = expression(Delta*" Total velocity (%)")
  ) +
  annotate(
    "text", 
    x = min(df_plot$delta_hip_circumference, na.rm = TRUE), 
    y = max(df_plot$delta_vt_eo, na.rm = TRUE),
    hjust = 0,
    label = expression(paste(R^2==0.36, ", ", italic(p)<"0.01")),
    parse = TRUE,
    size = 5
  )

# Plot VT x Abs knee strength  --------------------------------------------

lm_VT_abs_strength <- lm(delta_vt_eo ~ delta_pt_knee_60ds_exten, data = df_plot)
summary(lm_VT_abs_strength)
shapiro.test(lm_VT_abs_strength$residuals)
VT_abs_strength_plot <- ggplot(data = df_plot) +
  geom_point(
    mapping = aes(x = delta_pt_knee_60ds_exten, y = delta_vt_eo, shape = group),
    size = 3
  ) +
  geom_smooth(
    mapping = aes(x = delta_pt_knee_60ds_exten, y = delta_vt_eo),
    method = "lm",
    se = FALSE,
    color = "black"
  ) +
  scale_shape_manual(values = c(1, 20)) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  labs(
    x = expression(Delta*" Knee absolute strength (%)"), 
    y = expression(Delta*" Total velocity (%)")
  ) +
  annotate(
    "text", 
    x = min(df_plot$delta_pt_knee_60ds_exten, na.rm = TRUE), 
    y = max(df_plot$delta_vt_eo, na.rm = TRUE),
    hjust = 0,
    label = expression(paste(R^2==0.01, ", ", italic(p)==0.42)),
    parse = TRUE,
    size = 5
  )

# Plot VT x Rel knee strength  -------------------------------------------

lm_VT_rel_strength <- lm(delta_vt_eo ~ delta_pt_knee_60ds_exten_divided_whole_body_total_mass, data = df_plot)
summary(lm_VT_rel_strength)
shapiro.test(lm_VT_rel_strength$residuals)
VT_rel_strength_plot <- ggplot(data = df_plot) +
  geom_point(
    mapping = aes(x = delta_pt_knee_60ds_exten_divided_whole_body_total_mass, y = delta_vt_eo, shape = group),
    size = 3
  ) +
  geom_smooth(
    mapping = aes(x = delta_pt_knee_60ds_exten_divided_whole_body_total_mass, y = delta_vt_eo),
    method = "lm",
    se = FALSE,
    color = "black"
  ) +
  scale_shape_manual(values = c(1, 20)) +
  scale_y_continuous(
    limits = c(- 40, 45),
    breaks = seq(-40, 40, 20)
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  labs(
    x = expression(Delta*" Knee relative strength (%)"), 
    y = expression(Delta*" Total velocity (%)")
  ) +
  annotate(
    "text", 
    x = min(df_plot$delta_pt_knee_60ds_exten_divided_whole_body_total_mass, na.rm = TRUE), 
    y = max(df_plot$delta_vt_eo, na.rm = TRUE) + 6,
    hjust = 0,
    label = expression(paste(R^2==0.07, ", ", italic(p)==0.09)),
    parse = TRUE,
    size = 5
  )

# Plot grid ---------------------------------------------------------------

plots <- plot_grid(
  VAP_weight_plot + theme(legend.position = "none"), 
  VT_weight_plot + theme(legend.position = "none"), 
  VAP_BMI_plot + theme(legend.position = "none"), 
  VT_BMI_plot + theme(legend.position = "none"), 
  VAP_waist_plot + theme(legend.position = "none"), 
  VT_waist_plot + theme(legend.position = "none"), 
  VAP_hip_plot + theme(legend.position = "none"), 
  VT_hip_plot + theme(legend.position = "none"),
  VAP_abs_strength_plot + theme(legend.position = "none"), 
  VT_abs_strength_plot + theme(legend.position = "none"), 
  VAP_rel_strength_plot + theme(legend.position = "none"),
  VT_rel_strength_plot + theme(legend.position = "none"), 
  labels = c("A", "B", "", "", "", "", "", "", "", "", "", ""),
  align = "h", vjust = 1, label_size = 16,
  ncol = 2, nrow = 6
)

legend <- get_legend(VAP_weight_plot)

plot_grid <- plot_grid(plots, legend, ncol = 1, rel_heights = c(1, 0.03))

ggsave(
  filename = here("figs", "fig_1.tiff"),
  plot = plot_grid, width = 20, height = 60, dpi = 400, units = "cm"
)
