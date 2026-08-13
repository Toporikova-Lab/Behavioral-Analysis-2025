#load in all necessary packages
library(tidyverse)
library(car)
library(broom)
library(rstatix)
library(ggpubr)
library(ggrepel)

# Folder where the PDF plots will be saved
output_dir <- "/Users/isabelduarte/Library/CloudStorage/Box-Box/Summer2025-2026_Circadian_Students/Spider behavioral data and analysis/Locomotor activity monitor data/MW behavior for RNA-seq experiment"

# =========================================================
# Load + clean data (shared by both DD and LD analyses)
# =========================================================
df <- read.csv("MW_LD_DD_periods.csv", stringsAsFactors = FALSE)

df <- df %>%
  mutate(
    alive      = alive == "True",
    Experiment = Experiment == "True"
  )

# Confirm the conversion worked
table(df$alive, useNA = "ifany")
table(df$Experiment, useNA = "ifany")

exp_df <- df %>%
  filter(alive == TRUE, Experiment == TRUE) %>%
  mutate(
    Lights_On   = droplevels(factor(Lights.On)),
    source_file = droplevels(factor(source_file))
  )

nrow(exp_df)
table(exp_df$Lights_On, useNA = "ifany")
table(exp_df$source_file, useNA = "ifany")

# Numeric x position per group + a jittered version (used by both plots) -- this is so everything lines up
set.seed(42)
exp_df <- exp_df %>%
  mutate(
    x_num = as.numeric(source_file),
    x_jit = x_num + runif(n(), -0.1, 0.1)
  )
group_levels <- levels(exp_df$source_file)

# ---- Summary table: every individual group labeled with its schedule ----
group_summary_dd <- exp_df %>%
  group_by(source_file, Lights_On) %>%
  summarise(
    n           = n(),
    mean_period = mean(DD_period_h, na.rm = TRUE),
    sd_period   = sd(DD_period_h, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Lights_On, source_file)

print(group_summary_dd, n = 30)

# Helper to tidy + save both ANOVA and Tukey results
make_tables <- function(aov_model, tukey_model, label) {
  aov_tab   <- tidy(aov_model)
  tukey_tab <- tidy(tukey_model)
  
  write.csv(aov_tab,   paste0(label, "_ANOVA_table.csv"), row.names = FALSE)
  write.csv(tukey_tab, paste0(label, "_Tukey_table.csv"), row.names = FALSE)
  
  list(aov = aov_tab, tukey = tukey_tab)
}

# =========================================================
# ================        DD ANALYSIS        =============
# =========================================================

# ---- DD Question A: Do individual groups (source_file) differ? ----
dd_aov_by_group <- aov(DD_period_h ~ source_file, data = exp_df)
summary(dd_aov_by_group)
TukeyHSD(dd_aov_by_group)

# ---- DD Question B: Does schedule (Lights_On) alone differ? ----
dd_aov_by_schedule <- aov(DD_period_h ~ Lights_On, data = exp_df)
summary(dd_aov_by_schedule)
TukeyHSD(dd_aov_by_schedule)

# DD by source_file
dd_group_tables <- make_tables(dd_aov_by_group, TukeyHSD(dd_aov_by_group), "DD_by_group")
print(dd_group_tables$aov)
print(dd_group_tables$tukey)

# DD by Lights_On
dd_schedule_tables <- make_tables(dd_aov_by_schedule, TukeyHSD(dd_aov_by_schedule), "DD_by_schedule")
print(dd_schedule_tables$aov)
print(dd_schedule_tables$tukey)

# ---- DD ANOVA label for plot ----
aov_model_dd   <- aov(DD_period_h ~ source_file, data = exp_df)
aov_summary_dd <- summary(aov_model_dd)[[1]]
df1_dd    <- aov_summary_dd$Df[1]
df2_dd    <- aov_summary_dd$Df[2]
f_val_dd  <- round(aov_summary_dd$`F value`[1], 2)
p_val_dd  <- format.pval(aov_summary_dd$`Pr(>F)`[1], digits = 3, eps = 0.001)
anova_label_dd <- paste0("One-way ANOVA\nF(", df1_dd, ", ", df2_dd, ") = ", f_val_dd,
                         "\np = ", p_val_dd)

# ---- DD Tukey pairwise comparisons (significant pairs only), with bracket positions ----
tukey_res_dd <- exp_df %>%
  tukey_hsd(DD_period_h ~ source_file) %>%
  filter(p.adj < 0.05) %>%          # keep only significant pairs
  add_xy_position(x = "source_file", step.increase = 0.1)

# ---- DD outliers, flagged per group ----
outliers_dd <- exp_df %>%
  group_by(source_file) %>%
  identify_outliers(DD_period_h) %>%
  ungroup() %>%
  mutate(spider_id_label = gsub("_", " ", spider_id))

# ---- DD plot ----
p_dd <- ggplot(exp_df, aes(y = DD_period_h)) +
  geom_boxplot(aes(x = x_num, group = source_file, fill = Lights_On),
               width = 0.6, outlier.shape = NA) +
  geom_point(aes(x = x_jit), alpha = 0.4) +
  geom_hline(yintercept = 24, linetype = "dashed", color = "red") +
  stat_pvalue_manual(tukey_res_dd, label = "p.adj", tip.length = 0.01) +
  geom_point(data = outliers_dd, aes(x = x_jit, y = DD_period_h),
             color = "red", shape = 21, size = 2.5, stroke = 1, fill = NA) +
  geom_text_repel(data = outliers_dd, aes(x = x_jit, y = DD_period_h, label = spider_id_label),
                  size = 3, color = "red", min.segment.length = 0) +
  scale_x_continuous(breaks = seq_along(group_levels), labels = group_levels) +
  labs(title = "DD period by individual group (colored by light schedule)",
       y = "Period (h)", x = "Group (source_file)", fill = "Lights On") +
  annotate("label", x = -Inf, y = Inf, label = anova_label_dd,
           hjust = -0.05, vjust = 1.1, size = 4, fill = "white", linewidth = 0.4,
           label.padding = unit(0.4, "lines")) +
  theme_minimal() +
  theme(
    plot.title   = element_text(family = "Times", face = "bold", size = 18, hjust = 0.5,
                                margin = margin(b = 12)),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_blank()
  )

p_dd

ggsave(
  filename = file.path(output_dir, "DD-period-by-group-ANOVA-MW.pdf"),
  plot = p_dd, width = 10, height = 7, units = "in"
)

# =========================================================
# ================        LD ANALYSIS        =============
# =========================================================

# ---- LD Question A: Do individual groups (source_file) differ? ----
ld_aov_by_group <- aov(LD_period_h ~ source_file, data = exp_df)
summary(ld_aov_by_group)
TukeyHSD(ld_aov_by_group)

# ---- LD Question B: Does schedule (Lights_On) alone differ? ----
ld_aov_by_schedule <- aov(LD_period_h ~ Lights_On, data = exp_df)
summary(ld_aov_by_schedule)
TukeyHSD(ld_aov_by_schedule)

# LD by source_file
ld_group_tables <- make_tables(ld_aov_by_group, TukeyHSD(ld_aov_by_group), "LD_by_group")
print(ld_group_tables$aov)
print(ld_group_tables$tukey)

# LD by Lights_On
ld_schedule_tables <- make_tables(ld_aov_by_schedule, TukeyHSD(ld_aov_by_schedule), "LD_by_schedule")
print(ld_schedule_tables$aov)
print(ld_schedule_tables$tukey)

# ---- LD ANOVA label for plot ----
aov_model_ld   <- aov(LD_period_h ~ source_file, data = exp_df)
aov_summary_ld <- summary(aov_model_ld)[[1]]
df1_ld    <- aov_summary_ld$Df[1]
df2_ld    <- aov_summary_ld$Df[2]
f_val_ld  <- round(aov_summary_ld$`F value`[1], 2)
p_val_ld  <- format.pval(aov_summary_ld$`Pr(>F)`[1], digits = 3, eps = 0.001)
anova_label_ld <- paste0("One-way ANOVA\nF(", df1_ld, ", ", df2_ld, ") = ", f_val_ld,
                         "\np = ", p_val_ld)

# ---- LD Tukey pairwise comparisons (significant pairs only), with bracket positions ----
tukey_res_ld <- exp_df %>%
  tukey_hsd(LD_period_h ~ source_file) %>%
  filter(p.adj < 0.05) %>%          # keep only significant pairs
  add_xy_position(x = "source_file", step.increase = 0.1)

# ---- LD outliers, flagged per group ----
outliers_ld <- exp_df %>%
  group_by(source_file) %>%
  identify_outliers(LD_period_h) %>%
  ungroup() %>%
  mutate(spider_id_label = gsub("_", " ", spider_id))

# ---- LD plot ----
p_ld <- ggplot(exp_df, aes(y = LD_period_h)) +
  geom_boxplot(aes(x = x_num, group = source_file, fill = Lights_On),
               width = 0.6, outlier.shape = NA) +
  geom_point(aes(x = x_jit), alpha = 0.4) +
  geom_hline(yintercept = 24, linetype = "dashed", color = "red") +
  stat_pvalue_manual(tukey_res_ld, label = "p.adj", tip.length = 0.01) +
  geom_point(data = outliers_ld, aes(x = x_jit, y = LD_period_h),
             color = "red", shape = 21, size = 2.5, stroke = 1, fill = NA) +
  geom_text_repel(data = outliers_ld, aes(x = x_jit, y = LD_period_h, label = spider_id_label),
                  size = 3, color = "red", min.segment.length = 0) +
  scale_x_continuous(breaks = seq_along(group_levels), labels = group_levels) +
  labs(title = "LD period by individual group (colored by light schedule)",
       y = "Period (h)", x = "Group (source_file)", fill = "Lights On") +
  annotate("label", x = -Inf, y = Inf, label = anova_label_ld,
           hjust = -0.05, vjust = 1.1, size = 4, fill = "white", linewidth = 0.4,
           label.padding = unit(0.4, "lines")) +
  theme_minimal() +
  theme(
    plot.title   = element_text(family = "Times", face = "bold", size = 18, hjust = 0.5,
                                margin = margin(b = 12)),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_blank()
  )

p_ld

ggsave(
  filename = file.path(output_dir, "LD-period-by-group-ANOVA-MW.pdf"),
  plot = p_ld, width = 10, height = 7, units = "in"
)