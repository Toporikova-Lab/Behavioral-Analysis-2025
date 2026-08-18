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
MW_LD_DD_periods <- read.csv("MW_LD_DD_periods.csv", stringsAsFactors = FALSE)

MW_LD_DD_periods <- MW_LD_DD_periods %>%
  mutate(
    alive      = alive == "True",
    Experiment = Experiment == "True"
  )

MW_LD_DD_periods <- MW_LD_DD_periods %>%
  mutate(
    year_extracted = str_extract(source_file, "20(24|25)"),
    year_extracted = case_when(
      source_file %in% c("MW 0730-0813 Monitor1.txt", "MW 0730-0813 Monitor2.txt") ~ "2025",
      TRUE ~ year_extracted
    )
  )

# Confirm the conversion worked
table(MW_LD_DD_periods$alive, useNA = "ifany")
table(MW_LD_DD_periods$Experiment, useNA = "ifany")

exp_df <- MW_LD_DD_periods %>%
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

# new anovas for diff things 


# Folder where the PDF plots + tables will be saved
output_dir <- "/Users/isabelduarte/Library/CloudStorage/Box-Box/Summer2025-2026_Circadian_Students/Spider behavioral data and analysis/Locomotor activity monitor data/MW behavior for RNA-seq experiment"

# =========================================================
# Load + clean data
# =========================================================
MW_LD_DD_periods <- read.csv("MW_LD_DD_periods.csv", stringsAsFactors = FALSE)

MW_LD_DD_periods <- MW_LD_DD_periods %>%
  mutate(
    alive      = alive == "True",
    Experiment = Experiment == "True"
  )

# ---------------------------------------------------------
# Extract YEAR from source_file
# ---------------------------------------------------------
# Most files have a 4-digit year (2024 or 2025) embedded in the name.
# Two files ("MW 0730-0813 Monitor1.txt" / "Monitor2.txt") have NO year
# in the filename - these are hard-coded to 2025 based on manual
# confirmation of when that data was collected.
MW_LD_DD_periods <- MW_LD_DD_periods %>%
  mutate(
    year_extracted = str_extract(source_file, "20(24|25)"),
    year_extracted = case_when(
      source_file %in% c("MW 0730-0813 Monitor1.txt", "MW 0730-0813 Monitor2.txt") ~ "2025",
      TRUE ~ year_extracted
    )
  )

missing_year <- MW_LD_DD_periods %>% filter(is.na(year_extracted)) %>% distinct(source_file)
if (nrow(missing_year) > 0) {
  warning("These source_file values could not be assigned a year - check manually:\n",
          paste(missing_year$source_file, collapse = "\n"))
}

# ---------------------------------------------------------
# Combine ZT and CT into one sampling_time grouping variable
# ---------------------------------------------------------
# ZT and CT are mutually exclusive per row (a spider has one or the
# other, never both). Here they're merged into a single label like
# "ZT1", "ZT9", "CT5", "CT11" so every ZT and CT value present in a
# given year's data becomes its own group, all compared together in
# one ANOVA on DD_period_h.
MW_LD_DD_periods <- MW_LD_DD_periods %>%
  mutate(
    sampling_time = case_when(
      !is.na(ZT) ~ paste0("ZT", ZT),
      !is.na(CT) ~ paste0("CT", CT),
      TRUE       ~ NA_character_
    )
  )

# Sanity check: confirm year / ZT / CT / sampling_time mapping looks right
# before running any stats. REVIEW THIS OUTPUT.
MW_LD_DD_periods %>%
  distinct(source_file, year_extracted, ZT, CT, sampling_time) %>%
  arrange(year_extracted, sampling_time) %>%
  print(n = Inf)

# ---------------------------------------------------------
# Filter to alive + experimental rows with a valid DD_period_h
# and a non-missing sampling_time
# ---------------------------------------------------------
exp_df <- MW_LD_DD_periods %>%
  filter(
    alive == TRUE,
    Experiment == TRUE,
    !is.na(DD_period_h),
    !is.na(sampling_time)
  ) %>%
  mutate(year_extracted = factor(year_extracted))

nrow(exp_df)
table(exp_df$year_extracted, exp_df$sampling_time, useNA = "ifany")

# Helper to tidy + save both ANOVA and Tukey results
make_tables <- function(aov_model, tukey_model, label) {
  aov_tab   <- tidy(aov_model)
  tukey_tab <- tidy(tukey_model)
  
  write.csv(aov_tab,   file.path(output_dir, paste0(label, "_ANOVA_table.csv")), row.names = FALSE)
  write.csv(tukey_tab, file.path(output_dir, paste0(label, "_Tukey_table.csv")), row.names = FALSE)
  
  list(aov = aov_tab, tukey = tukey_tab)
}

# =========================================================
# Reusable function: run DD_period_h ANOVA + Tukey + plot,
# comparing combined ZT+CT sampling_time groups, within one year
# =========================================================
run_dd_analysis <- function(df, year_label) {
  
  df <- df %>%
    mutate(sampling_time = droplevels(factor(sampling_time)))
  
  n_groups <- n_distinct(df$sampling_time)
  if (n_groups < 2) {
    message("Skipping DD ", year_label,
            ": only ", n_groups, " sampling_time group(s) present - ANOVA needs at least 2.")
    return(invisible(NULL))
  }
  
  group_levels <- levels(df$sampling_time)
  
  set.seed(42)
  df <- df %>%
    mutate(
      x_num = as.numeric(sampling_time),
      x_jit = x_num + runif(n(), -0.1, 0.1)
    )
  
  # ---- Summary table ----
  group_summary <- df %>%
    group_by(sampling_time) %>%
    summarise(
      n           = n(),
      mean_period = mean(DD_period_h, na.rm = TRUE),
      sd_period   = sd(DD_period_h, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(sampling_time)
  print(group_summary, n = 30)
  
  # ---- ANOVA + Tukey by sampling_time (ZT + CT combined) ----
  aov_model   <- aov(DD_period_h ~ sampling_time, data = df)
  print(summary(aov_model))
  tukey_model <- TukeyHSD(aov_model)
  print(tukey_model)
  
  # ---- Assumption check: normality of residuals ----
  sw_test <- shapiro.test(residuals(aov_model))
  message("Shapiro-Wilk test on residuals (DD ", year_label, "): W = ",
          round(sw_test$statistic, 3), ", p = ", format.pval(sw_test$p.value, digits = 3, eps = 0.001))
  print(sw_test)
  
  qq_path <- file.path(output_dir, paste0("DD-", year_label, "-QQplot-residuals.pdf"))
  pdf(qq_path, width = 6, height = 6)
  qqnorm(residuals(aov_model), main = paste0("Q-Q Plot of Residuals - DD ", year_label))
  qqline(residuals(aov_model), col = "red")
  dev.off()
  
  label <- paste0("DD_", year_label, "_by_ZTandCT")
  tabs  <- make_tables(aov_model, tukey_model, label)
  
  # ---- ANOVA label for plot ----
  aov_summary <- summary(aov_model)[[1]]
  anova_df1 <- aov_summary$Df[1]
  anova_df2 <- aov_summary$Df[2]
  f_val     <- round(aov_summary$`F value`[1], 2)
  p_val     <- format.pval(aov_summary$`Pr(>F)`[1], digits = 3, eps = 0.001)
  anova_label <- paste0("One-way ANOVA\nF(", anova_df1, ", ", anova_df2, ") = ", f_val,
                        "\np = ", p_val)
  
  # ---- Tukey pairwise comparisons (significant pairs only), bracket positions ----
  tukey_res <- df %>%
    tukey_hsd(DD_period_h ~ sampling_time) %>%
    filter(p.adj < 0.05) %>%
    add_xy_position(x = "sampling_time", step.increase = 0.1)
  
  # Manually shift the ZT1 vs ZT21 bracket down (adjust the subtracted
  # value below to move it up/down further). Matches either group order.
  tukey_res <- tukey_res %>%
    mutate(y.position = if_else(
      (group1 == "ZT1" & group2 == "ZT21") | (group1 == "ZT21" & group2 == "ZT1"),
      y.position - 1,
      y.position
    ))
  
  # ---- Outliers, flagged per group ----
  outliers <- df %>%
    group_by(sampling_time) %>%
    identify_outliers(DD_period_h) %>%
    ungroup() %>%
    mutate(spider_id_label = gsub("_", " ", spider_id))
  
  # ---- Plot ----
  # ---- Plot title (used for both the on-plot label and the saved filename) ----
  plot_title <- paste0("DD period by sampling time (ZT + CT) - ", year_label)
  filename_safe <- plot_title %>%
    str_replace_all("[()]", "") %>%
    str_replace_all("\\+", "and") %>%
    str_replace_all("\\s+", "-") %>%
    str_replace_all("-{2,}", "-")
  
  p <- ggplot(df, aes(y = DD_period_h)) +
    geom_boxplot(aes(x = x_num, group = sampling_time, fill = sampling_time),
                 width = 0.6, outlier.shape = NA) +
    geom_point(aes(x = x_jit), alpha = 0.4) +
    geom_hline(yintercept = 24, linetype = "dashed", color = "red") +
    { if (nrow(tukey_res) > 0) stat_pvalue_manual(tukey_res, label = "p.adj", tip.length = 0.01) } +
    { if (nrow(outliers) > 0)
      geom_point(data = outliers, aes(x = x_jit, y = DD_period_h),
                 color = "red", shape = 21, size = 2.5, stroke = 1, fill = NA) } +
    { if (nrow(outliers) > 0)
      geom_text_repel(data = outliers, aes(x = x_jit, y = DD_period_h, label = spider_id_label),
                      size = 3, color = "red", min.segment.length = 0) } +
    scale_x_continuous(breaks = seq_along(group_levels), labels = group_levels) +
    labs(title = plot_title,
         y = "Period (h)", x = "Sampling time", fill = "Sampling time") +
    annotate("label", x = Inf, y = Inf, label = anova_label,
             hjust = 1.05, vjust = 1.1, size = 4, fill = "white", linewidth = 0.4,
             label.padding = unit(0.4, "lines")) +
    theme_minimal() +
    theme(
      plot.title   = element_text(family = "Times", face = "bold", size = 18, hjust = 0.5,
                                  margin = margin(b = 12)),
      axis.text.x  = element_text(angle = 45, hjust = 1),
      axis.ticks.x = element_blank()
    )
  
  print(p)
  
  ggsave(
    filename = file.path(output_dir, paste0(filename_safe, "-ANOVA-MW.pdf")),
    plot = p, width = 10, height = 7, units = "in"
  )
  
  invisible(list(aov = tabs$aov, tukey = tabs$tukey, plot = p))
}

# =========================================================
# Run for each year present in the data
# =========================================================
years <- levels(exp_df$year_extracted)

for (yr in years) {
  df_year <- exp_df %>% filter(year_extracted == yr)
  run_dd_analysis(df_year, yr)
}