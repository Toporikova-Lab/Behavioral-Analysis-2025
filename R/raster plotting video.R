# ----- Modified analyze_files_fullpath -----
analyze_files_fullpath <- function(full_path, save_csv = TRUE) {
  experiment_name <- basename(normalizePath(full_path))
  
  times <- get_file_times(full_path)
  df <- create_binary_df(times)
  stats <- if (nrow(df) > 0) get_stats(df) else list()
  
  if (save_csv && nrow(df) > 0) {
    out_path <- file.path(full_path, paste0(gsub("[ /]", "_", experiment_name), "_binary.csv"))
    write.csv(df, out_path, row.names = FALSE)
    message("✅ Saved binary CSV to: ", out_path)
  } else {
    message("⚠️ No activity detected — CSV not saved.")
  }
  
  list(df = df, stats = stats)
}

# ----- Your folder path -----
full_path <- "C:/Users/user/Box/Summer2025_Circadian_Students/Spider behavioral data and analysis/Video recording data/Raw Data/LCF10 0624_0606_25"

# ----- Run analysis -----
result <- analyze_files_fullpath(full_path)

# ----- Load and plot binary data safely -----
library(readr)
library(dplyr)
library(lubridate)

experiment_name <- basename(normalizePath(full_path))
csv_path <- file.path(full_path, paste0(gsub("[ /]", "_", experiment_name), "_binary.csv"))

if (file.exists(csv_path)) {
  df <- read_csv(csv_path, show_col_types = FALSE)
  
  # Only parse datetime if it's character
  if (!inherits(df$datetime, "POSIXct")) {
    df$datetime <- ymd_hms(df$datetime, quiet = TRUE)
  }
  
  # Drop rows where datetime is NA (failed to parse)
  df <- df[!is.na(df$datetime), ]
  
  # ---- Plotting functions (assuming these are already defined in your script) ----
  df_full <- fill_missing_times(df)
  p <- plot_one_spider(df_full, title = experiment_name)
  print(p)
  
  ggsave(
    filename = file.path(full_path, paste0(substr(experiment_name, 1, 6), "_raster.png")),
    plot = p, width = 6, height = 10, dpi = 300
  )
} else {
  warning("CSV file not found. Please check the path or re-run analyze_files_fullpath().")
}

plot_one_spider <- function(df, title = "Spider Actogram") {
  df <- df %>%
    mutate(
      day = as.Date(datetime),
      time_of_day = hour(datetime) * 60 + minute(datetime),
      light_phase = ifelse(is.na(Light) | Light < 1, "Dark", "Light")
    )
  
  ggplot(df, aes(x = time_of_day, y = as.factor(day))) +
    # Light/dark background shading
    geom_tile(aes(fill = light_phase), height = 0.9, width = 1) +
    # Activity bars
    geom_tile(data = df %>% filter(activity == 1),
              aes(fill = "Activity"), height = 0.9, width = 1) +
    scale_fill_manual(
      values = c("Light" = "#ffff99", "Dark" = "#999999", "Activity" = "black"),
      breaks = c("Light", "Dark"),  # Only show Light/Dark in legend
      name = "Light Cycle"
    ) +
    scale_x_continuous(
      breaks = seq(0, 1440, by = 180),
      labels = function(x) sprintf("%02d:%02d", x %/% 60, x %% 60),
      expand = c(0, 0)
    ) +
    labs(
      title = title,
      x = "Time of Day",
      y = "Date"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      legend.position = "right"
    )
}
