library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

find_files <- function(root_path) {
  list.files(
    path = root_path,
    pattern = "\\.mp4$",
    recursive = TRUE,
    full.names = TRUE
  )
}

extract_datetime <- function(file_path) {
  parts <- strsplit(file_path, "/|\\\\")[[1]]
  n <- length(parts)
  date <- parts[n - 3]
  hour <- parts[n - 2]
  minute <- tools::file_path_sans_ext(parts[n])
  ymd_hm(paste0(date, hour, minute), tz = "UTC")
}

get_file_times <- function(root_path) {
  files <- find_files(root_path)
  times <- vector("list", length(files))
  for (i in seq_along(files)) {
    ts <- try(extract_datetime(files[i]), silent = TRUE)
    if (!inherits(ts, "try-error")) times[[i]] <- ts
  }
  times <- do.call(c, times)
  times[!is.na(times)]
}

create_binary_df <- function(times) {
  if (length(times) == 0) return(data.frame())
  start <- min(times)
  end <- max(times)
  full_range <- seq(from = start, to = end, by = "1 min")
  df <- data.frame(
    datetime = full_range,
    activity = 0L,
    row.names = NULL
  )
  idx <- match(times, full_range)
  df$activity[idx] <- 1L
  df$Light <- NA
  df
}

fill_missing_times <- function(df) {
  start_time <- floor_date(min(df$datetime), unit = "day")
  end_time <- ceiling_date(max(df$datetime), unit = "day") - minutes(1)
  full_index <- seq(start_time, end_time, by = "1 min")
  tibble(datetime = full_index) %>%
    left_join(df, by = "datetime")
}

plot_one_spider <- function(df, title = "Spider Actogram") {
  df <- df %>%
    mutate(
      day = as.Date(datetime),
      time_of_day = hour(datetime) * 60 + minute(datetime),
      light_phase = ifelse(datetime < ymd_hm("2025-06-17 20:20"), "Light", NA)
    )
  
  days <- unique(df$day)
  border_df <- data.frame(
    day = days,
    y = as.factor(days),
    xmin = 0,
    xmax = 1440,
    ymin = as.numeric(as.factor(days)) - 0.45,
    ymax = as.numeric(as.factor(days)) + 0.45
  )
  
  ggplot() +
    geom_tile(data = df %>% filter(light_phase == "Light"),
              aes(x = time_of_day, y = as.factor(day), fill = light_phase),
              width = 1, height = 0.9) +
    geom_tile(data = df %>% filter(activity == 1),
              aes(x = time_of_day, y = as.factor(day)),
              fill = "black", width = 1, height = 0.9) +
    geom_rect(data = border_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              color = "black", fill = NA, linewidth = 0.3) +
    scale_fill_manual(values = c("Light" = "#ffff99"), na.translate = FALSE, name = "Light Cycle") +
    scale_x_continuous(
      breaks = seq(0, 1440, by = 180),
      labels = function(x) sprintf("%02d:%02d", x %/% 60, x %% 60),
      expand = c(0, 0)
    ) +
    labs(title = title, x = "Time of Day", y = "Date") +
    theme_minimal(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      legend.position = "right"
    )
}

analyze_files_fullpath <- function(full_path, save_csv = TRUE) {
  experiment_name <- basename(normalizePath(full_path))
  times <- get_file_times(full_path)
  df <- create_binary_df(times)
  
  if (save_csv && nrow(df) > 0) {
    out_path <- file.path(full_path, paste0(gsub("[ /]", "_", experiment_name), "_binary.csv"))
    write.csv(df, out_path, row.names = FALSE)
    message("Saved binary CSV to: ", out_path)
  } else {
    message("No activity detected — CSV not saved.")
  }
  
  list(df = df)
}

full_path <- "C:/Users/user/Box/Summer2025_Circadian_Students/Spider behavioral data and analysis/Video recording data/Raw Data/LCF10 0624_0606_25"

result <- analyze_files_fullpath(full_path)
experiment_name <- basename(normalizePath(full_path))
csv_path <- file.path(full_path, paste0(gsub("[ /]", "_", experiment_name), "_binary.csv"))

if (file.exists(csv_path)) {
  df <- read_csv(csv_path, show_col_types = FALSE)
  df$datetime <- ymd_hms(df$datetime, quiet = TRUE)
  df <- df[!is.na(df$datetime), ]
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
