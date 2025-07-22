library(dplyr)
library(lubridate)
library(readr)
library(stringr)
library(tibble)

find_files <- function(root_path) {
  files <- list.files(path = root_path, pattern = "\\.mp4$", recursive = TRUE, full.names = TRUE)
  message("Found ", length(files), " video files")
  return(files)
}

extract_datetime <- function(path) {
  parts <- strsplit(path, "/|\\\\")[[1]]
  n <- length(parts)
  if (n < 3) return(NA)
  
  date <- parts[n - 2]  
  hour <- parts[n - 1]
  minute <- tools::file_path_sans_ext(parts[n])  # "37"
  
  dt_string <- paste0(date, hour, minute)
  parsed <- tryCatch(ymd_hm(dt_string), error = function(e) NA)
  
  if (is.na(parsed)) warning(" Failed to parse datetime from: ", path)
  return(parsed)
}

get_file_times <- function(root_path) {
  files <- find_files(root_path)
  if (length(files) == 0) {
    warning("No video files found.")
    return(c())
  }
  times <- lapply(files, extract_datetime)
  times <- do.call(c, times)
  valid_times <- times[!is.na(times)]
  message("Parsed ", length(valid_times), " valid timestamps")
  return(valid_times)
}

create_binary_df <- function(times) {
  if (length(times) == 0) {
    warning(" No timestamps found")
    return(tibble())
  }
  start <- floor_date(min(times), unit = "min")
  end <- ceiling_date(max(times), unit = "min")
  full_range <- seq(from = start, to = end, by = "1 min")
  df <- tibble(datetime = full_range, activity = 0L)
  df$activity[df$datetime %in% times] <- 1L
  return(df)
}

get_stats <- function(df) {
  total <- nrow(df)
  active <- sum(df$activity)
  list(
    total = total,
    activity = active,
    inactivity = total - active,
    animal_active = sprintf("%.1f%% of the time", 100 * active / total)
  )
}

analyze_files <- function(root_path) {
  times <- get_file_times(root_path)
  if (length(times) == 0) {
    warning("No usable timestamps found. Check folder structure or naming.")
    return(NULL)
  }
  
  df <- create_binary_df(times)
  stats <- get_stats(df)
  
  out_file <- file.path(root_path, paste0(gsub("[ /]", "_", basename(root_path)), "_binary.csv"))
  write_csv(df, out_file)
  message(" Saved binary CSV to: ", out_file)
  
  return(list(df = df, stats = stats))
}

folder_path <- "C:/Users/user/Box/Summer2025_Circadian_Students/Spider behavioral data and analysis/Video recording data/Raw Data/LcF1 0528_0606_2025"
result <- analyze_files(folder_path)

if (!is.null(result)) {
  print(head(result$df))
  print(result$stats)
}
