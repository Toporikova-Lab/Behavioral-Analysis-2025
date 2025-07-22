library(dplyr)
library(lubridate)
library(readr)
library(stringr)
library(tibble)

find_files <- function(root_path) {
  list.files(path = root_path, pattern = "\\.mp4$", recursive = TRUE, full.names = TRUE)
}

extract_datetime <- function(path) {
  parts <- strsplit(path, "/|\\\\")[[1]]
  date <- parts[length(parts) - 3]
  hour <- parts[length(parts) - 2]
  minute <- tools::file_path_sans_ext(parts[length(parts)])
  dt_string <- paste0(date, hour, minute)
  parsed <- try(ymd_hm(dt_string), silent = TRUE)
  if (inherits(parsed, "try-error")) NA else parsed
}

get_file_times <- function(root_path) {
  files <- find_files(root_path)
  times <- lapply(files, extract_datetime)
  times <- do.call(c, times)
  times[!is.na(times)]
}

create_binary_df <- function(times) {
  if (length(times) == 0) return(tibble())
  start <- floor_date(min(times), unit = "min")
  end <- ceiling_date(max(times), unit = "min")
  full_range <- seq(from = start, to = end, by = "1 min")
  df <- tibble(datetime = full_range, activity = 0L)
  df$activity[df$datetime %in% times] <- 1L
  return(df)
}

# ---- Get summary stats ----
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

# ---- Main pipeline ----
analyze_files <- function(root_path) {
  times <- get_file_times(root_path)
  df <- create_binary_df(times)
  stats <- get_stats(df)
  
  out_file <- file.path(root_path, paste0(gsub("[ /]", "_", basename(root_path)), "_binary.csv"))
  write_csv(df, out_file)
  message("Saved binary CSV to: ", out_file)
  
  return(list(df = df, stats = stats))
}

# ---- Run for specific folder ----
folder_path <- "C:/Users/user/Box/Summer2025_Circadian_Students/Spider behavioral data and analysis/Video recording data/Raw Data/Lc F 9 0612_0622_2025"
result <- analyze_files(folder_path)

# Optional: print head and stats
head(result$df)
print(result$stats)
