library(lubridate)
library(dplyr)

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
  date <- parts[n - 2]
  hour <- parts[n - 1]
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
  df
}

get_stats <- function(df) {
  total <- nrow(df)
  present <- sum(df$activity)
  inactive <- total - present
  list(
    total = total,
    activity = present,
    inactivity = inactive,
    `animal active` = sprintf("%.1f%% of the time", 100 * present / total)
  )
}

analyze_files <- function(root_path) {
  times <- get_file_times(root_path)
  df <- create_binary_df(times)
  stats <- if (nrow(df) > 0) get_stats(df) else list()
  list(df = df, stats = stats)
}

if (interactive()) {
  data_folder <- "LcF2 0428_0606_2025 (1)"
  result <- analyze_files(data_folder)
  if (nrow(result$df) > 0) {
    print("Statistics:")
    print(result$stats)
    cat("\nFirst few rows:\n")
    print(head(result$df))
    cat(sprintf("\nData frame shape: %d rows × %d columns\n",
                nrow(result$df), ncol(result$df)))
  } else {
    message("No valid files found in the folder.")
  }
}
