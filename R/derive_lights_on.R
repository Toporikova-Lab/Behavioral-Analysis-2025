##############################################################################
# derive_lights_on.R
#
# Robustly derive the LD lights-on clock time for a monitor file, whether
# the file ends in DD (permanent dark release) or ends mid-LD (still
# cycling). Works by finding EVERY light-on transition inside the LD
# portion of the file and taking the modal time-of-day, rather than
# relying on a single "last light-off" event.
#
# Requires the raw DAM3 .txt file and, for files with mixed LD/DD segments,
# the day range of the LD segment(s) from light_segments.csv (so DD portions
# don't get mixed into the transition search).
#
# CONFIRMED FILE LAYOUT (verified against MsA_DD-LD_first_group_0624-0708-2024
# _Monitor_3.txt, tab-delimited, no header):
#   field 1  : reading index
#   field 2  : date, format "%d %b %y" e.g. "24 Jun 24"
#   field 3  : time, format "%H:%M:%S"
#   field 4  : status (constant 1 in this file)
#   field 5,7: unused (constant 0)
#   field 6  : monitor number
#   field 8  : "Ct" data-type tag (constant text, not data)
#   field 10 : LIGHT STATUS (0/1) <- confirmed empirically: its first run of
#              zeros exactly matches the DD segment row count in
#              light_segments.csv, and its on/off fraction matches the
#              expected LD-segment proportion.
#   field 11+: 32 activity channels (one per spider position)
#
# Validated on the MsA file: 6/7 light-on transitions at 08:00, one 2-min
# blip at 11:16 (manual light check, not the schedule) correctly outvoted
# by the modal method -> inferred schedule 8am-8pm.
##############################################################################

library(dplyr)
library(readr)

#' Extract modal lights-on time from a DAM3 monitor file
#'
#' @param filepath path to the raw monitor .txt file
#' @param light_col index of the light-status column (0/1). Default 10,
#'   confirmed against the MsA file -- re-verify per file if your export
#'   settings differ (see verify_light_column() below).
#' @param ld_day_range optional c(start_day, end_day) to restrict the
#'   search to the LD portion, using the same day-numbering convention
#'   as light_segments.csv. If NULL, uses the whole file (fine for files
#'   that are LD-only throughout).
#' @param tolerance_min transitions within this many minutes of each
#'   other are treated as "the same" clock time when voting for the mode
#'   (handles logging jitter of a minute or two, and outvotes brief
#'   manual-light-check blips like the one seen in the MsA file)
#'
#' @return a list: inferred_lights_on (HH:MM string), n_days_supporting,
#'   all_transition_times (for inspection), and a flag if agreement is weak
derive_lights_on <- function(filepath, light_col = 10, ld_day_range = NULL,
                             tolerance_min = 2) {
  
  raw <- read_delim(filepath, delim = "\t", col_names = FALSE,
                    show_col_types = FALSE)
  
  raw$datetime <- as.POSIXct(paste(raw[[2]], raw[[3]]), format = "%d %b %y %H:%M:%S")
  raw$light <- raw[[light_col]]
  
  if (!is.null(ld_day_range)) {
    day0 <- min(raw$datetime, na.rm = TRUE)
    raw$day_num <- as.numeric(difftime(raw$datetime, day0, units = "days")) + 1
    raw <- raw %>% filter(day_num >= ld_day_range[1], day_num <= ld_day_range[2])
  }
  
  raw <- raw %>% arrange(datetime)
  raw$prev_light <- lag(raw$light)
  
  # light-on transitions: 0 -> 1
  ons <- raw %>% filter(prev_light == 0, light == 1)
  
  if (nrow(ons) == 0) {
    return(list(inferred_lights_on = NA, n_days_supporting = 0,
                all_transition_times = character(0), weak_agreement = TRUE))
  }
  
  # time-of-day in minutes since midnight
  ons$tod_min <- as.numeric(format(ons$datetime, "%H")) * 60 +
    as.numeric(format(ons$datetime, "%M"))
  
  # cluster times within tolerance_min of each other, vote for the largest cluster
  sorted_tod <- sort(ons$tod_min)
  clusters <- split(sorted_tod, cumsum(c(1, diff(sorted_tod) > tolerance_min)))
  best_cluster <- clusters[[which.max(sapply(clusters, length))]]
  modal_min <- round(median(best_cluster))
  
  weak <- length(best_cluster) < 0.7 * nrow(ons)  # <70% agreement is a flag
  
  list(
    inferred_lights_on = sprintf("%02d:%02d", modal_min %/% 60, modal_min %% 60),
    n_days_supporting = length(best_cluster),
    n_total_transitions = nrow(ons),
    all_transition_times = sprintf("%02d:%02d", ons$tod_min %/% 60, ons$tod_min %% 60),
    weak_agreement = weak
  )
}

#' Sanity-check that a candidate column is really the light channel,
#' by cross-referencing its longest constant-0 run against the DD segment
#' row count from light_segments.csv. Run this once per new file/monitor
#' before trusting derive_lights_on()'s default light_col.
#'
#' @param filepath raw monitor .txt file
#' @param expected_dd_run_length the "n_rows" value for that file's DD
#'   segment, from light_segments.csv
#' @param candidate_cols columns to test (default checks 8:15, the
#'   plausible range based on the confirmed MsA layout)
verify_light_column <- function(filepath, expected_dd_run_length,
                                candidate_cols = 8:15) {
  raw <- read_delim(filepath, delim = "\t", col_names = FALSE,
                    show_col_types = FALSE)
  for (col in candidate_cols) {
    v <- raw[[col]]
    if (!all(v %in% c(0, 1))) next  # not binary, skip
    run_id <- cumsum(c(1, diff(v) != 0))
    run_lengths <- tapply(v, run_id, length)
    if (max(run_lengths) == expected_dd_run_length) {
      cat(sprintf("Column %d matches expected DD run length (%d rows) -- likely the light column\n",
                  col, expected_dd_run_length))
      return(col)
    }
  }
  warning("No column matched the expected DD run length -- inspect manually")
  NULL
}

## ---- usage once raw files are available -----------------------------------
# confirmed on the MsA file (light_col = 10, the default):
# result <- derive_lights_on("MsA DD-LD first group 0624-0708-2024 Monitor 3.txt")
# print(result$inferred_lights_on)   # "08:00"
# print(result$n_days_supporting)    # 6 (of 7 -- one blip correctly outvoted)
#
# for other files, verify the column first in case export settings differ:
# verify_light_column("some_other_file.txt", expected_dd_run_length = 8464)