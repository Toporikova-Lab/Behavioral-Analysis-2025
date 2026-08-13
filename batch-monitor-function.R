# ============================================================
# Batch Monitor Data Processor - FUNCTION DEFINITIONS ONLY.
#   1. SETUP - libraries, sourced helper scripts
#   2. MONITOR LOG HANDLING - reading/matching log files to channels
#   3. LIGHT REGIME DETECTION - DD start day, multi-phase segments,
#      lights-on clock time (auto-detected, no plotting)
#   4. PER-FILE PROCESSING - process_monitor_file(), the real work
#   5. CHECKPOINT DEDUP - select_latest_per_monitor(), extract_monitor_id()
#   6. BATCH RUNNERS & FAST PREVIEWS - run_batch_analysis() and the
#      check_*() no-plot sanity checks (run these before a full batch)
# ============================================================


# ============================================================
# 1. SETUP
# ============================================================

library(dplyr)
library(stringr)
library(lubridate)   # needed for ymd_hm()
library(readxl)      # needed to read .xlsx/.xls monitor logs (install.packages("readxl") if missing)

source('R/lsp_test.R')
source('R/death_detection.R')


# ============================================================
# 2. MONITOR LOG HANDLING
# ============================================================

# ------------------------------------------------------------
# Reads a monitor log file (CSV or Excel) as a data.frame, or just its
# column names if header_only = TRUE. Dispatches on file extension since
# CSV and Excel need different reader functions.
# ------------------------------------------------------------
read_monitor_log_file <- function(path, header_only = FALSE) {
  ext <- tolower(tools::file_ext(path))
  if (ext == 'csv') {
    if (header_only) names(read.csv(path, nrows = 0)) else read.csv(path)
  } else if (ext %in% c('xlsx', 'xls')) {
    if (header_only) names(read_excel(path, n_max = 0)) else as.data.frame(read_excel(path))
  } else {
    stop(str_interp("unsupported monitor log file type: .${ext}"))
  }
}

# ------------------------------------------------------------
# Finds the monitor log (CSV or Excel) in a folder by content, not
# filename - each group folder's log is named differently, and the ID
# column itself is also named differently across folders (id, Subject
# ID, subject_id, etc.). Normalizes header names (lowercase, strip
# punctuation/spaces) to match a 'channel' column and any recognized ID
# column. Also optionally matches a species-abbreviation column (e.g.
# 'Specie abbreviation') - when present, the real spider name is that
# abbreviation + the ID concatenated (e.g. 'Mw' + 7 = 'Mw7'), since some
# logs store those as two separate columns rather than one full name.
# Reports which actual column names it matched so the caller can rename
# them to 'channel'/'id'/'abbrev'. Returns NA with a note if nothing or
# multiple files match.
# ------------------------------------------------------------
find_monitor_log <- function(folder,
                             id_column_aliases = c('id', 'subjectid', 'spiderid', 'animalid'),
                             abbrev_column_aliases = c('specieabbreviation', 'speciesabbreviation',
                                                       'abbreviation', 'specieabbrev', 'speciesabbrev')) {
  candidate_files <- list.files(folder, pattern = '\\.(csv|xlsx|xls)$', full.names = TRUE, ignore.case = TRUE)
  
  if (length(candidate_files) == 0) {
    return(list(path = NA, note = "no CSV/XLSX files found in folder"))
  }
  
  normalize <- function(x) tolower(gsub('[^a-zA-Z0-9]', '', x))
  
  candidates <- lapply(candidate_files, function(f) {
    header <- tryCatch(read_monitor_log_file(f, header_only = TRUE), error = function(e) character())
    norm <- normalize(header)
    list(file = f,
         channel_col = header[which(norm == 'channel')[1]],
         id_col = header[which(norm %in% id_column_aliases)[1]],
         abbrev_col = header[which(norm %in% abbrev_column_aliases)[1]])
  })
  
  matches <- Filter(function(c) !is.na(c$channel_col) && !is.na(c$id_col), candidates)
  
  describe <- function(m) {
    cols <- str_interp("'${m$channel_col}', '${m$id_col}'")
    if (!is.na(m$abbrev_col)) cols <- str_interp("${cols}, abbreviation column '${m$abbrev_col}'")
    cols
  }
  
  if (length(matches) == 1) {
    m <- matches[[1]]
    return(list(path = m$file, channel_col = m$channel_col, id_col = m$id_col, abbrev_col = m$abbrev_col,
                note = str_interp("found monitor log: ${basename(m$file)} (columns ${describe(m)})")))
  } else if (length(matches) > 1) {
    m <- matches[[1]]
    files_list <- str_c(sapply(matches, function(x) basename(x$file)), collapse = ', ')
    return(list(path = m$file, channel_col = m$channel_col, id_col = m$id_col, abbrev_col = m$abbrev_col,
                note = str_interp("multiple matching files found (${files_list}) - using ${basename(m$file)}, worth checking this is the right one")))
  } else {
    return(list(path = NA, note = str_interp(
      "found file(s) (${str_c(basename(candidate_files), collapse=', ')}) but none had a 'channel' column plus a recognized ID column"
    )))
  }
}


# ============================================================
# 3. LIGHT REGIME DETECTION
# ============================================================

# ------------------------------------------------------------
# Day-numbering anchor for a file: first light-on, or first row if no
# light-on exists. Used only for the numeric section2_start_day path -
# detect_dd_start_day()/detect_light_segments() use the fixed zt_0
# parameter instead, to match combined_plot()'s own internal zt_day.
# ------------------------------------------------------------
resolve_zt_anchor <- function(data) {
  anchor <- data %>% filter(light - lag(light) == 1) %>% pull(datetime) %>% first()
  if (length(anchor) == 0 || is.na(anchor)) {
    return(list(anchor = data$datetime[1], used_fallback = TRUE))
  }
  list(anchor = anchor, used_fallback = FALSE)
}

# ------------------------------------------------------------
# Auto-detects the day DD begins in a file, using the LAST light change
# (either direction) so it works for files starting in DD or LD. Takes
# zt_0 explicitly to match combined_plot()'s own day-numbering.
# ------------------------------------------------------------
detect_dd_start_day <- function(data, zt_0) {
  
  if (!'light' %in% names(data) || !'datetime' %in% names(data)) {
    return(list(day = NA, note = "auto DD-start detection failed - light/datetime column missing"))
  }
  
  total_days <- as.numeric(floor(difftime(max(data$datetime, na.rm = TRUE),
                                          min(data$datetime, na.rm = TRUE), units = 'days'))) + 1
  
  # Light never changes at all - file is entirely DD or entirely LD.
  transitions <- data %>% filter(light != lag(light))
  
  if (nrow(transitions) == 0) {
    if (all(data$light == 0, na.rm = TRUE)) {
      return(list(day = 1, note = "file is DD throughout (light constant off) - no LD portion detected; entire file treated as DD"))
    } else if (all(data$light == 1, na.rm = TRUE)) {
      return(list(day = total_days + 1, note = "file is LD throughout (light constant on) - no DD portion detected; entire file treated as LD"))
    } else {
      return(list(day = total_days + 1, note = "light column has unexpected values (not 0/1) - could not classify regime; entire file treated as LD"))
    }
  }
  
  # Last change: OFF = genuine DD release; ON = still cycling, no release found.
  last_change <- transitions %>% slice_tail(n = 1)
  last_change_dt <- last_change$datetime
  last_change_light <- last_change$light  # value AFTER the change
  
  if (last_change_light != 0) {
    return(list(day = total_days + 1, note = "file ends in LD (light still cycling at the end, no permanent release into darkness within this file) - treated as LD for the single-split value"))
  }
  
  day_of_last_off <- as.numeric(
    floor(difftime(last_change_dt, zt_0, units = 'days')) -
      floor(difftime(data$datetime[1], zt_0, units = 'days')) + 1
  )
  
  note <- str_interp("auto-detected DD start at day ${day_of_last_off} (last light-off: ${last_change_dt})")
  
  list(day = day_of_last_off, note = note)
}

# ------------------------------------------------------------
# Segments a file's light column into ALL regime phases (not just the
# last), for multi-phase protocols like LD -> DD -> LD -> DD. Runs
# shorter than min_regime_hours (default 20h) count as ongoing cycling
# (LD); longer runs are their own constant phase (DD/LL). This also
# absorbs brief sensor noise into the surrounding phase automatically.
# Returns list(segment_id per row, summary data.frame of each phase).
# ------------------------------------------------------------
detect_light_segments <- function(data, zt_0, min_regime_hours = 20) {
  
  if (!'light' %in% names(data) || !'datetime' %in% names(data)) {
    return(NULL)
  }
  
  n <- nrow(data)
  r <- rle(data$light)
  n_runs <- length(r$values)
  run_ends <- cumsum(r$lengths)
  run_starts <- run_ends - r$lengths + 1
  run_duration_hours <- as.numeric(difftime(data$datetime[run_ends], data$datetime[run_starts], units = 'hours'))
  is_long_run <- !is.na(r$values) & !is.na(run_duration_hours) & run_duration_hours >= min_regime_hours
  
  segment_id <- integer(n)
  seg_regime <- character()
  seg_counter <- 0
  pending_cycling_start <- NA_integer_
  
  for (i in seq_len(n_runs)) {
    if (isTRUE(is_long_run[i])) {
      # flush any accumulated cycling (LD) stretch that came before this constant run
      if (!is.na(pending_cycling_start)) {
        seg_counter <- seg_counter + 1
        segment_id[pending_cycling_start:(run_starts[i] - 1)] <- seg_counter
        seg_regime[seg_counter] <- 'LD'
        pending_cycling_start <- NA_integer_
      }
      seg_counter <- seg_counter + 1
      segment_id[run_starts[i]:run_ends[i]] <- seg_counter
      seg_regime[seg_counter] <- if (r$values[i] == 0) 'DD' else if (r$values[i] == 1) 'LL' else 'unknown'
    } else {
      if (is.na(pending_cycling_start)) pending_cycling_start <- run_starts[i]
    }
  }
  # flush a trailing cycling stretch that runs to the end of the file
  if (!is.na(pending_cycling_start)) {
    seg_counter <- seg_counter + 1
    segment_id[pending_cycling_start:n] <- seg_counter
    seg_regime[seg_counter] <- 'LD'
  }
  
  day_of <- function(dt) as.numeric(
    floor(difftime(dt, zt_0, units = 'days')) -
      floor(difftime(data$datetime[1], zt_0, units = 'days')) + 1
  )
  
  summary <- data.frame(segment_id = segment_id, datetime = data$datetime) %>%
    group_by(segment_id) %>%
    summarize(start_dt = min(datetime), end_dt = max(datetime), n_rows = dplyr::n(), .groups = 'drop') %>%
    arrange(segment_id) %>%
    mutate(regime = seg_regime[segment_id],
           start_day = day_of(start_dt),
           end_day = day_of(end_dt)) %>%
    select(segment_id, regime, start_day, end_day, start_dt, end_dt, n_rows)
  
  list(segment_id = segment_id, summary = summary)
}

# ------------------------------------------------------------
# Derives the modal LD lights-on clock time from already-parsed monitor
# data (data$light / data$datetime, as produced by process()). Finds
# EVERY 0->1 light transition and takes the modal time-of-day rather
# than relying on a single event - robust to brief manual light checks
# (confirmed on the MsA file: a 2-min blip was correctly outvoted 6-to-1
# by the real 08:00 schedule).
# ------------------------------------------------------------
derive_lights_on_from_data <- function(data, tolerance_min = 2) {
  
  if (!'light' %in% names(data) || !'datetime' %in% names(data)) {
    return(list(inferred_lights_on = NA, n_days_supporting = 0, n_total_transitions = 0,
                weak_agreement = TRUE, note = "light/datetime column missing"))
  }
  
  data <- data %>% arrange(datetime)
  data$prev_light <- lag(data$light)
  ons <- data %>% filter(prev_light == 0, light == 1)  # 0 -> 1 transitions
  
  if (nrow(ons) == 0) {
    return(list(inferred_lights_on = NA, n_days_supporting = 0, n_total_transitions = 0,
                weak_agreement = TRUE, note = "no light-on transitions found in this file"))
  }
  
  ons$tod_min <- as.numeric(format(ons$datetime, "%H")) * 60 + as.numeric(format(ons$datetime, "%M"))
  
  # cluster transition times within tolerance_min of each other, vote for the largest cluster
  sorted_tod <- sort(ons$tod_min)
  clusters <- split(sorted_tod, cumsum(c(1, diff(sorted_tod) > tolerance_min)))
  best_cluster <- clusters[[which.max(sapply(clusters, length))]]
  modal_min <- round(median(best_cluster))
  
  weak <- length(best_cluster) < 0.7 * nrow(ons)  # <70% agreement is a flag
  
  list(
    inferred_lights_on = sprintf("%02d:%02d", modal_min %/% 60, modal_min %% 60),
    n_days_supporting = length(best_cluster),
    n_total_transitions = nrow(ons),
    weak_agreement = weak,
    note = if (weak) str_interp("weak agreement: only ${length(best_cluster)}/${nrow(ons)} transitions match the modal time - worth inspecting manually") else ""
  )
}

# ------------------------------------------------------------
# Sanity-checks that a candidate RAW column is really the light channel,
# by cross-referencing its longest constant-0 run against a known DD
# segment row count (e.g. from light_segments.csv). Operates on the raw
# .txt file directly (not the process()-parsed data), since this is for
# verifying process()'s own column assumptions - run once per file/export
# format before trusting derive_lights_on_from_data() on files from a new
# source.
# ------------------------------------------------------------
verify_light_column <- function(filepath, expected_dd_run_length, candidate_cols = 8:15) {
  raw <- read.delim(filepath, header = FALSE, sep = "\t")
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


# ============================================================
# 4. PER-FILE PROCESSING
# ============================================================

# ------------------------------------------------------------
# Process a single monitor .txt file
# ------------------------------------------------------------
process_monitor_file <- function(filename,
                                 group_name,
                                 section2_start_day = 7,
                                 dd_days = NULL,
                                 rasterplots_only = FALSE,
                                 zt_0 = ymd_hm('01-1-1 8:00'),
                                 output_root = './output') {
  
  flags <- character()  # collects anything worth flagging for this file
  
  subfolder_name <- filename %>%
    str_split_1('/') %>%
    last() %>%
    str_remove('.txt$')
  
  # Nest output under group + file name so same-named files across
  # different LC_ folders don't overwrite each other.
  out_dir <- file.path(output_root, group_name, subfolder_name)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ---- monitor log ----
  monitor_log_folder <- filename %>%
    str_split_1('/') %>%
    head(-1) %>%
    str_c(collapse = '/')
  
  found_log <- find_monitor_log(monitor_log_folder)
  
  if (!is.na(found_log$path)) {
    flags <- c(flags, found_log$note)
    monitor_log <- tryCatch(read_monitor_log_file(found_log$path), error = function(e) {
      flags <<- c(flags, str_interp("failed to read monitor log: ${conditionMessage(e)}"))
      NULL
    })
    if (is.null(monitor_log)) {
      monitor_log <- data.frame(channel = 1:32, id = paste0('s', 1:32))
    } else {
      # Rename whatever the actual channel/ID columns are called to the
      # standard 'channel'/'id' names find_monitor_log() matched on.
      names(monitor_log)[names(monitor_log) == found_log$channel_col] <- 'channel'
      names(monitor_log)[names(monitor_log) == found_log$id_col] <- 'id'
      # Force id to character - some logs have numeric-looking subject IDs
      # (e.g. 101, 102) that read_excel()/read.csv() infer as numeric,
      # which clashes with spider_data's character-typed name column.
      monitor_log$id <- as.character(monitor_log$id)
      # If a species-abbreviation column was matched, the real spider name
      # is abbreviation + id concatenated (e.g. 'Mw' + 7 = 'Mw7') - some
      # logs store those as two separate columns rather than one full name.
      if (!is.na(found_log$abbrev_col)) {
        abbrev <- str_trim(as.character(monitor_log[[found_log$abbrev_col]]))
        monitor_log$id <- ifelse(!is.na(abbrev) & abbrev != '', paste0(abbrev, monitor_log$id), monitor_log$id)
      }
      if (any(duplicated(monitor_log$channel))) {
        flags <- c(flags, "monitor log has duplicate channel numbers")
      }
    }
  } else {
    flags <- c(flags, str_interp("no monitor log found (${found_log$note}) - defaulted to generic s1-s32 IDs"))
    monitor_log <- data.frame(channel = 1:32, id = paste0('s', 1:32))
  }
  
  # ---- load data ----
  data <- tryCatch(process(filename), error = function(e) {
    flags <<- c(flags, str_interp("process() failed: ${conditionMessage(e)}"))
    NULL
  })
  
  if (is.null(data) || nrow(data) == 0) {
    flags <- c(flags, "no usable data rows returned by process() - file skipped")
    return(list(subfolder_name = subfolder_name, flags = flags))
  }
  
  if (!'light' %in% names(data)) {
    flags <- c(flags, "'light' column missing - LD/DD split will be skipped")
  }
  if (!'datetime' %in% names(data)) {
    flags <- c(flags, "'datetime' column missing")
  }
  
  # Resolve "auto" into a real day number, using the same zt_0 that
  # combined_plot() below will use. used_auto controls the LD/DD split
  # branch further down (single-split vs multi-phase segmentation).
  used_auto <- identical(section2_start_day, "auto")
  if (used_auto) {
    detected <- detect_dd_start_day(data, zt_0)
    flags <- c(flags, detected$note)
    if (is.na(detected$day)) {
      flags <- c(flags, "no light/datetime data to fall back on either - defaulting to section2_start_day = 7")
      section2_start_day <- 7
    } else {
      section2_start_day <- detected$day
    }
  }
  
  # Caps the DD section to dd_days past the split point (LD is already
  # bounded). used_auto must anchor on the same fixed zt_0 as
  # section2_start_day; numeric mode keeps its original per-file anchor.
  if (!is.null(dd_days)) {
    if (!'light' %in% names(data)) {
      flags <- c(flags, "dd_days requested but 'light' column missing - cannot anchor window, using full data instead")
    } else {
      if (used_auto) {
        zt_0_window <- zt_0
      } else {
        anchor_result <- resolve_zt_anchor(data)
        zt_0_window <- anchor_result$anchor
        if (anchor_result$used_fallback) {
          flags <- c(flags, "dd_days window: no light-on found - anchoring window on the file's first row instead")
        }
      }
      
      data <- data %>%
        mutate(
          .window_diff = difftime(datetime, zt_0_window, units = 'days'),
          .window_day = floor(.window_diff) - floor(.window_diff[1]) + 1
        )
      
      last_day_needed <- section2_start_day - 1 + dd_days
      available_days <- max(data$.window_day, na.rm = TRUE)
      
      if (available_days < last_day_needed) {
        flags <- c(flags, str_interp(
          "only ${available_days} day(s) of data available after the anchor - fewer than needed to cover section2_start_day (${section2_start_day}) + dd_days (${dd_days}); using what's available"
        ))
      }
      
      data <- data %>%
        filter(.window_day >= 1, .window_day <= last_day_needed) %>%
        select(-.window_diff, -.window_day)
      
      if (nrow(data) == 0) {
        flags <- c(flags, "windowing to section2_start_day/dd_days left 0 rows - check zt_0/light column and file coverage")
        return(list(subfolder_name = subfolder_name, flags = flags))
      }
    }
  }
  
  # Segments the (possibly dd_days-trimmed) data ONCE here, so the choice
  # of plotting method below and the actual segment files written further
  # down always agree with each other - previously this was computed
  # separately before AND after dd_days trimming, which could disagree if
  # dd_days cut off a phase that existed in the full file.
  #
  # combined_plot()'s peak-period split needs section2_start_day to leave
  # BOTH a real "before" period and a real "from" period. That's not just
  # a 3+-phase problem - a 2-phase file that never reaches a genuine DD
  # release (fallback: entire file treated as LD, split placed past the
  # end of the data) or one that's DD from row 1 (split at day 1, nothing
  # before it) breaks the same way with only 1-2 phases. use_rasterplot_
  # fallback catches both: phase count OR a split day outside the data's
  # actual day range.
  seg_result <- NULL
  is_multiphase <- FALSE
  use_rasterplot_fallback <- FALSE
  # Minimum actual elapsed time (not day-bucket count - a "day 9-9" phase
  # can span anywhere from minutes to ~48h depending on exactly where in
  # the day the transitions fall) for a periodogram to have any chance of
  # detecting a period. lsp_plot() searches 14-34h periods by default;
  # phases shorter than this just don't contain enough data.
  min_periodogram_hours <- 24
  
  if (used_auto) {
    seg_result <- detect_light_segments(data, zt_0)
    if (!is.null(seg_result) && nrow(seg_result$summary) > 0) {
      # Chronological label per phase (DD1, LD1, DD2, ...) - computed once
      # here and reused both for periodogram filenames below and for the
      # segment CSV filenames in the final LD/DD split section, so the two
      # always match up. periodogram_eligible checked here too, once per
      # phase rather than repeating a cryptic lsp_plot() error once per
      # channel below.
      seg_result$summary <- seg_result$summary %>%
        group_by(regime) %>%
        mutate(seg_label = paste0(regime, row_number())) %>%
        ungroup() %>%
        arrange(segment_id) %>%
        mutate(duration_hours = as.numeric(difftime(end_dt, start_dt, units = 'hours')),
               periodogram_eligible = duration_hours >= min_periodogram_hours)
      
      too_short <- seg_result$summary %>% filter(!periodogram_eligible)
      if (nrow(too_short) > 0) {
        short_list <- str_c(str_interp("${too_short$seg_label} (${round(too_short$duration_hours, 1)}h)"), collapse = ", ")
        flags <- c(flags, str_interp("skipping periodogram for phase(s) too short to analyze (< ${min_periodogram_hours}h of actual data): ${short_list}"))
      }
    }
    if (!is.null(seg_result) && nrow(seg_result$summary) > 2) {
      is_multiphase <- TRUE
    }
    
    total_days_now <- as.numeric(floor(difftime(max(data$datetime, na.rm = TRUE),
                                                min(data$datetime, na.rm = TRUE), units = 'days'))) + 1
    has_valid_split <- section2_start_day > 1 && section2_start_day <= total_days_now
    use_rasterplot_fallback <- is_multiphase || !has_valid_split
    
    if (use_rasterplot_fallback) {
      reason <- if (is_multiphase) {
        "file has 3+ real light-regime phases"
      } else {
        "the detected split day doesn't leave both an LD and a DD period with data"
      }
      flags <- c(flags, str_interp("${reason} - skipping combined_plot()'s single LD/DD peak split (doesn't apply here) and using rasterplot instead; activity_status/activity_proportion still recorded, peak_period_1/2 will be NA"))
    }
  }
  
  if (rasterplots_only) {
    spider_data <- data.frame(channel = numeric(), name = character(),
                              activity_status = character(), activity_proportion = numeric())
  } else {
    spider_data <- data.frame(channel = numeric(), name = character(),
                              peak_period_1 = numeric(), peak_period_2 = numeric(),
                              activity_status = character(), activity_proportion = numeric())
  }
  
  # Long-format peak periods for multi-phase files - one row per
  # channel per phase, since the number of phases varies file to file
  # and doesn't fit into fixed peak_period_1/2 columns.
  peak_rows <- data.frame(channel = numeric(), name = character(), segment = character(),
                          regime = character(), peak_period_hours = numeric())
  
  empty_channel_count <- 0
  
  for (sp_channel in 1:32) {
    spiderid <- paste0('s', sp_channel)
    
    name <- monitor_log %>% filter(channel == sp_channel) %>% pull(id)
    
    if (length(name) == 0) {
      message(str_interp("  Channel ${sp_channel}: EMPTY (no monitor_log entry)"))
      next
    }
    
    activity <- check_activity(data, spiderid)
    message(str_interp("  Channel ${sp_channel}: ${activity}"))
    
    if (activity == 'EMPTY') {
      empty_channel_count <- empty_channel_count + 1
      next
    }
    
    activity_proportion <- data %>%
      filter(get(spiderid) > 0) %>%
      nrow() / nrow(data)
    
    if (rasterplots_only) {
      plot <- tryCatch(
        rasterplot(data, spiderid, plot_title = str_interp('Activity for ${name}'), zt_0 = zt_0),
        error = function(e) {
          flags <<- c(flags, str_interp("rasterplot failed for ${spiderid}: ${conditionMessage(e)}"))
          NULL
        }
      )
      
      spider_data <- spider_data %>%
        add_row(channel = sp_channel, name = name,
                activity_status = activity, activity_proportion = activity_proportion)
      
      if (!is.null(plot)) {
        image_file <- file.path(out_dir, str_interp('${subfolder_name}_${spiderid}_raster.png'))
        ggsave(image_file, plot, width = 10, height = 6, units = 'in', create.dir = TRUE)
      }
      
    } else if (use_rasterplot_fallback) {
      raster_plot <- tryCatch(
        rasterplot(data, spiderid, plot_title = str_interp('Activity for ${name}'), zt_0 = zt_0),
        error = function(e) {
          flags <<- c(flags, str_interp("rasterplot failed for ${spiderid}: ${conditionMessage(e)}"))
          NULL
        }
      )
      
      # use_rasterplot_fallback: still record activity stats even though
      # there's no single peak_period split to compute for this file.
      spider_data <- spider_data %>%
        add_row(channel = sp_channel, name = name,
                peak_period_1 = NA, peak_period_2 = NA,
                activity_status = activity, activity_proportion = activity_proportion)
      
      # One periodogram per detected phase with enough data to analyze
      # (periodogram_eligible, checked once per phase above - the flag for
      # any skipped phases was already logged there, not repeated per
      # channel here). tryCatch stays as a safety net for eligible phases
      # that still fail for some other reason.
      perio_plots <- list()
      if (!is.null(seg_result) && nrow(seg_result$summary) > 0) {
        eligible_phases <- seg_result$summary %>% filter(periodogram_eligible)
        for (i in seq_len(nrow(eligible_phases))) {
          seg_row <- eligible_phases[i, ]
          perio <- tryCatch(
            lsp_plot(data, spiderid, str_interp('${seg_row$seg_label} Periodogram'),
                     time_start = seg_row$start_dt, time_end = seg_row$end_dt, return_peak = TRUE),
            error = function(e) {
              flags <<- c(flags, str_interp("periodogram failed for ${spiderid} (${seg_row$seg_label}): ${conditionMessage(e)}"))
              NULL
            }
          )
          if (!is.null(perio)) {
            peak_rows <- peak_rows %>%
              add_row(channel = sp_channel, name = name, segment = seg_row$seg_label,
                      regime = seg_row$regime, peak_period_hours = perio$peak)
            perio_plots[[length(perio_plots) + 1]] <- perio$plot
          }
        }
      }
      
      # Combine the raster and all successful periodograms into ONE image
      # - same layout idea as combined_plot() (periodograms stacked on the
      # left, raster spanning them on the right), generalized to however
      # many phases this file actually has instead of a fixed 2.
      if (!is.null(raster_plot)) {
        image_file <- file.path(out_dir, str_interp('${subfolder_name}_${spiderid}_combined.png'))
        if (length(perio_plots) > 0) {
          n <- length(perio_plots)
          combined_grob <- arrangeGrob(
            grobs = c(perio_plots, list(raster_plot)),
            layout_matrix = cbind(1:n, rep(n + 1, n))
          )
          ggsave(image_file, combined_grob, width = 10, height = max(6, 3 * n), units = 'in', create.dir = TRUE)
        } else {
          ggsave(image_file, raster_plot, width = 10, height = 6, units = 'in', create.dir = TRUE)
        }
      }
      
    } else {
      combined <- tryCatch(
        combined_plot(data, spiderid, 1, section2_start_day, return_peaks = TRUE,
                      zt_0 = zt_0, actogram_title = str_interp('Activity for ${name}')),
        error = function(e) {
          flags <<- c(flags, str_interp("combined_plot failed for ${spiderid}: ${conditionMessage(e)}"))
          NULL
        }
      )
      if (is.null(combined)) next
      
      spider_data <- spider_data %>%
        add_row(channel = sp_channel, name = name,
                peak_period_1 = combined$peak1, peak_period_2 = combined$peak2,
                activity_status = activity, activity_proportion = activity_proportion)
      
      image_file <- file.path(out_dir, str_interp('${subfolder_name}_${spiderid}_combined.png'))
      ggsave(image_file, combined$plot, width = 10, height = 6, units = 'in', create.dir = TRUE)
    }
  }
  
  if (empty_channel_count == 32) {
    flags <- c(flags, "ALL 32 channels came back EMPTY - worth double-checking this file")
  } else if (empty_channel_count > 20) {
    flags <- c(flags, str_interp("${empty_channel_count}/32 channels EMPTY - unusually high"))
  }
  
  data_file <- file.path(out_dir, 'data.csv')
  write.csv(spider_data, data_file, row.names = FALSE)
  
  if (nrow(peak_rows) > 0) {
    peaks_file <- file.path(out_dir, 'peaks.csv')
    write.csv(peak_rows, peaks_file, row.names = FALSE)
  }
  
  # "auto": full multi-phase segmentation - 2 or fewer phases keeps the
  # usual _LD.csv/_DD.csv pair; 3+ phases writes numbered files
  # (_LD1.csv, _DD1.csv, ...) plus _phases.csv. Numeric mode: unchanged
  # single-cutoff split.
  if (!rasterplots_only && 'light' %in% names(data)) {
    
    if (used_auto) {
      # seg_result already computed earlier (post dd_days trim) - reused
      # here rather than recomputed, so the plotting decision above and
      # the segment files written below always agree with each other.
      if (is.null(seg_result) || nrow(seg_result$summary) == 0) {
        flags <- c(flags, "multi-phase segmentation failed (missing light/datetime) - LD/DD split skipped")
      }
      
      if (!is.null(seg_result) && nrow(seg_result$summary) > 0) {
        n_phases <- nrow(seg_result$summary)
        
        if (n_phases <= 2) {
          # Simple case - same two-file output as always.
          ld_seg_ids <- seg_result$summary$segment_id[seg_result$summary$regime == 'LD']
          dd_seg_ids <- seg_result$summary$segment_id[seg_result$summary$regime %in% c('DD', 'LL', 'unknown')]
          
          data_ld <- data[seg_result$segment_id %in% ld_seg_ids, ]
          data_dd <- data[seg_result$segment_id %in% dd_seg_ids, ]
          
          if (nrow(data_ld) == 0) flags <- c(flags, "LD section has 0 rows")
          if (nrow(data_dd) == 0) flags <- c(flags, "DD section has 0 rows")
          
          write.csv(data_ld, file.path(out_dir, str_interp('${subfolder_name}_LD.csv')), row.names = FALSE)
          write.csv(data_dd, file.path(out_dir, str_interp('${subfolder_name}_DD.csv')), row.names = FALSE)
          
        } else {
          # paste0() (not str_interp()) since str_interp() isn't vectorized in mutate().
          phase_note <- seg_result$summary %>%
            mutate(desc = paste0(regime, " (day ", start_day, "-", end_day, ")")) %>%
            pull(desc) %>%
            str_c(collapse = ", ")
          flags <- c(flags, str_interp("detected ${n_phases} light-regime phases: ${phase_note} - writing numbered segment files instead of a single LD/DD split"))
          
          for (i in seq_len(nrow(seg_result$summary))) {
            seg_row <- seg_result$summary[i, ]
            seg_data <- data[seg_result$segment_id == seg_row$segment_id, ]
            seg_file <- file.path(out_dir, str_interp('${subfolder_name}_${seg_row$seg_label}.csv'))
            write.csv(seg_data, seg_file, row.names = FALSE)
          }
          
          phases_file <- file.path(out_dir, str_interp('${subfolder_name}_phases.csv'))
          write.csv(seg_result$summary %>% select(-segment_id), phases_file, row.names = FALSE)
        }
      }
      
    } else {
      # Numeric section2_start_day - original single-cutoff behavior.
      anchor_result <- resolve_zt_anchor(data)
      zt_0_detected <- anchor_result$anchor
      if (anchor_result$used_fallback) {
        flags <- c(flags, "LD/DD split: no light-on found - anchoring split on the file's first row instead")
      }
      
      data <- data %>%
        mutate(
          diff = difftime(datetime, zt_0_detected, units = 'days'),
          zt_day = floor(diff) - floor(diff[1]) + 1
        )
      
      data_ld <- data %>% filter(zt_day < section2_start_day) %>% select(!c(diff, zt_day))
      data_dd <- data %>% filter(zt_day >= section2_start_day) %>% select(!c(diff, zt_day))
      
      if (nrow(data_ld) == 0) flags <- c(flags, "LD section has 0 rows - check section2_start_day")
      if (nrow(data_dd) == 0) flags <- c(flags, "DD section has 0 rows - check section2_start_day")
      
      write.csv(data_ld, file.path(out_dir, str_interp('${subfolder_name}_LD.csv')), row.names = FALSE)
      write.csv(data_dd, file.path(out_dir, str_interp('${subfolder_name}_DD.csv')), row.names = FALSE)
    }
  }
  
  list(subfolder_name = subfolder_name, flags = flags)
}


# ============================================================
# 5. CHECKPOINT DEDUP
# ============================================================

# ------------------------------------------------------------
# Extracts the monitor identifier from a filename - "Monitor1"/
# "Monitor 3" style, or a trailing "_1"/"_2" before .txt. Returns NA if
# neither pattern matches (caller treats files with an unknown monitor
# ID as ambiguous rather than guessing).
# ------------------------------------------------------------
extract_monitor_id <- function(filename) {
  base <- basename(filename)
  m <- str_match(base, regex('Monitor\\s*(\\d+)', ignore_case = TRUE))
  if (!is.na(m[1, 2])) return(m[1, 2])
  m2 <- str_match(base, '_(\\d+)\\.txt$')
  if (!is.na(m2[1, 2])) return(m2[1, 2])
  NA_character_
}

# ------------------------------------------------------------
# Some monitors get re-exported multiple times mid-recording - same
# start_datetime AND same monitor identifier, growing row count each
# time. This finds those groups and keeps only the file with the most
# rows (the most complete export) per monitor.
#
# Matching on start_datetime alone isn't enough - two DIFFERENT monitors
# can genuinely start recording at the same moment, and naively merging
# those would silently discard real data. Grouping requires BOTH the
# same start time AND the same monitor ID (extracted from the filename,
# e.g. "Monitor1"/"Monitor2", or a trailing "_1"/"_2") before treating
# files as duplicates. Files where the monitor ID can't be determined
# are kept as-is rather than guessed at, and flagged for manual review.
#
# Returns list(keep = character vector of files to process,
#               dropped = data.frame logging what was excluded and why)
# ------------------------------------------------------------
select_latest_per_monitor <- function(txt_files) {
  
  dropped <- data.frame(file = character(), superseded_by = character(), note = character())
  ambiguous <- data.frame(file = character(), note = character())
  
  if (length(txt_files) <= 1) {
    return(list(keep = txt_files, dropped = dropped, ambiguous = ambiguous))
  }
  
  file_info <- lapply(txt_files, function(f) {
    data <- tryCatch(process(f), error = function(e) NULL)
    if (is.null(data) || nrow(data) == 0 || !'datetime' %in% names(data)) {
      return(data.frame(file = f, start_dt = NA, n_rows = NA, monitor_id = NA))
    }
    data.frame(file = f, start_dt = as.character(min(data$datetime, na.rm = TRUE)),
               n_rows = nrow(data), monitor_id = extract_monitor_id(f))
  }) %>% bind_rows()
  
  # Files we couldn't read get kept as-is. Files we could read but
  # couldn't identify a monitor ID for also get kept, but logged as
  # ambiguous - dedup is skipped for these since it's not safe to
  # assume they're duplicates just because they share a start time.
  unreadable <- file_info %>% filter(is.na(start_dt))
  unknown_monitor <- file_info %>% filter(!is.na(start_dt) & is.na(monitor_id))
  readable <- file_info %>% filter(!is.na(start_dt) & !is.na(monitor_id))
  
  keep <- c(unreadable$file, unknown_monitor$file)
  
  if (nrow(unknown_monitor) > 0) {
    for (i in seq_len(nrow(unknown_monitor))) {
      ambiguous <- ambiguous %>%
        add_row(file = basename(unknown_monitor$file[i]),
                note = "could not determine a monitor ID from the filename (no 'MonitorN' or trailing '_N' pattern found) - kept without checking for duplicates, worth verifying manually if other files in this folder share its start time")
    }
  }
  
  if (nrow(readable) > 0) {
    readable <- readable %>% mutate(group_key = paste(start_dt, monitor_id, sep = '|'))
    for (grp_key in unique(readable$group_key)) {
      grp <- readable %>% filter(group_key == grp_key) %>% arrange(desc(n_rows))
      keep <- c(keep, grp$file[1])
      
      if (nrow(grp) > 1) {
        for (i in 2:nrow(grp)) {
          dropped <- dropped %>%
            add_row(file = basename(grp$file[i]),
                    superseded_by = basename(grp$file[1]),
                    note = str_interp("same start time and monitor ID '${grp$monitor_id[1]}' (${grp$start_dt[1]}), fewer rows (${grp$n_rows[i]} vs ${grp$n_rows[1]}) - looks like an earlier checkpoint export of the same recording"))
        }
      }
    }
  }
  
  list(keep = keep, dropped = dropped, ambiguous = ambiguous)
}


# ============================================================
# 6. BATCH RUNNERS & FAST PREVIEWS
# ============================================================

# ------------------------------------------------------------
# Run the pipeline over multiple LC_ folders, each containing
# one or more .txt monitor files
# ------------------------------------------------------------
run_batch_analysis <- function(base_dir,
                               group_folders,
                               section2_start_day = 7,
                               dd_days = NULL,
                               rasterplots_only = FALSE,
                               zt_0 = ymd_hm('01-1-1 8:00'),
                               output_root = './output',
                               dedupe_checkpoints = TRUE) {
  
  flag_log <- data.frame(group = character(), file = character(), flag = character())
  
  for (group_name in group_folders) {
    folder_path <- file.path(base_dir, group_name)
    
    if (!dir.exists(folder_path)) {
      message(str_interp("!! Folder not found, skipping: ${folder_path}"))
      flag_log <- flag_log %>% add_row(group = group_name, file = NA, flag = "folder not found - check spelling/path")
      next
    }
    
    txt_files <- list.files(folder_path, pattern = '\\.txt$', full.names = TRUE)
    
    if (length(txt_files) == 0) {
      message(str_interp("!! No .txt files found in ${folder_path}"))
      flag_log <- flag_log %>% add_row(group = group_name, file = NA, flag = "no .txt files found in folder")
      next
    }
    
    if (dedupe_checkpoints) {
      message(str_interp("Checking for duplicate/checkpoint exports in ${group_name}..."))
      dedupe_result <- select_latest_per_monitor(txt_files)
      txt_files <- dedupe_result$keep
      
      if (nrow(dedupe_result$dropped) > 0) {
        for (i in seq_len(nrow(dedupe_result$dropped))) {
          d <- dedupe_result$dropped[i, ]
          message(str_interp("  Skipping ${d$file} - superseded by ${d$superseded_by}"))
          flag_log <- flag_log %>%
            add_row(group = group_name, file = d$file,
                    flag = str_interp("SKIPPED (not processed): ${d$note}"))
        }
      }
      
      if (nrow(dedupe_result$ambiguous) > 0) {
        for (i in seq_len(nrow(dedupe_result$ambiguous))) {
          a <- dedupe_result$ambiguous[i, ]
          message(str_interp("  Keeping ${a$file} without dedup check - could not determine its monitor ID"))
          flag_log <- flag_log %>%
            add_row(group = group_name, file = a$file,
                    flag = str_interp("KEPT (dedup skipped): ${a$note}"))
        }
      }
    }
    
    for (f in txt_files) {
      message(str_interp("\n=== Processing ${group_name} / ${basename(f)} ==="))
      
      result <- tryCatch(
        process_monitor_file(f, group_name, section2_start_day, dd_days, rasterplots_only, zt_0, output_root),
        error = function(e) {
          message(str_interp("!! FATAL ERROR on ${f}: ${conditionMessage(e)}"))
          list(subfolder_name = basename(f),
               flags = c(str_interp("FATAL ERROR - file skipped entirely: ${conditionMessage(e)}")))
        }
      )
      
      if (length(result$flags) > 0) {
        for (fl in result$flags) {
          flag_log <- flag_log %>% add_row(group = group_name, file = basename(f), flag = fl)
        }
      }
    }
  }
  
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  write.csv(flag_log, file.path(output_root, 'flags_summary.csv'), row.names = FALSE)
  
  if (nrow(flag_log) > 0) {
    message(str_interp("\n${nrow(flag_log)} issue(s) flagged - see ${file.path(output_root, 'flags_summary.csv')}"))
  } else {
    message("\nNo issues flagged across all files.")
  }
  
  invisible(flag_log)
}

# ------------------------------------------------------------
# Quick check: how many days of data does each file span?
# Just reads timestamps - no plotting, so it's fast. Handy for
# sanity-checking section2_start_day before running the full batch.
# ------------------------------------------------------------
check_monitor_lengths <- function(base_dir, group_folders, output_root = './output') {
  
  summary_log <- data.frame(
    group = character(), file = character(),
    start_datetime = as.POSIXct(character()),
    end_datetime = as.POSIXct(character()),
    n_days = numeric(), n_rows = integer(), note = character()
  )
  
  for (group_name in group_folders) {
    folder_path <- file.path(base_dir, group_name)
    
    if (!dir.exists(folder_path)) {
      message(str_interp("!! Folder not found, skipping: ${folder_path}"))
      summary_log <- summary_log %>%
        add_row(group = group_name, file = NA, start_datetime = NA, end_datetime = NA,
                n_days = NA, n_rows = NA, note = "folder not found")
      next
    }
    
    txt_files <- list.files(folder_path, pattern = '\\.txt$', full.names = TRUE)
    
    for (f in txt_files) {
      data <- tryCatch(process(f), error = function(e) NULL)
      
      if (is.null(data) || nrow(data) == 0 || !'datetime' %in% names(data)) {
        summary_log <- summary_log %>%
          add_row(group = group_name, file = basename(f), start_datetime = NA, end_datetime = NA,
                  n_days = NA, n_rows = if (is.null(data)) NA else nrow(data),
                  note = "could not read datetime / empty file")
        next
      }
      
      start_dt <- min(data$datetime, na.rm = TRUE)
      end_dt <- max(data$datetime, na.rm = TRUE)
      n_days <- round(as.numeric(difftime(end_dt, start_dt, units = 'days')), 2)
      
      summary_log <- summary_log %>%
        add_row(group = group_name, file = basename(f), start_datetime = start_dt, end_datetime = end_dt,
                n_days = n_days, n_rows = nrow(data), note = "")
    }
  }
  
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  write.csv(summary_log, file.path(output_root, 'monitor_lengths.csv'), row.names = FALSE)
  message(str_interp("\nSaved to ${file.path(output_root, 'monitor_lengths.csv')}"))
  
  summary_log
}

# ------------------------------------------------------------
# Fast (no-plot) preview of what "auto" would pick as the DD start day
# for every file. Run before the full batch to sanity-check detection.
# ------------------------------------------------------------
check_dd_start_days <- function(base_dir, group_folders, output_root = './output',
                                zt_0 = ymd_hm('01-1-1 8:00')) {
  
  summary_log <- data.frame(
    group = character(), file = character(),
    detected_dd_start_day = numeric(), note = character()
  )
  
  for (group_name in group_folders) {
    folder_path <- file.path(base_dir, group_name)
    
    if (!dir.exists(folder_path)) {
      message(str_interp("!! Folder not found, skipping: ${folder_path}"))
      summary_log <- summary_log %>%
        add_row(group = group_name, file = NA, detected_dd_start_day = NA, note = "folder not found")
      next
    }
    
    txt_files <- list.files(folder_path, pattern = '\\.txt$', full.names = TRUE)
    
    for (f in txt_files) {
      data <- tryCatch(process(f), error = function(e) NULL)
      
      if (is.null(data) || nrow(data) == 0) {
        summary_log <- summary_log %>%
          add_row(group = group_name, file = basename(f), detected_dd_start_day = NA,
                  note = "could not read file / empty file")
        next
      }
      
      detected <- detect_dd_start_day(data, zt_0)
      
      summary_log <- summary_log %>%
        add_row(group = group_name, file = basename(f),
                detected_dd_start_day = detected$day, note = detected$note)
    }
  }
  
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  write.csv(summary_log, file.path(output_root, 'dd_start_days.csv'), row.names = FALSE)
  message(str_interp("\nSaved to ${file.path(output_root, 'dd_start_days.csv')}"))
  
  summary_log
}

# ------------------------------------------------------------
# Fast (no-plot) preview of every detected light-regime phase per file -
# useful for multi-phase/re-entrainment protocols before the full run.
# ------------------------------------------------------------
check_light_segments <- function(base_dir, group_folders, output_root = './output',
                                 zt_0 = ymd_hm('01-1-1 8:00')) {
  
  summary_log <- data.frame(
    group = character(), file = character(), segment_id = integer(),
    regime = character(), start_day = numeric(), end_day = numeric(),
    n_rows = integer(), note = character()
  )
  
  for (group_name in group_folders) {
    folder_path <- file.path(base_dir, group_name)
    
    if (!dir.exists(folder_path)) {
      message(str_interp("!! Folder not found, skipping: ${folder_path}"))
      summary_log <- summary_log %>%
        add_row(group = group_name, file = NA, segment_id = NA, regime = NA,
                start_day = NA, end_day = NA, n_rows = NA, note = "folder not found")
      next
    }
    
    txt_files <- list.files(folder_path, pattern = '\\.txt$', full.names = TRUE)
    
    for (f in txt_files) {
      data <- tryCatch(process(f), error = function(e) NULL)
      
      if (is.null(data) || nrow(data) == 0) {
        summary_log <- summary_log %>%
          add_row(group = group_name, file = basename(f), segment_id = NA, regime = NA,
                  start_day = NA, end_day = NA, n_rows = NA, note = "could not read file / empty file")
        next
      }
      
      seg_result <- detect_light_segments(data, zt_0)
      
      if (is.null(seg_result) || nrow(seg_result$summary) == 0) {
        summary_log <- summary_log %>%
          add_row(group = group_name, file = basename(f), segment_id = NA, regime = NA,
                  start_day = NA, end_day = NA, n_rows = NA, note = "light/datetime column missing - could not segment")
        next
      }
      
      summary_log <- summary_log %>%
        add_row(group = group_name, file = basename(f),
                segment_id = seg_result$summary$segment_id,
                regime = seg_result$summary$regime,
                start_day = seg_result$summary$start_day,
                end_day = seg_result$summary$end_day,
                n_rows = seg_result$summary$n_rows,
                note = "")
    }
  }
  
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  write.csv(summary_log, file.path(output_root, 'light_segments.csv'), row.names = FALSE)
  message(str_interp("\nSaved to ${file.path(output_root, 'light_segments.csv')}"))
  
  summary_log
}

# ------------------------------------------------------------
# Fast (no-plot) preview of the modal LD lights-on clock time for every
# file - run to sanity-check the light schedule, or to fill in the
# lights-on time for files where it isn't otherwise known.
# ------------------------------------------------------------
check_lights_on <- function(base_dir, group_folders, output_root = './output', tolerance_min = 2) {
  
  summary_log <- data.frame(
    group = character(), file = character(), inferred_lights_on = character(),
    n_days_supporting = integer(), n_total_transitions = integer(),
    weak_agreement = logical(), note = character()
  )
  
  for (group_name in group_folders) {
    folder_path <- file.path(base_dir, group_name)
    
    if (!dir.exists(folder_path)) {
      message(str_interp("!! Folder not found, skipping: ${folder_path}"))
      summary_log <- summary_log %>%
        add_row(group = group_name, file = NA, inferred_lights_on = NA, n_days_supporting = NA,
                n_total_transitions = NA, weak_agreement = NA, note = "folder not found")
      next
    }
    
    txt_files <- list.files(folder_path, pattern = '\\.txt$', full.names = TRUE)
    
    for (f in txt_files) {
      data <- tryCatch(process(f), error = function(e) NULL)
      
      if (is.null(data) || nrow(data) == 0) {
        summary_log <- summary_log %>%
          add_row(group = group_name, file = basename(f), inferred_lights_on = NA, n_days_supporting = NA,
                  n_total_transitions = NA, weak_agreement = NA, note = "could not read file / empty file")
        next
      }
      
      result <- derive_lights_on_from_data(data, tolerance_min)
      
      summary_log <- summaryView_log %>%
        add_row(group = group_name, file = basename(f),
                inferred_lights_on = result$inferred_lights_on,
                n_days_supporting = result$n_days_supporting,
                n_total_transitions = result$n_total_transitions,
                weak_agreement = result$weak_agreement,
                note = result$note)
    }
  }
  
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  write.csv(summary_log, file.path(output_root, 'lights_on_times.csv'), row.names = FALSE)
  message(str_interp("\nSaved to ${file.path(output_root, 'lights_on_times.csv')}"))
  
  summary_log
}