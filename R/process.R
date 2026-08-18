library(ggplot2)
library(lubridate)
library(dplyr)

process <- function(filename, header=FALSE, sep='\t') {
  spider_ids <- paste0("s", 1:32)
  new_cols <- c("index", "date", "time", "status", "extras", "monitor", "tube", "dtype", "_", "light", spider_ids)
  
  df <- read.csv(filename, header, sep) %>%
    `colnames<-`(new_cols) %>%
    na.omit() %>%
    mutate(datetime = dmy_hms(paste(date, time))) %>%
    select(datetime, status, light, all_of(spider_ids))
}

detectmissing <- function(data) {
  data %>% 
    mutate(
      diff = datetime %>% 
        lag() %>% 
        difftime(datetime, units='min')
    ) %>%
    filter(abs(diff) > 1) %>%
    pull(datetime)
}

lineplot <- function(data, spiderid) {
  ggplot(data=data, mapping=aes(x=datetime)) +
    geom_ribbon(aes(ymin=0, ymax=light), fill="#ffff66") +
    geom_ribbon(aes(ymin=0, ymax=(get(spiderid)>0)*1), stat="identity", fill="#101010")
}

# ------------------------------------------------------------
# CHANGED: rows are now real CALENDAR days (midnight to midnight),
# computed from the file's own first timestamp - NOT anchored to zt_0
# the way they used to be. The x-axis is real clock time, 0:00 fixed at
# the left through 24:00 at the right, regardless of what clock time
# zt_0/lights-on falls on. This trades off the old behavior (each row a
# clean 0-24 ZT-relative window, but x-axis position varying with zt_0)
# for: unbroken calendar-day rows, at the cost of a row's light/activity
# data potentially not starting exactly at the left edge if the
# recording's lights-on isn't at midnight.
#
# zt_0 is still accepted (for the sample_point ZT->clock-time conversion
# and for the "lights on HH:MM" caption on the x-axis), but no longer
# determines row boundaries or the per-tick ZT labels - see the earlier
# version of this function (or combined_plot(), unchanged) if the old
# zt_0-relative-day / ZT-per-tick behavior is needed instead.
# ------------------------------------------------------------
rasterplot <- function(data, spiderid, plot_title = NULL, zt_0 = NULL, start_dt = NULL, end_dt = NULL, sample_point = NULL) {
  if (is.null(start_dt)) {
    start_dt <- data$datetime
  }
  
  if (is.null(end_dt)) {
    end_dt <- data$datetime %>% last()
  }
  
  if (is.null(zt_0)) {
    light_changes <- data %>%
      filter(light - lag(light) == 1) %>%
      pull(datetime)
    
    if (light_changes %>% length() != 0) {
      zt_0 <- light_changes %>% first()
    } else {
      zt_0 <- 0
    }
  }
  
  if (is.null(plot_title)) {
    plot_title <- paste('Activity for', spiderid)
  }
  
  # Calendar-day anchor: midnight of this file's first recorded day.
  midnight <- floor_date(data$datetime[1], unit = "day")
  
  data <- data %>%
    mutate(
      day_idx = as.numeric(floor(difftime(datetime, midnight, units = 'days'))),
      clock_time = (as.numeric(difftime(datetime, midnight, units = 'hours'))) %% 24,
      color = case_when(data[,spiderid] > 0 ~ "active",
                        light == 1 ~ "light",
                        light == 0 ~ "none"
      )
    )
  
  y_breaks <- sort(unique(data$day_idx))
  y_labels <- format(midnight + days(y_breaks), "%b %d")
  
  # Anchor the tick grid to zt_0 itself, so the "ZT0" tick sits exactly at
  # the real lights-on clock time (rather than at the nearest fixed 3h
  # clock-grid tick, which for a schedule like 01:00 would land ZT0's
  # label on a tick that isn't actually when the lights come on). Other
  # ticks are still 3 real hours apart, same spacing as before - just
  # shifted so the grid starts exactly where the light cycle does.
  if (inherits(zt_0, "POSIXct")) {
    zt0_hour <- as.numeric(format(zt_0, "%H")) + as.numeric(format(zt_0, "%M")) / 60
    zt_ticks <- seq(0, 24, 3)                         # ZT0, ZT3, ..., ZT24
    clock_positions <- (zt0_hour + zt_ticks) %% 24     # where each ZT tick actually falls on the clock
    keep <- !duplicated(round(clock_positions, 6))     # ZT0 and ZT24 land on the same clock position - keep one
    clock_positions <- clock_positions[keep]
    zt_ticks <- zt_ticks[keep]
    ord <- order(clock_positions)
    x_breaks <- clock_positions[ord]
    zt_ticks <- zt_ticks[ord]
    x_labels <- paste0("ZT", zt_ticks, "\n",
                       sprintf("%02d:%02d", floor(x_breaks), round((x_breaks %% 1) * 60)))
  } else {
    x_breaks <- seq(0, 24, 3)
    x_labels <- sprintf("%02d:00", x_breaks)
  }
  
  # zt_0's clock time, also shown once as a caption for quick reference.
  lights_on_caption <- if (inherits(zt_0, "POSIXct")) {
    sprintf("lights on %02d:%02d", as.numeric(format(zt_0, "%H")), as.numeric(format(zt_0, "%M")))
  } else {
    NULL
  }
  
  sample_line_df <- NULL
  if (!is.null(sample_point) && !is.na(sample_point$zt) && inherits(zt_0, "POSIXct")) {
    # Convert the sample's ZT/CT offset (hours since zt_0) into an actual
    # clock-time x-position, since the axis is now real clock time rather
    # than ZT-relative.
    zt0_hour <- as.numeric(format(zt_0, "%H")) + as.numeric(format(zt_0, "%M")) / 60
    sample_clock <- (zt0_hour + sample_point$zt) %% 24
    sample_line_df <- tibble(
      day_idx = max(data$day_idx, na.rm = TRUE),
      clock_time = sample_clock,
      label = str_interp("${sample_point$label_type}${sample_point$zt}"),
      label_hjust = if_else(sample_clock >= 20, 1, 0),
      label_x = if_else(sample_clock >= 20, sample_clock - 0.5, sample_clock + 0.5)
    )
  }
  
  missing <- detectmissing(data)
  
  missing_df <- data %>%
    transmute(datetime,
              clock_time,
              day_idx,
              start_time = lag(clock_time),
              start_day = lag(day_idx)
    ) %>%
    filter(datetime %in% missing)
  
  df_1 <- missing_df %>%
    mutate(clock_time = if_else(day_idx == start_day, clock_time, 24),
           day_idx = start_day)
  
  missing_df <- missing_df %>%
    filter(day_idx != start_day) %>%
    mutate(start_time = 0) %>%
    bind_rows(df_1)
  
  ggplot(data, aes(x = clock_time + 1/120, width = 1/60, y = day_idx, height = .8)) +
    geom_tile(mapping = aes(fill = color)) +
    scale_y_reverse(breaks = y_breaks, labels = y_labels) +
    scale_x_continuous(breaks = x_breaks, labels = x_labels, expand = expansion(mult = c(0.04, 0.02))) +
    scale_fill_manual(values = c(none = "#00000000", light = "#ffff66", active = "#000000", missing = '#ff0000')) +
    geom_rect(data = missing_df, mapping = aes(xmin = start_time, xmax = clock_time, ymin = day_idx - .4, ymax = day_idx + .4, fill = "missing")) +
    geom_rect(mapping = aes(xmin = 0, xmax = 24, ymin = day_idx - .4, ymax = day_idx + .4, fill = "none"), color = "#000000") +
    { if (!is.null(sample_line_df))
      geom_segment(data = sample_line_df,
                   aes(x = clock_time, xend = clock_time,
                       y = max(data$day_idx) - .4,
                       yend = max(data$day_idx) + .4),
                   color = "red", linewidth = 1, linetype = "solid", inherit.aes = FALSE)
    } +
    { if (!is.null(sample_line_df))
      geom_text(data = sample_line_df,
                aes(x = label_x, y = day_idx, label = label, hjust = label_hjust),
                color = "red", size = 3, fontface = "bold", inherit.aes = FALSE)
    } +
    theme(panel.background = element_rect(fill="#ffffff"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(color = "black"),
          axis.ticks.x = element_line(color = "black")) +
    ggtitle(plot_title) +
    labs(
      x = if (!is.null(lights_on_caption)) paste0("Clock time      ", lights_on_caption) else "Clock time",
      y = "Date",
      color = "Legend"
    ) }

# ------------------------------------------------------------
# UNCHANGED: combined_plot() still uses its own internal zt_0-anchored
# zt_day for splitting the file into Section 1 / Section 2 for the two
# periodograms - that's a data-windowing concern, separate from how the
# raster panel it embeds chooses to lay out its rows/x-axis (which now
# follows rasterplot()'s new calendar-day/clock-time behavior above,
# since combined_plot() just calls rasterplot() directly).
# ------------------------------------------------------------
combined_plot <- function(data, spiderid, start_zt_day, transition_zt_day, zt_0=NULL, actogram_title=NULL, return_peaks=FALSE) {
  if (is.null(zt_0)) {
    zt_0 <- data %>%
      filter(light - lag(light) == 1) %>%
      pull(datetime) %>%
      first()
  }
  data <- data %>%
    mutate(
      diff = difftime(datetime, zt_0, units='days'),
      zt_day = floor(diff) - floor(diff[1]) + 1
    )
  
  start_dt = data %>%
    filter(zt_day == start_zt_day) %>%
    pull(datetime) %>%
    first()
  
  transition_dt = data %>%
    filter(zt_day == transition_zt_day) %>%
    pull(datetime) %>%
    first()
  
  section1 <- lsp_plot(data, spiderid, 'Section 1 Periodogram', start_dt, transition_dt, return_peak = return_peaks)
  section2 <- lsp_plot(data, spiderid, 'Section 2 Periodogram', transition_dt + days(1), return_peak = return_peaks)
  
  rplot <- rasterplot(data, spiderid, plot_title=actogram_title, zt_0=zt_0)
  combined_plot <- arrangeGrob(
    section1$plot, section2$plot, rplot,
    layout_matrix = rbind(c(1, 3),
                          c(2, 3)))
  
  if (return_peaks) {
    list(plot=combined_plot, peak1=section1$peak, peak2=section2$peak)
  }
  else {
    combined_plot
  }
}