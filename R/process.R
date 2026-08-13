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
  
  data <- data %>%
    mutate(
      diff = difftime(datetime, zt_0, units='days'),
      zt_day = floor(diff) - floor(diff[1]) + 1,
      zt_time = (as.numeric(diff) %% 1) * 24,
      color = case_when(data[,spiderid] > 0 ~ "active",
                        light == 1 ~ "light",
                        light == 0 ~ "none"
      )
    )
  
  y_breaks <- unique(data$zt_day)
  x_breaks <- seq(0, 24, 3)
  
  # Clock-time labels stacked under ZT ticks. Guards against zt_0 having
  # fallen back to a bare numeric 0 above (no light column) rather than
  # a real POSIXct - in that case just show ZT numbers.
  x_labels <- as.character(x_breaks)
  if (inherits(zt_0, "POSIXct")) {
    zt0_hour <- as.numeric(format(zt_0, "%H")) + as.numeric(format(zt_0, "%M")) / 60
    clock_at_zt <- function(zt) {
      ch <- (zt0_hour + zt) %% 24
      sprintf("%02d:%02d", floor(ch), round((ch %% 1) * 60))
    }
    x_labels <- paste0("ZT", x_breaks, "\n", sapply(x_breaks, clock_at_zt))
  }
  
  sample_line_df <- NULL
  if (!is.null(sample_point) && !is.na(sample_point$zt)) {
    sample_line_df <- tibble(
      zt_day = max(data$zt_day, na.rm = TRUE),
      zt_time = sample_point$zt,
      label = str_interp("${sample_point$label_type}${sample_point$zt}"),
      # Flip label direction near the right edge of the 0-24 axis so it
      # grows left instead of clipping off the plot.
      label_hjust = if_else(sample_point$zt >= 20, 1, 0),
      label_x = if_else(sample_point$zt >= 20, sample_point$zt - 0.5, sample_point$zt + 0.5)
    )
  }
  
  missing <- detectmissing(data)
  
  missing_df <- data %>%
    transmute(datetime,
              zt_time,
              zt_day,
              start_time=lag(zt_time),
              start_day=lag(zt_day)
    ) %>%
    filter(datetime %in% missing)
  
  df_1 <- missing_df %>%
    mutate(zt_time = if_else(zt_day == start_day, zt_time, 24),
           zt_day = start_day)
  
  missing_df <- missing_df %>%
    filter(zt_day != start_day) %>%
    mutate(start_time=0) %>%
    bind_rows(df_1)
  
  ggplot(data, aes(x=zt_time+1/120, width=1/60, y=zt_day, height=.8)) +
    geom_tile(mapping=aes(fill=color)) +
    scale_y_reverse(breaks=y_breaks) +
    scale_x_continuous(breaks=x_breaks, labels=x_labels) +
    scale_fill_manual(values=c(none= "#00000000", light="#ffff66", active="#000000", missing='#ff0000')) +
    geom_rect(data=missing_df, mapping=aes(xmin=start_time, xmax=zt_time, ymin=zt_day-.4, ymax=zt_day+.4, fill="missing")) +
    geom_rect(mapping=aes(xmin=0, xmax=24, ymin=zt_day - .4, ymax=zt_day + .4, fill="none"), color="#000000") +
    { if (!is.null(sample_line_df))
      geom_segment(data = sample_line_df,
                   aes(x = zt_time, xend = zt_time,
                       y = max(data$zt_day) - .4,
                       yend = max(data$zt_day) + .4),
                   color = "red", linewidth = 1, linetype = "solid", inherit.aes = FALSE)
    } +
    { if (!is.null(sample_line_df))
      geom_text(data = sample_line_df,
                aes(x = label_x, y = zt_day, label = label, hjust = label_hjust),
                color = "red", size = 3, fontface = "bold", inherit.aes = FALSE)
    } +
    theme(panel.background = element_rect(fill="#ffffff"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(color = "black"),
          axis.ticks.x = element_line(color = "black")) +
    ggtitle(plot_title) +
    labs(
      x="ZT (hrs) / Time of Day",
      y="Day",
      color="Legend"
    ) }