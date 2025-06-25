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

rasterplot <- function(data, spiderid, plot_title = NULL, zt_0 = NULL, start_dt = NULL, end_dt = NULL) {
  if (is.null(start_dt)) {
    start_dt <- data$datetime
  }
  
  if (is.null(end_dt)) {
    end_dt <- data$datetime %>% last()
  }
  
  if (is.null(zt_0)) {
    # get first time light turns on
    zt_0 <- data %>%
      filter(light - lag(light) == 1) %>%
      pull(datetime) %>%
      first()
  }
  else {
    zt_0 <- hm(zt_0)
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
  
  ggplot(data, aes(xmin=zt_time, xmax=zt_time + 1/60, ymin=zt_day - .4, ymax=zt_day + .4)) +
    geom_rect(mapping=aes(fill=color)) +
    scale_y_reverse(breaks=y_breaks) +
    scale_x_continuous(breaks=x_breaks) +
    scale_fill_manual(values=c(none= "#00000000", light="#ffff66", active="#000000", missing='#ff0000')) +
    geom_rect(data=missing_df, mapping=aes(xmin=start_time, xmax=zt_time, ymin=zt_day-.4, ymax=zt_day+.4, fill="missing")) +
    geom_rect(mapping=aes(xmin=0, xmax=24, ymin=zt_day - .4, ymax=zt_day + .4, fill="none"), color="#000000") +
    theme(panel.background = element_rect(fill="#ffffff"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    ggtitle(plot_title) +
    labs(
      x="ZT (hrs)",
      y="Day",
      color="Legend"
    )
}