library(ggplot2)

process <- function(filename, header=FALSE, sep='\t') {
  df <- read.csv(filename, header, sep)
  
  df <- na.omit(df)
  
  spider_ids <- paste0("s", 1:32)
  
  datetime_format <- "%d %b %y %H:%M:%S"
  
  colnames(df) <- c("index", "date", "time", "status", "extras", "monitor", "tube", "dtype", "_", "light", spider_ids)
  
  df$datetime <- as.POSIXct(paste(df$date, df$time), format=datetime_format)
  
  new_columns <- c("datetime", "light", spider_ids)
  
  return(df[new_columns])
}

lineplot <- function(data, spiderid) {
  ggplot(data=data, mapping=aes(x=datetime)) +
    geom_ribbon(aes(ymin=0, ymax=light), fill="#ffff66") +
    geom_ribbon(aes(ymin=0, ymax=(get(spiderid)>0)*1), stat="identity", fill="#101010")
}

rasterplot <- function(data, spiderid, zt_0 = NULL, start_dt = NULL, end_dt = NULL) {
  datetime <- data$datetime
  
  if (is.null(start_dt)) {
    start_dt <- datetime[1]
  }
  if (is.null(end_dt)) {
    end_dt <- tail(datetime, 1)
  }
  
  if (is.null(zt_0)) {
    zt_0 <- data$datetime[diff(data$light, 1) == 1][1] # first time light turns on
  }
  else {
    zt_0 <- as.POSIXct(zt_0, format='%H:%M')
  }
  
  diff <- difftime(datetime, zt_0, units='days')
  
  data$zt_day <- floor(diff) - floor(diff[1]) + 1
  
  data$zt_time <- (as.numeric(diff) %% 1) * 24
  
  y_breaks <- unique(data$zt_day)
  x_breaks <- seq(0, 24, 3)
  
  ggplot(data, aes(x = zt_time, y = zt_day)) +
    geom_tile(aes(fill=light), width=1/60, height=.8) +
    scale_y_reverse(breaks=y_breaks) +
    scale_x_continuous(breaks=x_breaks) +
    scale_fill_gradientn(colours=c("#ffffff00", "#ffff66")) +
    annotate(geom="tile",
             x=data$zt_time,
             y=data$zt_day,
             width=1/60,
             height=.8,
             fill=scales::colour_ramp(c("#ffffff00", "#000000"))(data[,spiderid] > 0)
    ) +
    theme(legend.position = "none") +
    ggtitle(paste('Activity for ', spiderid)) +
    xlab('ZT (hrs)') +
    ylab('Day')
}