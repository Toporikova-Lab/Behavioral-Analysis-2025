library(ggplot2)
library(cowplot)

process <- function(filename, header=FALSE, sep='\t') {
  df <- read.csv(filename, header, sep)
  
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

rasterplot <- function(data, spiderid, start_dt = NULL) {
  if (is.null(start_dt)) {
    start_dt <- data$datetime[1]
  }
  
  end_dt <- tail(data$datetime, 1)
  
  dts <- seq(start_dt, end_dt, by="day")
  
  print(dts)
  
  plots <- list()
  
  for (dt in dts) {
    data_day <- data[dt < data$datetime & data$datetime < dt + 24*60*60,]
    
    if (dt == tail(dts, 1)) {
      plot <- lineplot(data_day, spiderid) + xlim(dt, dt + 24*60*60)
    }
    else {
      plot <- lineplot(data_day, spiderid) +
        xlab('') +
        ylab('') +
        theme(axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    }
    
    plots <- append(plots, list(plot))
  }
  
  plot_grid(plotlist=plots, nrow=length(plots), ncol=1)
}

