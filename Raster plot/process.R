library(ggplot2)

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
  ggplot(data=data, mapping=aes(x=datetime,y=(s1 > 0)*max(s1))) +
    geom_ribbon(aes(ymin=0, ymax=light*max(s1)), fill="#ffff66") +
    geom_bar(stat="identity")
}