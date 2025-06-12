library(ggplot2)

process <- function(filename, header=FALSE, sep="\t") {
  df <- read.csv(filename, header=header, sep=sep, stringsAsFactors=FALSE)
  
  spider_ids <- paste0("s", 1:32)
  colnames(df) <- c("index", "date", "time", "status", "extras", "monitor", "tube", "dtype", "_", "light", spider_ids)
  
  datetime_format <- "%d %b %y %H:%M:%S"
  df$datetime <- as.POSIXct(paste(df$date, df$time), format=datetime_format)
  
  for (spider in spider_ids) {
    df[[spider]] <- as.integer(df[[spider]] > 0)
  }
  
  return(df[, c("datetime", "light", spider_ids)])
}

lineplot <- function(data, spiderid) {
  maxval <- 1  
  
  ggplot(data, aes(x=datetime)) +
    geom_ribbon(aes(ymin=0, ymax=light * maxval), fill="#000000", alpha=0.5) +
    geom_bar(aes(y = get(spiderid) * maxval), stat="identity", width=0.01, fill="black") +
    labs(title = paste("Binarized Activity Plot for", spiderid),
         x = "Time",
         y = "Activity (binarized)") +
    theme_minimal()
}

df <- process("Monitor2.txt", header=FALSE, sep="\t")
lineplot(df, "s1")

head(df)
