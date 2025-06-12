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

data <- process("Monitor2.txt")
library(lomb)

spiderid <- "s1"
activity <- data[[spiderid]]
time <- as.numeric(data$datetime) - as.numeric(data$datetime[1])  # Time in seconds from start

result <- lsp(x = activity, times = time, from = NULL, to = NULL, type = "period", ofac = 10, plot = TRUE)
library(ggplot2)
df_lsp <- data.frame(period = result$scanned, power = result$power)
ggplot(df_lsp, aes(x = period, y = power)) +
  geom_line() +
  labs(x = "Period", y = "Activity", title = paste("Lomb-Scargle Periodogram for", spiderid))

