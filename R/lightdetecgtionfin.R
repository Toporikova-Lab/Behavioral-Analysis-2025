library(dplyr)
library(lubridate)
library(ggplot2)
#Process Function
process <- function(filename, header = FALSE, sep = '\t') {
  df <- read.csv(filename, header = header, sep = sep)
  
  spider_ids <- paste0("s", 1:32)
  
  datetime_format <- "%d %b %y %H:%M:%S"
  
  colnames(df) <- c("index", "date", "time", "status", "extras", "monitor", "tube", "dtype", "_", "light", spider_ids)
  
  df$datetime <- as.POSIXct(paste(df$date, df$time), format = datetime_format)
  
  new_columns <- c("datetime", "light", spider_ids)
  
  return(df[new_columns])
}

#light detection
df <- process("Monitor1.txt", header = FALSE, sep = '\t')

df$hour <- hour(df$datetime)
df$expected_light <- ifelse(df$hour >= 0 & df$hour <= 11, 1, 0)

df$light_status_correct <- df$light == df$expected_light
df$interruption <- ifelse(df$light_status_correct, "OK", "INTERRUPTED")

interruptions <- df %>% filter(interruption == "INTERRUPTED")
print(interruptions)

write.csv(df, "Light_Check_Results.csv", row.names = FALSE)
df <- process("Monitor1.txt", header = FALSE, sep = '\t')

df$hour <- hour(df$datetime)
df$expected_light <- ifelse(df$hour >= 0 & df$hour <= 11, 1, 0)

df$light_status_correct <- df$light == df$expected_light
df$interruption <- ifelse(df$light_status_correct, "OK", "INTERRUPTED")

interruptions <- df %>% filter(interruption == "INTERRUPTED")
print(interruptions)


write.csv(df, "Light_Check_Results.csv", row.names = FALSE)
df$light_OK <- ifelse(df$light_status_correct, 'LIGHT OK', 'LIGHT INTERRUPTED')

p <- ggplot(df, aes(x = datetime, y = light)) 
    geom_line(color = 'blue') 
  
 
df$light_OK <- ifelse(df$light_status_correct,'LIGHT OK','LIGHT INTERRUPTED')
lineplot <- function(data, spiderid) {
  ggplot(data=data, mapping=aes(x=datetime)) +
    geom_ribbon(aes(ymin=0, ymax=light), fill="#ffff66") +
    geom_ribbon(aes(ymin=0, ymax=(get(spiderid)>0)*1), stat="identity", fill="#101010")

geom_line(color='blue')
geom_text(
  data=interruptions,
  aes(y=0.5, label='light interrupted'),
  color='red',angle=90,hjust=0,vjust=0.5
  )
labs(title='Light Status Over Time',
x='time',
y='Light')
print(p)