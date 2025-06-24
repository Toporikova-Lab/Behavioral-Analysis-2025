library(dplyr)
library(stringr)
library(lubridate)

extract_datetimes <- function(paths) {
  paths %>%
    str_remove('.mp4') %>%
    str_split('/') %>%
    lapply(function(v) {
      v %>% 
        tail(3) %>%
        str_c(collapse='/')
    }) %>%
    ymd_hm()
}

create_binary_df <- function(root_folder) {
  meta_path = paste(root_folder, 'Video metafile.csv', sep='/')
  
  times <- str_interp('${root_folder}/*/*/*mp4') %>%
    Sys.glob() %>%
    extract_datetimes()
  
  meta <- read.csv(meta_path)
  
  light_on <- meta$Light.on %>% hms()
  light_off <- meta$Light.off %>% hms()
  ld_start <- meta$LD.starts %>% ymd()
  ld_end <- meta$LD.ends %>% ymd()

  timeseq <- seq(min(times), max(times), by='min')
  
  data.frame(datetime=timeseq) %>%
    mutate(
      light_time = paste(hour(datetime), minute(datetime)) %>% 
        hm() %>% 
        between(light_on, light_off),
      light_day = datetime %>% 
        between(ld_start, ld_end),
      light = (light_time & light_day) %>% 
        as.numeric(),
      activity = datetime %in% times %>% 
        as.numeric()
    ) %>%
    select(datetime, light, activity)
}

activity_percentage <- function(df) {
  time_active <- df$activity %>% sum()
  time_total <- df %>% nrow()
  
  time_active / time_total
}