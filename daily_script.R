library(dplyr)
library(stringr)

source('R/lsp_test.R')
source('R/death_detection.R')

filename <- "C:/Users/tayoub-winder/Documents/Behavioral-Analysis-2025/Data/Raw Monitor Data/LC_2025-06-27_2/LC2025_06-27_06-30_2.txt"
section2_start_day = 3

rasterplots_only = TRUE

subfolder_name <- filename %>% 
  str_split_1('/') %>% 
  last() %>%
  str_remove('.txt')

monitor_log_file <- filename %>%
  str_split_1('/') %>%
  head(-1) %>%
  append('monitor_log.csv') %>%
  str_c(collapse='/')

if (file.exists(monitor_log_file)) {
  monitor_log <- read.csv(monitor_log_file)
} else {
  monitor_log <- data.frame(channel=1:32, id=paste0('s', 1:32))
}

data <- process(filename)

if (rasterplots_only) {
  spider_data <- data.frame(channel=numeric(), name=character(), activity_status=character(), activity_proportion=numeric())
} else {
  spider_data <- data.frame(channel=numeric(), name=character(), peak_period_1=numeric(), peak_period_2=numeric(), activity_status=character(), activity_proportion=numeric())
}

for (sp_channel in 1:32) {
  spiderid <- paste0('s', sp_channel)
  
  name <- monitor_log %>%
    filter(channel == sp_channel) %>%
    pull(id)
  
  if (length(name) == 0) {
    print(str_interp('Channel ${sp_channel}: EMPTY'))
  }
  else {
    activity <- check_activity(data, spiderid)
    
    print(str_interp('Channel ${sp_channel}: ${activity}'))
    
    if (activity != 'EMPTY') {
      activity_proportion = data %>%
        filter(get(spiderid) > 0) %>%
        nrow() /
        nrow(data)
      
      if (rasterplots_only) {
        plot <- rasterplot(data, spiderid, plot_title=str_interp('Activity for ${name}'))
        
        spider_data <- spider_data %>%
          add_row(
            channel = sp_channel,
            name = name,
            activity_status = activity,
            activity_proportion = activity_proportion
          )
        
        image_file = str_interp('./output/${subfolder_name}/${subfolder_name}_${spiderid}_raster.png')
        
        ggsave(image_file, plot, width=10, height=6, units='in', create.dir = TRUE)
      }
      else {
        combined <- combined_plot(data, spiderid, 1, section2_start_day, return_peaks=TRUE, actogram_title=str_interp('Activity for ${name}'))
        
        spider_data <- spider_data %>%
          add_row(
            channel = sp_channel,
            name = name,
            peak_period_1 = combined$peak1,
            peak_period_2 = combined$peak2,
            activity_status = activity,
            activity_proportion = activity_proportion
          )
        
        image_file = str_interp('./output/${subfolder_name}/${subfolder_name}_${spiderid}_combined.png')
        
        ggsave(image_file, combined$plot, width=10, height=6, units='in', create.dir = TRUE)
      }
    }
  }
}

data_file = str_interp('./output/${subfolder_name}/data.csv')
write.csv(spider_data, data_file, row.names=FALSE)  
