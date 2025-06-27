library(dplyr)
library(stringr)

source('R/lsp_test.R')
source('R/death_detection.R')

filename <- "C:/Users/tayoub-winder/Documents/Behavioral-Analysis-2025/Data/Raw Monitor Data/LC_2025-06-09_1/LC_2025-06-09_06-24_1.txt"
section2_start_day = 7

subfolder_name <- filename %>% 
  str_split_1('/') %>% 
  last() %>%
  str_remove('.txt')

name_ref_file <- filename %>%
  str_split_1('/') %>%
  head(-1) %>%
  append('name_ref.csv') %>%
  str_c(collapse='/')

ids <- paste0('s', 1:32)

if (file.exists(name_ref_file)) {
  name_ref <- read.csv(name_ref_file)
} else {
  name_ref <- data.frame(id=ids, name=ids)
}

data <- process(filename)

spider_data = data.frame(id=character(), name=character(), peak_period_1=numeric(), peak_period_2=numeric(), activity_status=character(), activity_proportion=numeric())

for (spiderid in ids) {
  name <- name_ref %>%
    filter(id == spiderid) %>%
    pull(name)
  
  if (length(name) == 0) {
    print(str_interp('${spiderid}: EMPTY'))
  }
  else {
    activity <- check_activity(data, spiderid)
    
    print(str_interp('${spiderid}: ${activity}'))
    if (activity != 'EMPTY') {
      combined <- combined_plot(data, spiderid, 2, section2_start_day, return_peaks=TRUE)
      
      activity_proportion = data %>%
        filter(get(spiderid) > 0) %>%
        nrow() /
        nrow(data)
      
      spider_data <- spider_data %>%
        add_row(
          id = spiderid,
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
  
  data_file = str_interp('./output/${subfolder_name}/data.csv')
  write.csv(spider_data, data_file, row.names=FALSE)
}
