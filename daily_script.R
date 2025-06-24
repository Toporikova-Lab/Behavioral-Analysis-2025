library(dplyr)
library(stringr)

source('R/process.R')
source('R/death_detection.R')

filename <- "C:/Users/tayoub-winder/Documents/Behavioral-Analysis-2025/Data/Raw Monitor Data/LC_2025-06-09_1"

subfolder_name <- filename %>% 
  str_split_1('/') %>% 
  last() %>%
  str_remove('.txt')

name_ref <- filename %>%
  str_split_1('/') %>%
  head(-1) %>%
  append('name_ref.csv') %>%
  str_c(collapse='/') %>%
  read.csv()

data <- process(filename)

ids <- paste0('s', 1:32)

for (spiderid in ids) {
  name <- name_ref %>%
    filter(id == spiderid) %>%
    pull(name)
  
  if (length(name) == 0) {
    print(paste(spiderid, ': Tube Empty, Skipping'))
  }
  else {
    is_active <- check_activity(data, spiderid)
    if (!is.na(is_active) & is_active) {
      print(paste(spiderid, ': Active'))
    }
    else {
      print(paste(spiderid, ': Inactive'))
    }
    generated_plot <- rasterplot(data, spiderid, paste('Activity for', name))
    
    image_file = sprintf('./output/generated_plots/%s/%s_%s_raster.png', subfolder_name, subfolder_name, spiderid)
    ggsave(image_file, width=7, height=4, units='in', create.dir = TRUE)
  }
}