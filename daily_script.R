library(dplyr)
library(stringr)

source('R/lsp_test.R')
source('R/death_detection.R')

filename <- "C:/Users/tayoub-winder/Documents/Behavioral-Analysis-2025/Data/Raw Monitor Data/test-data/Monitor2.txt"

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
      generated_plot <- combined_plot(data, spiderid, 2, 8)
      
      image_file = str_interp('./output/generated_plots/${subfolder_name}/${subfolder_name}_${spiderid}_combined.png')
      ggsave(image_file, generated_plot, width=10, height=6, units='in', create.dir = TRUE)
    }
  }
}