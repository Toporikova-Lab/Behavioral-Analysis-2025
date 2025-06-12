source('R/process.R')
source('R/death_detection.R')

filename <- "C:/Users/tayoub-winder/Documents/Behavioral-Analysis-2025/Data/Raw Monitor Data/2025-6-12/Monitor1.txt"

subfolder_name <- "LC_2025-06-09_06-12_1"

data <- process(filename)

spiderids = paste0('s', 1:32)

for (id in spiderids) {
    if (all(data[,id] == 0)) {
      print(paste(id, ': Tube Empty, Skipping'))
    }
    else {
      is_active <- check_activity(data, id)
      if (!is.na(is_active) & is_active) {
        print(paste(id, ': Active'))
      }
      else {
        print(paste(id, ': Inactive'))
      }
      generated_plot <- rasterplot(data, id)
      
      image_file = sprintf('./output/generated_plots/%s/%s_%s_raster.png', subfolder_name, subfolder_name, id)
      ggsave(image_file, width=7, height=4, units='in', create.dir = TRUE)
    }
}