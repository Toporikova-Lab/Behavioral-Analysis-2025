source('Raster plot/process.R')
source('Raster plot/death_detection.R')

filename <- "Raw Monitor Data/test-data/Monitor2.txt"
lights_on_time <- "00:00"

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
      generated_plot <- rasterplot(data, id, zt_0=lights_on_time)
      
      image_file = paste('./generated_plots/', id, '_raster.png')
      ggsave(image_file, create.dir = TRUE)
    }
}