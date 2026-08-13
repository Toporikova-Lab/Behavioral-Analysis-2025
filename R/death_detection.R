library(dplyr)

check_activity <- function(data, spiderid, end_dt=NULL, cutoff_hrs=24) {
  if (is.null(end_dt)) {
    end_dt <- data$datetime %>% last()
  }
  
  active_dts <- data %>%
    filter(get(spiderid) != 0 & data$datetime <= end_dt) %>%
    pull(datetime)
  
  if (length(active_dts) == 0) {
    'EMPTY'
  }
  else if ((end_dt - active_dts %>% last()) < as.difftime('24:00:00')) {
    'ACTIVE'
  }
  else {
    'INACTIVE'
  }
  
}

check_activity_all <- function(data, end_dt=NULL, cutoff_hrs=24) {
  ids <- paste0('s', 1:32)
  sapply(ids, function(id) {
    check_activity(data, id, end_dt, cutoff_hrs)
  })
}