check_activity <- function(data, spiderid, end_dt=NULL, cutoff_hrs=24) {
  if (is.null(end_dt)) {
    end_dt <- tail(data$datetime, 1)
  }
  
  data_active <- data[data[,spiderid] != 0 & data$datetime <= end_dt,]
  
  last_dt <- tail(data_active$datetime, 1)
  
  (end_dt - last_dt) < hours(24)
}

check_activity_all <- function(data, end_dt=NULL, cutoff_hrs=24) {
  ids <- paste0('s', 1:32)
  sapply(ids, function(id) {
    if (all(data[,id] == 0)) {
      return('EMPTY')
    }
    
    is_active <- check_activity(data, id)
    if (is_active) {
      return('ACTIVE')
    }
    return('INACTIVE')
  })
}