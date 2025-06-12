detect_light_periods <- function(data) {
  
  
  light_col <- data[[10]]
  
  light_status <- ifelse(light_col==1,"Light", "Dark")
  
  data$LightStatus <- light_status
  
  transitions <- which(diff(light_col)!=0)
  transition_points <- data[transitions + 1,]
  
  list(
    full_data = data,
    transitions = transition_points
  )
}
