source('R/goodwin_oscillator.R')

get_period_from_c <- function(c) {
  state <- c(x=.02, y=.2, z=2)
  params <- c(c=c)
  
  dt <- .025
  
  time <- seq(0, 24*10, by=dt)
  
  ode(state, time, model_single, params) %>%
    data.frame() %>%
    select(time, x) %>%
    solution_period()
}

find_c <- function(period) {
  upper <- 1
  lower <- .05
  
  while (TRUE) {
    
    guess <- (upper + lower) / 2
    
    guess_period <- get_period_from_c(guess)
    
    if (abs(guess_period - period) < .01) {
      break
    }
    else if (guess_period > period) {
      lower <- guess
    }
    else {
      upper <- guess
    }
    
    print(paste(guess, guess_period))
  }
  
  guess
}

gvals <- data.frame(period = 10:40) %>%
  mutate(c = period %>% sapply(function(x) {find_c(x)}))