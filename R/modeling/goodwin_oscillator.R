library(ggplot2)
library(deSolve)
library(dplyr)

f <- function(x) {
  1 / (1 + x^9)
}

# state: (x, y, z)
# params: (c,)
model_single <- function(time, state, params) {
  with(as.list(c(state, params)), {
    dx <- f(z) - c*x
    dy <- x - c*y
    dz <- y - c*z
    
    list(c(dx, dy, dz))
  })
}

# state: (x1, y1, z1, x2, y2, z2),
# params: (c1, c2, k1, k2)
model_coupled <- function(time, state, params) {
  with(as.list(c(state, params)), {
    dx1 = f(z1 + k1*z2) - c1*x1
    dy1 = x1 - c1*y1
    dz1 = y1 - c1*z1
    
    dx2 = f(z2 + k2*z1) - c2*x2
    dy2 = x2 - c2*y2
    dz2 = y2 - c2*z2
    
    list(c(dx1, dy1, dz1, dx2, dy2, dz2))
  })
}

# df columns: (time, x)
solution_period <- function(df) {
  df %>%
    mutate(dx = x %>% diff() %>% c(NA) / diff(time) %>% c(NA)) %>%
    na.omit() %>% 
    filter(dx >= 0 & lag(dx) < 0) %>% 
    pull(time) %>% 
    diff() %>%
    last()
}

find_coupled_periods <- function(A, B) {
  dt <- .05
  
  time <- seq(0, 24*20, by=dt)
  
  state <- c(x1=.02,
             y1=.2,
             z1=2,
             x2=.02,
             y2=.2,
             z2=2)
  
  params <- c(c1=.1727,
              c2=.1458,
              k1=A,
              k2=B)
  
  solution <- ode(state, time, model_coupled, params) %>% data.frame()
  
  print(paste(A, B))
  
  c(solution %>% mutate(x=x1) %>% solution_period(),
    solution %>% mutate(x=x2) %>% solution_period())
}

periods_from_ks <- function() {
  kvals <- seq(-.15, .2, by=.005)
  
  df <- expand.grid(k1=kvals, k2=kvals) %>%
    data.frame() %>%
    rowwise() %>%
    mutate(periods = list(find_coupled_periods(k1, k2))) %>%
    transmute(k1, k2, period1 = periods[1], period2 = periods[2])
  
  df
}

main <- function(k1, k2) {
  dt <- .01
  
  time <- seq(0, 24*10, by=dt)
  
  state <- c(x1=.02,
             y1=.2,
             z1=2,
             x2=.02,
             y2=.2,
             z2=2)
  
  params <- c(c1=.1727, # 22 hr period
              c2=.1458, # 26 hr period
              k1=k1, # coupling constants
              k2=k2)
  
  solution <- ode(state, time, model_coupled, params) %>% data.frame()
  
  print(solution %>% mutate(x=x1) %>% solution_period())
  print(solution %>% mutate(x=x2) %>% solution_period())

  ggplot(solution, aes(x=time)) +
    geom_line(aes(y=x1), color='skyblue') +
    geom_line(aes(y=x2), color='orange') + 
    geom_hline(yintercept=0)
}