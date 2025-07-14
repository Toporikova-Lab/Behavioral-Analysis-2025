library(ggplot2)
library(deSolve)
library(lomb)

f <- function(x) {
  1 / (1 + x^9)
}

derivatives <- function(x, y, z, c) {
  dx <- f(z) - c*x
  dy <- x - c*y
  dz <- y - c*z
  
  c(dx, dy, dz)
}

# state: (x, y, z)
# params: (c,)
model_single <- function(time, state, params) {
  with(as.list(c(state, params)), {
    d_state <- derivatives(x, y, z, c)
    
    list(d_state)
  })
}

# state: (x1, y1, z1, x2, y2, z2),
# params: (c1, c2, k1, k2)
model_coupled <- function(time, state, params) {
  with(as.list(c(state, params)), {
    d1 <- derivatives(x1, y1, z1, c1)
    d2 <- derivatives(x2, y2, z2, c2)
    
    d1[1] <- d1[1] + k1*x2
    d2[1] <- d2[1] + k2*x1
    
    list(c(d1, d2))
  })
}

# df columns: (time, x)
solution_period <- function(df) {
  df %>%
    mutate(dx = x %>% diff() %>% c(NA) / dt) %>%
    na.omit() %>% 
    filter(dx >= 0 & lag(dx) < 0) %>% 
    pull(time) %>% 
    diff() %>%
    last()
}

main <- function(k1) {
  dt <- .01
  
  time <- seq(0, 24*20, by=dt)
  
  state <- c(x1=.02,
             y1=.2,
             z1=2,
             x2=.02,
             y2=.2,
             z2=2)
  
  params <- c(c1=.1727, # 22 hr period
              c2=.1458, # 26 hr period
              k1=k1, # coupling constants
              k2=.05-k1)
  
  solution <- ode(state, time, model_coupled, params) %>% data.frame()
  
  print(solution %>% mutate(x=x1) %>% solution_period())
  print(solution %>% mutate(x=x2) %>% solution_period())

  ggplot(solution, aes(x=time)) +
    geom_line(aes(y=x1), color='skyblue') +
    geom_line(aes(y=x2), color='orange') + 
    geom_hline(yintercept=0)
}