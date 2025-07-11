library(ggplot2)
library(deSolve)
library(lomb)

state <- c(x=0,
           y=0,
           z=0)

f <- function(x) {
  1/(1+x^9)
}

g <- function(x) {
  if(x <= 1) {1}
  else {0}
}

model <- function(time, state, params) {
  with(as.list(c(state, params)), {
    dx <- f(z) - a*x
    dy <- x - b*y
    dz <- y - b*z
    
    list(c(dx, dy, dz))
  })
}

time <- seq(0, 200, by=.01)

vals <- seq(.025, .5, by=.025)

# periods <- vals %>% sapply(function(val) {
#  params <- list(a=val,
#                 b=val,
#                 c=val)
#  
#  solution <- ode(state, time, model, params)
#
#  result <- solution %>%
#    data.frame() %>%
#    filter(time >= 100) %>%
#    select(time, x) %>%
#    lsp(from=10, to=50, type='period', ofac=20, plot=FALSE)
#  
#  print(val)
#  
#  result$peak.at[1]
#})

lower_bound <- .1
upper_bound <- .5

lower_period <- 10
upper_period <- 50
period <- 0

target=23

while (abs(period - target) > .05) {
  val <- (upper_bound + lower_bound) / 2
  params <- list(a=val,
                 b=val,
                 c=val)
  result <- ode(state, time, model, params) %>%
    data.frame() %>%
    filter(time >= 100) %>%
    select(time, x) %>%
    lsp(from=lower_period, to=upper_period, type='period', ofac=40, plot=FALSE)
  
  period <- result$peak.at[1]
  
  if (period > target) {
    lower_bound <- val
    upper_period <- period
  }
  else {
    upper_bound <- val
    lower_period <- period
  }
  
  print(paste(val, period))
}
